{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module MultiFold (pfoldValidatorW, pmintFoldPolicyW, pmintRewardFoldPolicyW, prewardFoldValidatorW) where

import Conversions (pconvert)
import Plutarch.Api.V1 (PCredential (..))
import Plutarch.Api.V1.Value (padaSymbol, padaToken, pforgetPositive, pnoAdaValue, pnormalize, pvalueOf)
import Plutarch.Api.V1.Value qualified as Value
import Plutarch.Api.V2 (
  PAddress (..),
  PCurrencySymbol,
  PMintingPolicy,
  POutputDatum (POutputDatum),
  PScriptContext,
  PScriptPurpose (PMinting, PSpending),
  PTokenName (..),
  PTxInInfo (..),
  PTxOut,
  PValidator,
 )
import Plutarch.DataRepr (
  PDataFields,
 )
import Plutarch.Extra.Interval (pbefore)
import Plutarch.Extra.ScriptContext (pfromPDatum, ptryFromInlineDatum)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Types.Classes
import Types.Constants (commitFoldTN, foldingFee, poriginNodeTN, rewardFoldTN, rewardTokenHolderTN)
import Types.StakingSet
import Utils (
  fetchConfigDetails,
  pand'List,
  pcountOfUniqueTokens,
  pcountScriptInputs,
  pelemAt',
  pfindCurrencySymbolsByTokenName,
  pfoldl2,
  phasCS,
  pheadSingleton,
  ptryLookupValue,
  ptryOutputToAddress,
  ptryOwnInput,
  ptryOwnOutput,
  ptxSignedByPkh,
  pvalueOfOneScott,
  (#/=), paysAtleastValueToAddress, pcountGivenScriptInputs,
 )
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
 )

data PFoldMintConfig (s :: S)
  = PFoldMintConfig
      ( Term
          s
          ( PDataRecord
              '[ "nodeCS" ':= PCurrencySymbol
               , "foldAddr" ':= PAddress
               , "configCS" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PFoldMintConfig where
  type DPTStrat _ = PlutusTypeData

data PFoldMintAct (s :: S)
  = PMintFold (Term s (PDataRecord '[]))
  | PBurnFold (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PFoldMintAct where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFoldMintAct

instance PTryFrom PData (PAsData PFoldMintAct)

data PFoldDatum (s :: S)
  = PFoldDatum
      ( Term
          s
          ( PDataRecord
              '[ "currNode" ':= PStakingSetNode
               , "staked" ':= PInteger
               , "owner" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PFoldDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFoldDatum

instance PTryFrom PData (PAsData PFoldDatum)

data PFoldAct (s :: S)
  = PFoldNodes
      ( Term
          s
          ( PDataRecord
              '["nodeIdxs" ':= PBuiltinList (PAsData PInteger)]
          )
      )
  | PReclaim (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PFoldAct where
  type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PFoldAct

data PCommitFoldState (s :: S) = PCommitFoldState
  { key :: Term s PNodeKeyState
  , next :: Term s PNodeKeyState
  , staked :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PCommitFoldState where
  type DPTStrat _ = PlutusTypeScott

pmintFoldPolicyW :: Term s (PFoldMintConfig :--> PMintingPolicy)
pmintFoldPolicyW = phoistAcyclic $
  plam $ \fconfig redm ctx ->
    let red = pconvert @PFoldMintAct redm
     in pmatch red $ \case
          PMintFold _ -> popaque $ pmintCommitFold # fconfig # ctx
          PBurnFold _ -> popaque $ pburnCommitFold # ctx

pburnCommitFold :: Term s (PScriptContext :--> PUnit)
pburnCommitFold = phoistAcyclic $
  plam $ \ctx -> unTermCont $ do
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    PMinting policy <- pmatchC ctxF.purpose
    ownPolicyId <- pletC $ pfield @"_0" # policy

    info <- pletFieldsC @'["mint"] ctxF.txInfo
    tkPairs <- pletC $ ptryLookupValue # ownPolicyId # (pnormalize # info.mint)
    tkPair <- pletC (pheadSingleton # tkPairs)
    let numMinted = psndBuiltin # tkPair
    pure $
      pif (pfromData numMinted #== -1) (pconstant ()) perror

pmintCommitFold :: Term s (PFoldMintConfig :--> PScriptContext :--> PUnit)
pmintCommitFold = phoistAcyclic $
  plam $ \fconfig ctx -> unTermCont $ do
    foldConfF <- pletFieldsC @'["nodeCS", "foldAddr", "configCS"] fconfig
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    info <- pletFieldsC @'["referenceInputs", "outputs", "mint", "validRange"] ctxF.txInfo

    PPair configTN config <- pmatchC $ fetchConfigDetails # foldConfF.configCS # info.referenceInputs

    PMinting policy <- pmatchC ctxF.purpose
    ownPolicyId <- pletC $ pfield @"_0" # policy

    tkPairs <- pletC $ ptryLookupValue # ownPolicyId # (pnormalize # info.mint)
    tkPair <- pletC (pheadSingleton # tkPairs)

    let numMinted = psndBuiltin # tkPair
        foldOutput = ptryOutputToAddress # info.outputs # foldConfF.foldAddr
        nodeRefInput =
          pfield @"resolved"
            #$ pheadSingleton
            # ( pfilter @PBuiltinList
                  # plam (\inp -> pvalueOf # (pfield @"value" # (pfield @"resolved" # inp)) # foldConfF.nodeCS # poriginNodeTN #== 1)
                  # info.referenceInputs
              )

    nodeRefInputF <- pletFieldsC @'["value", "datum"] nodeRefInput
    let refInpDat = pfromPDatum @PStakingSetNode #$ ptryFromInlineDatum # nodeRefInputF.datum

    foldOutputF <- pletFieldsC @'["value", "datum"] foldOutput
    (POutputDatum foldOutputDatum) <- pmatchC foldOutputF.datum

    let foldOutDatum = pfromPDatum @PFoldDatum # (pfield @"outputDatum" # foldOutputDatum)
    foldOutDatumF <- pletFieldsC @'["currNode", "staked"] foldOutDatum

    let foldInitChecks =
          pand'List
            [ pfromData numMinted #== 1
            , pvalueOf # foldOutputF.value # pfromData ownPolicyId # commitFoldTN #== pconstant 1
            , foldOutDatumF.currNode #== refInpDat
            , configTN #== pfield @"configTN" # refInpDat
            , pfromData foldOutDatumF.staked #== pconstant 0
            , pbefore # pfromData (pfield @"endStaking" # config) # info.validRange
            ]
    pure $
      pif
        foldInitChecks
        (pconstant ())
        perror

pfoldValidatorW :: Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PValidator)
pfoldValidatorW = phoistAcyclic $
  plam $ \configCS nodeCS datum redeemer ctx -> P.do
    ctxF <- pletFields @'["txInfo", "purpose"] ctx
    infoF <- pletFields @'["inputs", "signatories", "mint", "referenceInputs"] ctxF.txInfo

    PPair configTN config <- pmatch $ fetchConfigDetails # pfromData configCS # infoF.referenceInputs

    let dat = pconvert @PFoldDatum datum
        red = pconvert @PFoldAct redeemer
     in pmatch red $ \case
          PFoldNodes r -> pletFields @'["nodeIdxs"] r $ \redF ->
            pfoldNodes # config # configTN # pfromData nodeCS # redF.nodeIdxs # dat # ctx
          PReclaim _ -> unTermCont $ do
            datF <- pletFieldsC @'["owner", "currNode"] dat
            PPubKeyCredential ((pfield @"_0" #) -> ownerPkh) <- pmatchC (pfield @"credential" # datF.owner)

            PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose
            let ownInput = ptryOwnInput # infoF.inputs # ownRef
            ownInputF <- pletFieldsC @'["address", "value"] ownInput
            PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatchC (pfield @"credential" # ownInputF.address)

            let possibleCSs = pfindCurrencySymbolsByTokenName # ownInputF.value # commitFoldTN
                commitCS = pheadSingleton # possibleCSs
                commitPairs = ptryLookupValue # commitCS # infoF.mint
                commitPair = (pheadSingleton # commitPairs)
                commitMinted = psndBuiltin # commitPair
            pure $
              pif
                ( pand'List
                    [ ptxSignedByPkh # ownerPkh # infoF.signatories
                    , pfromData commitMinted #== -1
                    , pfield @"configTN" # datF.currNode #== configTN
                    , pcountGivenScriptInputs # ownValHash # 0 # infoF.inputs #== 1
                    ]
                )
                (popaque $ pconstant ())
                perror

pisSuccessor ::
  Term
    s
    ( PStakingConfig
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PCommitFoldState
        :--> PTxOut
        :--> PCommitFoldState
    )
pisSuccessor = plam $ \config configTN nodeCS accNode node -> unTermCont $ do
  configF <- pletFieldsC @'["stakeCS", "stakeTN"] config
  accNodeF <- pmatchC accNode
  nodeF <- pletFieldsC @'["value", "datum"] node
  nodeValue <- pletC nodeF.value
  let nodeDatum = pfromPDatum @PStakingSetNode #$ (ptryFromInlineDatum # nodeF.datum)
      hasNodeTk = pvalueOfOneScott # nodeCS # nodeValue
  currNodeF <- pletFieldsC @'["key", "next", "configTN"] nodeDatum

  let nodeKey = toScott $ pfromData currNodeF.key
      accState =
        pcon @PCommitFoldState
          accNodeF
            { next = toScott (pfromData currNodeF.next)
            , staked = accNodeF.staked + (pvalueOf # nodeValue # configF.stakeCS # configF.stakeTN)
            }
      finalChecks =
        pand'List
          [ ptraceIfFalse "Incorred Successor" $ accNodeF.next #== nodeKey
          , {- To prevent Repeated Fold Attack. Prevents folding again by checking that head node is not
               included in the fold again. CommitFoldDatum.currNode.key == PEmtpy -}
            ptraceIfFalse "Cannot Fold Head Node Again" $ accNodeF.key #/= nodeKey
          , ptraceIfFalse "Missing Node Tokne" $ hasNodeTk
          , ptraceIfFalse "Incorrect ConfigTN" $ currNodeF.configTN #== configTN
          ]

  pure $ pif finalChecks accState perror

pfoldNodes ::
  Term
    s
    ( PStakingConfig
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PBuiltinList (PAsData PInteger)
        :--> PFoldDatum
        :--> PScriptContext
        :--> POpaque
    )
pfoldNodes = phoistAcyclic $
  plam $ \config configTN nodeCS nodeIndices dat ctx -> unTermCont $ do
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    -- all reference inputs have node currency symbol
    -- all reference inputs are connected in the linked list
    info <- pletFieldsC @'["inputs", "referenceInputs", "outputs", "mint", "validRange"] ctxF.txInfo
    PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose

    let ownInput = ptryOwnInput # info.inputs # ownRef
    ownInputF <- pletFieldsC @'["address", "value"] ownInput
    PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatchC (pfield @"credential" # ownInputF.address)
    datF <- pletFieldsC @'["currNode", "staked", "owner"] dat
    currFoldNodeF <- pletFieldsC @'["key", "next", "configTN"] datF.currNode

    let ownOutput = ptryOwnOutput # info.outputs # ownValHash
    ownOutputF <- pletFieldsC @'["address", "value", "datum"] ownOutput

    let commitFoldState =
          pcon
            ( PCommitFoldState
                (toScott $ pfromData currFoldNodeF.key)
                (toScott $ pfromData currFoldNodeF.next)
                (pfromData datF.staked)
            )
        foldOutDatum = pfromPDatum @PFoldDatum #$ ptryFromInlineDatum # ownOutputF.datum
    newFoldDatumF <- pletFieldsC @'["currNode", "staked", "owner"] foldOutDatum
    newFoldNodeF <- pletFieldsC @'["key", "next", "configTN"] newFoldDatumF.currNode

    let refInputs :: Term _ (PBuiltinList PTxInInfo)
        refInputs = info.referenceInputs

    refIns <- pletC refInputs

    let nodeInputs :: Term _ (PBuiltinList PTxOut)
        nodeInputs = pmap @PBuiltinList # plam (\i -> pfield @"resolved" # (pelemAt' # pfromData i # refIns)) # nodeIndices
        newCommitFoldState' = pfoldl # (pisSuccessor # config # configTN # nodeCS) # commitFoldState # nodeInputs

    newCommitFoldState <- pmatchC newCommitFoldState'
    let foldChecks =
          pand'List
            [ pcountGivenScriptInputs # ownValHash # 0 # info.inputs #== 1
            , pfromData info.mint #== mempty
            , currFoldNodeF.configTN #== configTN
            , newFoldNodeF.configTN #== configTN
            , currFoldNodeF.key #== newFoldNodeF.key
            , newCommitFoldState.next #== (toScott $ pfromData newFoldNodeF.next)
            , pfromData newFoldDatumF.staked #== newCommitFoldState.staked
            , newFoldDatumF.owner #== datF.owner
            , pnoAdaValue # ownOutputF.value #== pnoAdaValue # ownInputF.value
            , pbefore # (pfromData $ pfield @"endStaking" # config) # info.validRange
            ]
    pure $
      pif foldChecks (popaque (pconstant ())) perror

data PRewardMintFoldConfig (s :: S)
  = PRewardMintFoldConfig
      ( Term
          s
          ( PDataRecord
              '[ "nodeCS" ':= PCurrencySymbol
               , "tokenHolderCS" ':= PCurrencySymbol
               , "rewardScriptAddr" ':= PAddress
               , "commitFoldCS" ':= PCurrencySymbol
               , "configCS" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PRewardMintFoldConfig where
  type DPTStrat _ = PlutusTypeData

data PRewardFoldDatum (s :: S)
  = PRewardFoldDatum
      ( Term
          s
          ( PDataRecord
              '[ "currNode" ':= PStakingSetNode
               , "totalRewardTokens" ':= PInteger
               , "totalStaked" ':= PInteger
               , "owner" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PRewardFoldDatum where
  type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PRewardFoldDatum

data PRewardsFoldAct (s :: S)
  = PRewardsFoldNodes
      ( Term
          s
          ( PDataRecord
              '[ "nodeIdxs" ':= PBuiltinList (PAsData PInteger)
               , "nodeOutIdxs" ':= PBuiltinList (PAsData PInteger)
               ]
          )
      )
  | PRewardsReclaim (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PRewardsFoldAct where
  type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PRewardsFoldAct

data PRewardFoldMintAct (s :: S)
  = PMintRewardFold (Term s (PDataRecord '[]))
  | PBurnRewardFold (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PRewardFoldMintAct where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PRewardFoldMintAct

instance PTryFrom PData (PAsData PRewardFoldMintAct)

pmintRewardFoldPolicyW :: Term s (PRewardMintFoldConfig :--> PMintingPolicy)
pmintRewardFoldPolicyW = phoistAcyclic $ plam $ \rewardConfig red' ctx ->
  let red = pconvert @PRewardFoldMintAct red'
   in pmatch red $ \case
        PMintRewardFold _ -> popaque $ pmintRewardFold # rewardConfig # ctx
        PBurnRewardFold _ -> popaque $ pburnRewardFold # ctx

pburnRewardFold :: Term s (PScriptContext :--> PUnit)
pburnRewardFold = phoistAcyclic $ plam $ \ctx -> unTermCont $ do
  ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
  PMinting policy <- pmatchC ctxF.purpose
  ownPolicyId <- pletC $ pfield @"_0" # policy

  info <- pletFieldsC @'["mint"] ctxF.txInfo
  tkPairs <- pletC $ ptryLookupValue # ownPolicyId # (pnormalize # info.mint)
  tkPair <- pletC (pheadSingleton # tkPairs)
  let numMinted = psndBuiltin # tkPair
  pure $
    pif (pfromData numMinted #== -1) (pconstant ()) perror

pmintRewardFold :: Term s (PRewardMintFoldConfig :--> PScriptContext :--> PUnit)
pmintRewardFold = phoistAcyclic $
  plam $ \rewardConfig ctx -> unTermCont $ do
    rewardConfigF <- pletFieldsC @'["nodeCS", "tokenHolderCS", "rewardScriptAddr", "commitFoldCS", "configCS"] rewardConfig
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    info <- pletFieldsC @'["inputs", "outputs", "mint", "referenceInputs"] ctxF.txInfo

    PPair configTN config <- pmatchC $ fetchConfigDetails # rewardConfigF.configCS # info.referenceInputs
    configF <- pletFieldsC @'["rewardCS", "rewardTN"] config

    PMinting policy <- pmatchC ctxF.purpose
    ownPolicyId <- pletC $ pfield @"_0" # policy

    mintedValue <- pletC $ (pnormalize # info.mint)
    tkPairs <- pletC $ ptryLookupValue # ownPolicyId # mintedValue
    tkPair <- pletC (pheadSingleton # tkPairs)

    let commitInp =
          pfield @"resolved"
            #$ pheadSingleton
            # ( pfilter @PBuiltinList
                  # plam (\inp -> pvalueOf # (pfield @"value" # (pfield @"resolved" # inp)) # rewardConfigF.commitFoldCS # commitFoldTN #== 1)
                  # info.inputs
              )
        projectInput =
          pfield @"resolved"
            #$ pheadSingleton
            # ( pfilter @PBuiltinList
                  # plam (\inp -> pvalueOf # (pfield @"value" # (pfield @"resolved" # inp)) # rewardConfigF.tokenHolderCS # rewardTokenHolderTN #== 1)
                  # info.inputs
              )
        {- 'pheadSingleton' ensures that only one node input is being spent (it throws an error if
            there is more than one element in the list). This node is confimed to be a head node by
            comparing its value to nodeOutput which has nodeCS.poriginNodeTN
        -}
        nodeInput =
          pfield @"resolved"
            #$ pheadSingleton
            # ( pfilter @PBuiltinList
                  # plam (\inp -> phasCS # (pfield @"value" # (pfield @"resolved" # inp)) # rewardConfigF.nodeCS)
                  # info.inputs
              )
        nodeOutput =
          pheadSingleton
            # ( pfilter @PBuiltinList
                  # plam (\out -> pvalueOf # (pfield @"value" # out) # rewardConfigF.nodeCS # poriginNodeTN #== 1)
                  # info.outputs
              )
        numMinted = psndBuiltin # tkPair
        foldOutput = ptryOutputToAddress # info.outputs # rewardConfigF.rewardScriptAddr

    commitInpF <- pletFieldsC @'["value", "datum"] commitInp
    (POutputDatum commitDatum) <- pmatchC commitInpF.datum
    let commitDat = pfromPDatum @PFoldDatum # (pfield @"outputDatum" # commitDatum)
    commitDatF <- pletFieldsC @'["currNode", "staked", "owner"] commitDat
    commitFoldNodeF <- pletFieldsC @'["key", "next"] commitDatF.currNode

    nodeInputF <- pletFieldsC @'["value", "datum", "address"] nodeInput
    (POutputDatum nodeInpDatum) <- pmatchC nodeInputF.datum
    let nodeInpDat = pfromPDatum @PStakingSetNode # (pfield @"outputDatum" # nodeInpDatum)

    nodeOutputF <- pletFieldsC @'["value", "datum", "address"] nodeOutput
    (POutputDatum nodeOutDatum) <- pmatchC nodeOutputF.datum
    let nodeOutDat = pfromPDatum @PStakingSetNode # (pfield @"outputDatum" # nodeOutDatum)

    foldOutputF <- pletFieldsC @'["value", "datum"] foldOutput
    (POutputDatum foldOutputDatum) <- pmatchC foldOutputF.datum

    let foldOutDatum = pfromPDatum @PRewardFoldDatum # (pfield @"outputDatum" # foldOutputDatum)
    foldOutDatumF <- pletFieldsC @'["currNode", "totalRewardTokens", "totalStaked"] foldOutDatum

    let foldingFeeVal = Value.psingleton # padaSymbol # padaToken # (-foldingFee)

    totalRewardTkns <- pletC foldOutDatumF.totalRewardTokens
    let foldInitChecks =
          pand'List
            [ pfromData numMinted #== 1
            , foldOutDatumF.currNode #== nodeInpDat
            , pfield @"configTN" # nodeInpDat #== configTN
            , totalRewardTkns #== pvalueOf # foldOutputF.value # configF.rewardCS # configF.rewardTN
            , totalRewardTkns #== pvalueOf # (pfield @"value" # projectInput) # configF.rewardCS # configF.rewardTN
            , pvalueOf # foldOutputF.value # pfromData ownPolicyId # rewardFoldTN #== 1
            , pcountOfUniqueTokens # foldOutputF.value #== 3
            , pmatch
                commitFoldNodeF.next
                ( \case
                    PEmpty _ -> pconstant True
                    PKey _ -> pconstant False
                )
            , commitDatF.staked #== foldOutDatumF.totalStaked
            , pvalueOf # mintedValue # rewardConfigF.tokenHolderCS # rewardTokenHolderTN #== -1
            , pvalueOf # mintedValue # rewardConfigF.commitFoldCS # commitFoldTN #== -1
            , nodeInpDat #== nodeOutDat
            , nodeInputF.address #== nodeOutputF.address
            , -- Taking folding fee from head node as an indicator that rewards fold has been initiated
              pforgetPositive nodeInputF.value <> foldingFeeVal #== pforgetPositive nodeOutputF.value
            ]
    pure $
      pif
        foldInitChecks
        (pconstant ())
        perror

data PRewardsFoldState (s :: S) = PRewardsFoldState
  { next :: Term s PNodeKeyState
  , owedRewardTkns :: Term s PInteger
  , foldCount :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PRewardsFoldState where
  type DPTStrat _ = PlutusTypeScott

prewardSuccessor ::
  Term s PStakingConfig ->
  Term s PTokenName ->
  Term s PCurrencySymbol ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PRewardsFoldState ->
  Term s PTxOut ->
  Term s PTxOut ->
  Term s PRewardsFoldState
prewardSuccessor config configTN nodeCS totalRewardTokens totalStaked state inputNode outputNode = unTermCont $ do
  configF <- pletFieldsC @'["rewardTN", "rewardCS", "stakeCS", "stakeTN"] config
  accNodeF <- pmatchC state
  nodeInputF <- pletFieldsC @'["address", "value", "datum"] inputNode
  inputValue <- pletC $ pforgetPositive nodeInputF.value
  (POutputDatum nodeInpDatum) <- pmatchC nodeInputF.datum
  let nodeInpDat = pconvert @PStakingSetNode (pto $ pfromData $ pfield @"outputDatum" # nodeInpDatum)
  nodeInDatF <- pletFieldsC @'["key", "next", "configTN"] nodeInpDat

  nodeStake <- pletC $ pvalueOf # inputValue # configF.stakeCS # configF.stakeTN
  owedRewardTokens <- pletC $ pdiv # (nodeStake * totalRewardTokens) # totalStaked

  nodeOutputF <- pletFieldsC @'["address", "value", "datum"] outputNode
  nodeOutputValue <- pletC $ nodeOutputF.value

  let owedRewardValue = Value.psingleton # configF.rewardCS # configF.rewardTN # owedRewardTokens
      owedAdaValue = Value.psingleton # padaSymbol # padaToken # (-foldingFee)
      nodeKey = toScott $ pfromData nodeInDatF.key

      successorChecks =
        pand'List
          [ ptraceIfFalse "Incorrect order of nodes" $ (accNodeF.next #== nodeKey)
          , ptraceIfFalse "Incorrect reward amount" $ (inputValue <> owedRewardValue <> owedAdaValue) #== pforgetPositive nodeOutputValue
          , ptraceIfFalse "Incorrect node output address" $ nodeOutputF.address #== nodeInputF.address
          , ptraceIfFalse "Incorrect node configTN" $ nodeInDatF.configTN #== configTN
          , ptraceIfFalse "Cannot udpate node datum" $ nodeOutputF.datum #== nodeInputF.datum
          , ptraceIfFalse "Does not contain node token" $ pvalueOfOneScott # nodeCS # inputValue
          ]

      accState =
        pcon @PRewardsFoldState
          accNodeF
            { next = toScott (pfromData nodeInDatF.next)
            , owedRewardTkns = accNodeF.owedRewardTkns + owedRewardTokens
            , foldCount = accNodeF.foldCount + 1
            }

  pure $ pif successorChecks accState perror

pfoldCorrespondingUTxOs ::
  Term s PStakingConfig ->
  Term s PTokenName ->
  Term s PCurrencySymbol ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PRewardsFoldState ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList PTxOut) ->
  Term s PRewardsFoldState
pfoldCorrespondingUTxOs config configTN nodeCS totalRewardTokens totalStaked acc la lb =
  pfoldl2
    # plam
      ( \state nodeIn nodeOut ->
          prewardSuccessor config configTN nodeCS totalRewardTokens totalStaked state nodeIn nodeOut
      )
    # acc
    # la
    # lb

prewardFoldValidatorW :: Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PValidator)
prewardFoldValidatorW = phoistAcyclic $
  plam $ \configCS nodeCS datum redeemer ctx -> P.do
    ctxF <- pletFields @'["txInfo", "purpose"] ctx
    infoF <- pletFields @'["inputs", "outputs", "signatories", "mint", "referenceInputs"] ctxF.txInfo
    PPair configTN config <- pmatch $ fetchConfigDetails # pfromData configCS # infoF.referenceInputs

    let dat = pconvert @PRewardFoldDatum datum
        red = pconvert @PRewardsFoldAct redeemer
     in pmatch red $ \case
          PRewardsFoldNodes r -> pletFields @'["nodeIdxs", "nodeOutIdxs"] r $ \redF ->
            prewardFoldNodes # config # configTN # pfromData nodeCS # redF.nodeIdxs # redF.nodeOutIdxs # dat # ctx
          PRewardsReclaim _ -> unTermCont $ do
            datF <- pletFieldsC @'["owner", "currNode"] dat
            currNodeF <- pletFieldsC @'["next", "configTN"] datF.currNode
            configF <- pletFieldsC @'["penaltyAddress", "rewardTN", "rewardCS"] config

            PPubKeyCredential ((pfield @"_0" #) -> ownerPkh) <- pmatchC (pfield @"credential" # datF.owner)
            let signedByOwner = (ptxSignedByPkh # ownerPkh # infoF.signatories)
                atEnd =
                  pmatch
                    currNodeF.next
                    ( \case
                        PEmpty _ -> pconstant True
                        PKey _ -> pconstant False
                    )

            PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose
            let ownInput = ptryOwnInput # infoF.inputs # ownRef
            ownInputF <- pletFieldsC @'["value"] ownInput
            let rewardFoldCS = pheadSingleton #$ pfindCurrencySymbolsByTokenName # ownInputF.value # rewardFoldTN
            mintedValue <- pletC $ (pnormalize # infoF.mint)

            let rewardsLeft = pvalueOf # ownInputF.value # configF.rewardCS # configF.rewardTN

            let returnsReward = paysAtleastValueToAddress # (Value.passertPositive # (Value.psingleton # configF.rewardCS # configF.rewardTN # rewardsLeft)) # configF.penaltyAddress

            let rewardsReturned = pif (rewardsLeft #== 0) (pconstant True) (pany # returnsReward # infoF.outputs)
             
            pure $
              pif
                ( pand'List
                    [ ptraceIfFalse "Must be signed by owner" $ signedByOwner
                    , ptraceIfFalse "Reward fold must be completed" $ atEnd
                    , ptraceIfFalse "Must burn reward fold token" $ pvalueOf # mintedValue # pfromData rewardFoldCS # rewardFoldTN #== -1
                    , ptraceIfFalse "Incorrect configTN" $ currNodeF.configTN #== configTN
                    , ptraceIfFalse "Incorrect script inputs" $ pcountScriptInputs # infoF.inputs #== 1
                    , ptraceIfFalse "Rewards not returned to penalty address" $ rewardsReturned
                    ]
                )
                (popaque $ pconstant ())
                perror

prewardFoldNodes ::
  Term
    s
    ( PStakingConfig
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PBuiltinList (PAsData PInteger)
        :--> PBuiltinList (PAsData PInteger)
        :--> PRewardFoldDatum
        :--> PScriptContext
        :--> POpaque
    )
prewardFoldNodes = phoistAcyclic $
  plam $ \config configTN nodeCS inputIdxs outputIdxs dat ctx -> unTermCont $ do
    configF <- pletFieldsC @'["rewardTN", "rewardCS", "stakeCS", "stakeTN"] config
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    info <- pletFieldsC @'["inputs", "outputs", "referenceInputs", "mint"] ctxF.txInfo
    txIns <- pletC $ pfromData info.inputs
    txOuts <- pletC $ pfromData info.outputs

    let nodeInputs :: Term _ (PBuiltinList PTxOut)
        nodeInputs = pmap @PBuiltinList # plam (\i -> pfield @"resolved" # (pelemAt' # pfromData i # txIns)) # inputIdxs
        nodeOutputs :: Term _ (PBuiltinList PTxOut)
        nodeOutputs = pmap @PBuiltinList # plam (\i -> (pelemAt' # pfromData i # txOuts)) # outputIdxs

    PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose

    let ownInput = ptryOwnInput # txIns # ownRef
    ownInputF <- pletFieldsC @'["address", "value"] ownInput
    PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatchC (pfield @"credential" # ownInputF.address)
    datF <- pletFieldsC @'["currNode", "totalRewardTokens", "totalStaked", "owner"] dat
    currFoldNodeF <- pletFieldsC @'["key", "next", "configTN"] datF.currNode

    let ownOutput = ptryOwnOutput # txOuts # ownValHash
    ownOutputF <- pletFieldsC @'["value", "datum"] ownOutput

    let foldOutDatum = pfromPDatum @PRewardFoldDatum #$ ptryFromInlineDatum # ownOutputF.datum
    newDatumF <- pletFieldsC @'["currNode", "totalRewardTokens", "totalStaked", "owner"] foldOutDatum
    newFoldNodeF <- pletFieldsC @'["key", "next", "configTN"] newDatumF.currNode

    let rewardsFoldState =
          pcon
            ( PRewardsFoldState
                (toScott $ pfromData currFoldNodeF.next)
                0
                1
            )

    totalRewardTokens <- pletC datF.totalRewardTokens
    totalStake <- pletC datF.totalStaked

    newRewardsFoldState <- pmatchC $ pfoldCorrespondingUTxOs config configTN nodeCS totalRewardTokens totalStake rewardsFoldState nodeInputs nodeOutputs
    let owedRewardTknsValue = Value.psingleton # configF.rewardCS # configF.rewardTN # (-newRewardsFoldState.owedRewardTkns)

    let foldChecks =
          pand'List
            [ ptraceIfFalse "Cannot change key" $ newFoldNodeF.key #== currFoldNodeF.key
            , ptraceIfFalse "Cannot change totalRewardTokens" $ newDatumF.totalRewardTokens #== totalRewardTokens
            , ptraceIfFalse "Cannot change totalStaked" $ newDatumF.totalStaked #== totalStake
            , ptraceIfFalse "Cannot change owner" $ newDatumF.owner #== datF.owner
            , ptraceIfFalse "Incorrect next node" $ newRewardsFoldState.next #== (toScott $ pfromData newFoldNodeF.next)
            , ptraceIfFalse "Incorrect amount of rewards spent" $ pnormalize # (Value.pforgetPositive ownInputF.value <> owedRewardTknsValue) #== Value.pforgetPositive ownOutputF.value
            , ptraceIfFalse "Incorrect script inputs" $ (pcountScriptInputs # txIns) #== newRewardsFoldState.foldCount
            , ptraceIfFalse "Incorrect configTN" $ currFoldNodeF.configTN #== configTN
            , ptraceIfFalse "Cannot change configTN" $ newFoldNodeF.configTN #== configTN
            ]
    pure $
      pif foldChecks (popaque (pconstant ())) perror

-- prewardFoldNode ::
--   Term
--     s
--     ( PStakingConfig
--         :--> PTokenName
--         :--> PCurrencySymbol
--         :--> PRewardFoldDatum
--         :--> PScriptContext
--         :--> POpaque
--     )
-- prewardFoldNode = phoistAcyclic $
--   plam $ \config configTN nodeCS dat ctx -> unTermCont $ do
--     configF <- pletFieldsC @'["rewardTN", "rewardCS", "stakeCS", "stakeTN"] config
--     ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
--     info <- pletFieldsC @'["inputs", "outputs", "referenceInputs", "mint"] ctxF.txInfo

--     PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose
--     let ownInput = ptryOwnInput # info.inputs # ownRef
--     ownInputF <- pletFieldsC @'["address", "value"] ownInput
--     PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatchC (pfield @"credential" # ownInputF.address)

--     datF <- pletFieldsC @'["currNode", "totalRewardTokens", "totalStaked", "owner"] dat
--     currFoldNodeF <- pletFieldsC @'["key", "next", "configTN"] datF.currNode

--     txOuts <- pletC info.outputs
--     let ownOutput = ptryOwnOutput # txOuts # ownValHash
--     ownOutputF <- pletFieldsC @'["address", "value", "datum"] ownOutput

--     let foldOutDatum = pfromPDatum @PRewardFoldDatum #$ ptryFromInlineDatum # ownOutputF.datum
--     foldOutDatumF <- pletFieldsC @'["currNode", "totalRewardTokens", "totalStaked", "owner"] foldOutDatum
--     newFoldNodeF <- pletFieldsC @'["key", "next", "configTN"] foldOutDatumF.currNode

--     oldTotalRewardTokens <- pletC datF.totalRewardTokens
--     oldTotalStaked <- pletC datF.totalStaked

--     let nodeInputs =
--           pfilter @PBuiltinList
--             # plam (\inp -> (pvalueOfOneScott # nodeCS # (pfield @"value" # (pfield @"resolved" # inp))))
--             # info.inputs
--     nodeInputF <- pletFieldsC @'["address", "value", "datum"] (pfield @"resolved" # (pheadSingleton # nodeInputs))

--     nodeInputValue <- pletC nodeInputF.value
--     nodeStake <- pletC $ pvalueOf # nodeInputValue # configF.stakeCS # configF.stakeTN

--     owedRewardTkns <- pletC $ pdiv # (nodeStake * oldTotalRewardTokens) # oldTotalStaked
--     -- doesn't work with no decimal tokens
--     let nodeInpDat = pfromPDatum @PStakingSetNode #$ ptryFromInlineDatum # nodeInputF.datum
--     nodeInpDatF <- pletFieldsC @'["key", "next", "configTN"] nodeInpDat

--     let nodeOutput = ptryOutputToAddress # txOuts # nodeInputF.address
--     nodeOutputF <- pletFieldsC @'["value", "datum"] nodeOutput
--     let nodeOutDat = pfromPDatum @PStakingSetNode #$ ptryFromInlineDatum # nodeOutputF.datum

--     mkRewardValue <- pletC $ Value.psingleton # configF.rewardCS # configF.rewardTN
--     mkAdaValue <- pletC $ Value.psingleton # padaSymbol # padaToken
--     distributedValue <- pletC $ mkRewardValue # (-owedRewardTkns)
--     posDistributedValue <- pletC $ mkRewardValue # owedRewardTkns
--     collectedAdaValue <- pletC $ mkAdaValue # (-foldingFee)

--     let correctOwnOutput = (pforgetPositive ownInputF.value) <> distributedValue
--         correctNodeOutput = (pforgetPositive nodeInputValue <> posDistributedValue) <> collectedAdaValue

--     let foldChecks =
--           pand'List
--             [ ptraceIfFalse "Cannot change key" $ currFoldNodeF.key #== newFoldNodeF.key
--             , ptraceIfFalse "Incorrect node folded" $ currFoldNodeF.next #== nodeInpDatF.key
--             , ptraceIfFalse "Incorrect next node" $ newFoldNodeF.next #== nodeInpDatF.next
--             , ptraceIfFalse "Cannot change totalStaked" $ foldOutDatumF.totalStaked #== oldTotalStaked
--             , ptraceIfFalse "Cannot change totalRewardTokens" $ foldOutDatumF.totalRewardTokens #== oldTotalRewardTokens
--             , ptraceIfFalse "Cannot change owner" $ foldOutDatumF.owner #== datF.owner
--             , ptraceIfFalse "Incorrect reward spent" $ (pnoAdaValue # correctOwnOutput) #== (pnoAdaValue #$ pforgetPositive ownOutputF.value)
--             , ptraceIfFalse "Incorrect reward distributed" $ correctNodeOutput #== (pforgetPositive nodeOutputF.value)
--             , ptraceIfFalse "Incorrect node configTN" $ nodeInpDatF.configTN #== configTN
--             , ptraceIfFalse "Cannot change node datum" $ nodeInpDat #== nodeOutDat
--             , ptraceIfFalse "Incorrect input configTN" $ currFoldNodeF.configTN #== configTN
--             , ptraceIfFalse "Incorrect output configTN" $ newFoldNodeF.configTN #== configTN
--             ]
--     pure $ pif foldChecks (popaque (pconstant ())) perror
