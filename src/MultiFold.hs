{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module MultiFold (pfoldValidatorW, pmintFoldPolicyW, pmintRewardFoldPolicyW, prewardFoldValidatorW) where 

import Plutarch.Api.V1 (PCredential (..))
import Plutarch.Api.V1.Value (padaSymbol, padaToken, pforgetPositive, pnoAdaValue, pnormalize, pvalueOf)
import Plutarch.Api.V1.Value qualified as Value
import Plutarch.Api.V2 (
  PAddress (..),
  PCurrencySymbol,
  PMintingPolicy,
  POutputDatum (POutputDatum),
  PPOSIXTime (..),
  PScriptContext,
  PScriptPurpose (PMinting, PSpending),
  PTokenName (..),
  PTxInInfo (..),
  PTxOut,
  PValidator,
 )
import Plutarch.Bool (pand')
import Plutarch.DataRepr (
  PDataFields,
 )
import Plutarch.Extra.Interval (pbefore)
import Plutarch.Extra.ScriptContext (pfromPDatum, ptryFromInlineDatum)
import Plutarch.Prelude
import Types.Classes
import Types.Constants (commitFoldTN, foldingFee, poriginNodeTN, rewardTokenHolderTN, rewardFoldTN)
import Types.StakingSet
import Utils (
  pand'List,
  pcountOfUniqueTokens,
  pcountScriptInputs,
  pelemAt',
  pfindCurrencySymbolsByTokenName,
  pfoldl2,
  pheadSingleton,
  ptryLookupValue,
  ptryOutputToAddress,
  ptryOwnInput,
  ptryOwnOutput,
  ptxSignedByPkh,
  pvalueOfOneScott,
 )
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
 )
import Conversions (pconvert)

data PFoldMintConfig (s :: S)
  = PFoldMintConfig
      ( Term
          s
          ( PDataRecord
              '[ "nodeCS" ':= PCurrencySymbol
               , "foldAddr" ':= PAddress
               , "endStaking" ':= PPOSIXTime
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

data PFoldConfig (s :: S)
  = PFoldConfig
      ( Term
          s
          ( PDataRecord
              '[ "nodeCS" ':= PCurrencySymbol
               , "stakeCS" ':= PCurrencySymbol
               , "stakeTN" ':= PTokenName
               , "endStaking" ':= PPOSIXTime
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PFoldConfig where
  type DPTStrat _ = PlutusTypeData

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
    contextFields <- pletFieldsC @'["txInfo", "purpose"] ctx
    PMinting policy <- pmatchC contextFields.purpose
    ownPolicyId <- pletC $ pfield @"_0" # policy

    info <- pletFieldsC @'["mint"] contextFields.txInfo
    tkPairs <- pletC $ ptryLookupValue # ownPolicyId # (pnormalize # info.mint)
    tkPair <- pletC (pheadSingleton # tkPairs)
    let numMinted = psndBuiltin # tkPair
    pure $
      pif (pfromData numMinted #== -1) (pconstant ()) perror

pmintCommitFold :: Term s (PFoldMintConfig :--> PScriptContext :--> PUnit)
pmintCommitFold = phoistAcyclic $
  plam $ \fconfig ctx -> unTermCont $ do
    foldConfF <- pletFieldsC @'["nodeCS", "foldAddr", "endStaking"] fconfig
    contextFields <- pletFieldsC @'["txInfo", "purpose"] ctx

    PMinting policy <- pmatchC contextFields.purpose
    ownPolicyId <- pletC $ pfield @"_0" # policy

    info <- pletFieldsC @'["referenceInputs", "outputs", "mint", "validRange"] contextFields.txInfo

    tkPairs <- pletC $ ptryLookupValue # ownPolicyId # (pnormalize # info.mint)
    tkPair <- pletC (pheadSingleton # tkPairs)

    let numMinted = psndBuiltin # tkPair
        foldOutput = ptryOutputToAddress # info.outputs # foldConfF.foldAddr
        refInput = pfield @"resolved" # (pheadSingleton @PBuiltinList # info.referenceInputs)

    refInputF <- pletFieldsC @'["value", "datum"] refInput

    (POutputDatum refInpDatum) <- pmatchC refInputF.datum
    let refInpDat = pfromPDatum @PStakingSetNode # (pfield @"outputDatum" # refInpDatum)

    foldOutputF <- pletFieldsC @'["value", "datum"] foldOutput
    (POutputDatum foldOutputDatum) <- pmatchC foldOutputF.datum

    let foldOutDatum = pfromPDatum @PFoldDatum # (pfield @"outputDatum" # foldOutputDatum)
    foldOutDatumF <- pletFieldsC @'["currNode", "staked"] foldOutDatum

    let foldInitChecks =
          pand'List
            [ pfromData numMinted #== 1
            , pvalueOf # foldOutputF.value # pfromData ownPolicyId # commitFoldTN #== pconstant 1
            , foldOutDatumF.currNode #== refInpDat
            , pfromData foldOutDatumF.staked #== pconstant 0
            , pvalueOf # refInputF.value # foldConfF.nodeCS # poriginNodeTN #== pconstant 1
            , pbefore # pfromData foldConfF.endStaking # info.validRange
            ]
    pure $
      pif
        foldInitChecks
        (pconstant ())
        perror

pfoldValidatorW :: Term s (PFoldConfig :--> PValidator)
pfoldValidatorW = phoistAcyclic $
  plam $ \config datum redeemer ctx ->
    let dat = pconvert @PFoldDatum datum
        red = pconvert @PFoldAct redeemer
     in pmatch red $ \case
          PFoldNodes r -> pletFields @'["nodeIdxs"] r $ \redF ->
            pfoldNodes # config # redF.nodeIdxs # dat # ctx
          PReclaim _ -> unTermCont $ do
            PPubKeyCredential ((pfield @"_0" #) -> ownerPkh) <- pmatchC (pfield @"credential" # (pfield @"owner" # dat))
            ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
            infoF <- pletFieldsC @'["inputs", "signatories", "mint"] ctxF.txInfo
            PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose
            let ownInput = ptryOwnInput # infoF.inputs # ownRef
            ownInputF <- pletFieldsC @'["address", "value"] ownInput
            let possibleCSs = pfindCurrencySymbolsByTokenName # ownInputF.value # commitFoldTN
                commitCS = pheadSingleton # possibleCSs
                commitPairs = ptryLookupValue # commitCS # infoF.mint
                commitPair = (pheadSingleton # commitPairs)
                commitMinted = psndBuiltin # commitPair
            pure $
              pif
                (ptxSignedByPkh # ownerPkh # infoF.signatories #&& pfromData commitMinted #== -1)
                (popaque $ pconstant ())
                perror

pisSuccessor :: Term s (PFoldConfig :--> PCommitFoldState :--> PTxOut :--> PCommitFoldState)
pisSuccessor = plam $ \config accNode node -> unTermCont $ do
  configF <- pletFieldsC @'["nodeCS", "stakeCS", "stakeTN"] config
  accNodeF <- pmatchC accNode
  nodeF <- pletFieldsC @'["value", "datum"] node
  nodeValue <- pletC nodeF.value
  let nodeDatum = pfromPDatum @PStakingSetNode #$ (ptryFromInlineDatum # nodeF.datum)
      hasNodeTk = pvalueOfOneScott # configF.nodeCS # nodeValue
  currNodeF <- pletFieldsC @'["key", "next"] nodeDatum

  let nodeKey = toScott $ pfromData currNodeF.key
      accState =
        pcon @PCommitFoldState
          accNodeF
            { next = toScott (pfromData currNodeF.next)
            , staked = accNodeF.staked + (pvalueOf # nodeValue # configF.stakeCS # configF.stakeTN)
            }

  pure $ pif (pand' # (accNodeF.next #== nodeKey) # hasNodeTk) accState perror

pfoldNodes :: Term s (PFoldConfig :--> PBuiltinList (PAsData PInteger) :--> PFoldDatum :--> PScriptContext :--> POpaque)
pfoldNodes = phoistAcyclic $
  plam $ \config nodeIndices dat ctx -> unTermCont $ do
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    -- all reference inputs have node currency symbol
    -- all reference inputs are connected in the linked list
    info <- pletFieldsC @'["inputs", "referenceInputs", "outputs", "mint", "validRange"] ctxF.txInfo
    PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose

    let ownInput = ptryOwnInput # info.inputs # ownRef
    ownInputF <- pletFieldsC @'["address", "value"] ownInput
    PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatchC (pfield @"credential" # ownInputF.address)
    datF <- pletFieldsC @'["currNode", "staked", "owner"] dat
    currFoldNodeF <- pletFieldsC @'["key", "next"] datF.currNode

    let ownOutput = ptryOwnOutput # info.outputs # ownValHash
    ownOutputF <- pletFieldsC @'["address", "value", "datum"] ownOutput
    (POutputDatum foldOutputDatum) <- pmatchC ownOutputF.datum

    let commitFoldState =
          pcon
            ( PCommitFoldState
                (toScott $ pfromData currFoldNodeF.key)
                (toScott $ pfromData currFoldNodeF.next)
                (pfromData datF.staked)
            )
        foldOutDatum = pfromPDatum @PFoldDatum # (pfield @"outputDatum" # foldOutputDatum)
    newFoldDatumF <- pletFieldsC @'["currNode", "staked", "owner"] foldOutDatum
    newFoldNodeF <- pletFieldsC @'["key", "next"] newFoldDatumF.currNode

    let refInputs :: Term _ (PBuiltinList PTxInInfo)
        refInputs = info.referenceInputs

    refIns <- pletC refInputs

    let nodeInputs :: Term _ (PBuiltinList PTxOut)
        nodeInputs = pmap @PBuiltinList # plam (\i -> pfield @"resolved" # (pelemAt' # pfromData i # refIns)) # nodeIndices
        newCommitFoldState' = pfoldl # (pisSuccessor # config) # commitFoldState # nodeInputs
        countOwnInputs =
          plength
            # ( pfilter @PBuiltinList
                  # plam (\txInp -> (pfield @"address" # (pfield @"resolved" # txInp)) #== ownInputF.address)
                  # info.inputs
              )
    newCommitFoldState <- pmatchC newCommitFoldState'
    let foldChecks =
          pand'List
            [ countOwnInputs #== 1
            , pfromData info.mint #== mempty
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
               , "rewardTN" ':= PTokenName
               , "rewardCS" ':= PCurrencySymbol
               , "commitFoldCS" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PRewardMintFoldConfig where
  type DPTStrat _ = PlutusTypeData

data PRewardFoldConfig (s :: S)
  = PRewardFoldConfig
      ( Term
          s
          ( PDataRecord
              '[ "nodeCS" ':= PCurrencySymbol
               , "rewardCS" ':= PCurrencySymbol
               , "rewardTN" ':= PTokenName
               , "stakeCS" ':= PCurrencySymbol
               , "stakeTN" ':= PTokenName
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PRewardFoldConfig where
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
  | PRewardsFoldNode (Term s (PDataRecord '[]))
  | PRewardsReclaim (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PRewardsFoldAct where
  type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PRewardsFoldAct

pmintRewardFoldPolicyW :: Term s (PRewardMintFoldConfig :--> PMintingPolicy)
pmintRewardFoldPolicyW = phoistAcyclic $
  plam $ \rewardConfig _redm ctx -> unTermCont $ do
    rewardConfigF <- pletFieldsC @'["nodeCS", "tokenHolderCS", "rewardScriptAddr", "rewardTN", "rewardCS", "commitFoldCS"] rewardConfig
    contextFields <- pletFieldsC @'["txInfo", "purpose"] ctx

    PMinting policy <- pmatchC contextFields.purpose
    ownPolicyId <- pletC $ pfield @"_0" # policy

    info <- pletFieldsC @'["inputs", "referenceInputs", "outputs", "mint"] contextFields.txInfo

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
        nodeRefInput =
          pfield @"resolved"
            #$ pheadSingleton
            # ( pfilter @PBuiltinList
                  # plam (\inp -> pvalueOf # (pfield @"value" # (pfield @"resolved" # inp)) # rewardConfigF.nodeCS # poriginNodeTN #== 1)
                  # info.referenceInputs
              )
        projectInput =
          pfield @"resolved"
            #$ pheadSingleton
            # ( pfilter @PBuiltinList
                  # plam (\inp -> pvalueOf # (pfield @"value" # (pfield @"resolved" # inp)) # rewardConfigF.tokenHolderCS # rewardTokenHolderTN #== 1)
                  # info.inputs
              )
        numMinted = psndBuiltin # tkPair
        foldOutput = ptryOutputToAddress # info.outputs # rewardConfigF.rewardScriptAddr

    commitInpF <- pletFieldsC @'["value", "datum"] commitInp
    (POutputDatum commitDatum) <- pmatchC commitInpF.datum
    let commitDat = pfromPDatum @PFoldDatum # (pfield @"outputDatum" # commitDatum)
    commitDatF <- pletFieldsC @'["currNode", "staked", "owner"] commitDat
    commitFoldNodeF <- pletFieldsC @'["key", "next"] commitDatF.currNode

    refInputF <- pletFieldsC @'["value", "datum"] nodeRefInput

    (POutputDatum refInpDatum) <- pmatchC refInputF.datum
    let refInpDat = pfromPDatum @PStakingSetNode # (pfield @"outputDatum" # refInpDatum)

    foldOutputF <- pletFieldsC @'["value", "datum"] foldOutput
    (POutputDatum foldOutputDatum) <- pmatchC foldOutputF.datum

    let foldOutDatum = pfromPDatum @PRewardFoldDatum # (pfield @"outputDatum" # foldOutputDatum)
    foldOutDatumF <- pletFieldsC @'["currNode", "totalRewardTokens", "totalStaked"] foldOutDatum

    totalRewardTkns <- pletC foldOutDatumF.totalRewardTokens
    let foldInitChecks =
          pand'List
            [ pfromData numMinted #== 1
            , foldOutDatumF.currNode #== refInpDat
            , totalRewardTkns #== pvalueOf # foldOutputF.value # rewardConfigF.rewardCS # rewardConfigF.rewardTN
            , totalRewardTkns #== pvalueOf # (pfield @"value" # projectInput) # rewardConfigF.rewardCS # rewardConfigF.rewardTN
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
            ]
    pure $
      pif
        foldInitChecks
        (popaque $ pconstant ())
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
  Term s PRewardFoldConfig ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PRewardsFoldState ->
  Term s PTxOut ->
  Term s PTxOut ->
  Term s PRewardsFoldState
prewardSuccessor config totalRewardTokens totalStaked state inputNode outputNode = unTermCont $ do
  configF <- pletFieldsC @'["nodeCS", "rewardTN", "rewardCS", "stakeCS" , "stakeTN"] config
  accNodeF <- pmatchC state
  nodeInputF <- pletFieldsC @'["address", "value", "datum"] inputNode
  inputValue <- pletC $ pforgetPositive nodeInputF.value
  (POutputDatum nodeInpDatum) <- pmatchC nodeInputF.datum
  let nodeInpDat = pfromPDatum @PStakingSetNode #(pfield @"outputDatum" # nodeInpDatum)
  nodeInDatF <- pletFieldsC @'["key", "next"] nodeInpDat

  nodeStake <- pletC $ pvalueOf # inputValue # configF.stakeCS # configF.stakeTN
  owedRewardTokens <- pletC $ pdiv # (nodeStake * totalRewardTokens) # totalStaked

  nodeOutputF <- pletFieldsC @'["address", "value", "datum"] outputNode
  nodeOutputValue <- pletC $ nodeOutputF.value

  let owedRewardValue = Value.psingleton # configF.rewardCS # configF.rewardTN # owedRewardTokens
      owedAdaValue = Value.psingleton # padaSymbol # padaToken # (-foldingFee)
      nodeKey = toScott $ pfromData nodeInDatF.key

      successorChecks =
        pand'List
          [ (accNodeF.next #== nodeKey)
          , (inputValue <> owedRewardValue <> owedAdaValue) #== pforgetPositive nodeOutputValue
          , nodeOutputF.address #== nodeInputF.address
          , nodeOutputF.datum #== nodeInputF.datum
          , pvalueOfOneScott # configF.nodeCS # inputValue
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
  Term s PRewardFoldConfig ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PRewardsFoldState ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList PTxOut) ->
  Term s PRewardsFoldState
pfoldCorrespondingUTxOs config totalRewardTokens totalStaked acc la lb =
  pfoldl2
    # plam
      ( \state nodeIn nodeOut ->
          prewardSuccessor config totalRewardTokens totalStaked state nodeIn nodeOut
      )
    # acc
    # la
    # lb

prewardFoldValidatorW :: Term s (PRewardFoldConfig :--> PValidator)
prewardFoldValidatorW = phoistAcyclic $
  plam $ \rewardConfig datum redeemer ctx ->
    let dat = pconvert @PRewardFoldDatum datum
        red = pconvert @PRewardsFoldAct redeemer
     in pmatch red $ \case
          PRewardsFoldNode _ -> prewardFoldNode # rewardConfig # dat # ctx
          PRewardsFoldNodes r -> pletFields @'["nodeIdxs", "nodeOutIdxs"] r $ \redF ->
            prewardFoldNodes # rewardConfig # redF.nodeIdxs # redF.nodeOutIdxs # dat # ctx
          PRewardsReclaim _ -> unTermCont $ do
            PPubKeyCredential ((pfield @"_0" #) -> ownerPkh) <- pmatchC (pfield @"credential" # (pfield @"owner" # dat))
            infoF <- pletFieldsC @'["signatories"] (pfield @"txInfo" # ctx)
            let signedByOwner = (ptxSignedByPkh # ownerPkh # infoF.signatories)
                atEnd =
                  pmatch
                    (pfield @"next" # (pfield @"currNode" # dat))
                    ( \case
                        PEmpty _ -> pconstant True
                        PKey _ -> pconstant False
                    )
            pure $
              pif
                (signedByOwner #&& atEnd)
                (popaque $ pconstant ())
                perror

prewardFoldNodes ::
  Term
    s
    ( PRewardFoldConfig
        :--> PBuiltinList (PAsData PInteger)
        :--> PBuiltinList (PAsData PInteger)
        :--> PRewardFoldDatum
        :--> PScriptContext
        :--> POpaque
    )
prewardFoldNodes = phoistAcyclic $ plam $ \rewardConfig inputIdxs outputIdxs dat ctx -> unTermCont $ do
  rewardConfigF <- pletFieldsC @'["nodeCS", "rewardTN", "rewardCS", "stakeCS" , "stakeTN"] rewardConfig
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
  currFoldNodeF <- pletFieldsC @'["key", "next"] datF.currNode

  let ownOutput = ptryOwnOutput # txOuts # ownValHash
  ownOutputF <- pletFieldsC @'["value", "datum"] ownOutput
  (POutputDatum foldOutputDatum) <- pmatchC ownOutputF.datum

  let foldOutDatum = pfromPDatum @PRewardFoldDatum # (pfield @"outputDatum" # foldOutputDatum)
  newDatumF <- pletFieldsC @'["currNode", "totalRewardTokens", "totalStaked", "owner"] foldOutDatum
  newFoldNodeF <- pletFieldsC @'["key", "next"] newDatumF.currNode

  let rewardsFoldState =
        pcon
          ( PRewardsFoldState
              (toScott $ pfromData currFoldNodeF.next)
              0
              1
          )

  totalRewardTokens <- pletC datF.totalRewardTokens
  totalStake <- pletC datF.totalStaked

  newRewardsFoldState <- pmatchC $ pfoldCorrespondingUTxOs rewardConfig totalRewardTokens totalStake rewardsFoldState nodeInputs nodeOutputs
  let owedRewardTknsValue = Value.psingleton # rewardConfigF.rewardCS # rewardConfigF.rewardTN # (-newRewardsFoldState.owedRewardTkns)
  
  let foldChecks =
        pand'List
          [ newFoldNodeF.key #== currFoldNodeF.key
          , newDatumF.totalRewardTokens #== totalRewardTokens
          , newDatumF.totalStaked #== totalStake
          , newDatumF.owner #== datF.owner
          , newRewardsFoldState.next #== (toScott $ pfromData newFoldNodeF.next)
          , pnormalize # (Value.pforgetPositive ownInputF.value <> owedRewardTknsValue) #== Value.pforgetPositive ownOutputF.value
          , (pcountScriptInputs # txIns) #== newRewardsFoldState.foldCount
          ]
  pure $
    pif foldChecks (popaque (pconstant ())) perror

prewardFoldNode :: Term s (PRewardFoldConfig :--> PRewardFoldDatum :--> PScriptContext :--> POpaque)
prewardFoldNode = phoistAcyclic $
  plam $ \rewardConfig dat ctx -> unTermCont $ do
    rewardConfigF <- pletFieldsC @'["nodeCS", "rewardTN", "rewardCS", "stakeCS" , "stakeTN"] rewardConfig
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    info <- pletFieldsC @'["inputs", "outputs", "referenceInputs", "mint"] ctxF.txInfo

    PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose
    let ownInput = ptryOwnInput # info.inputs # ownRef
    ownInputF <- pletFieldsC @'["address", "value"] ownInput
    PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatchC (pfield @"credential" # ownInputF.address)

    datF <- pletFieldsC @'["currNode", "totalRewardTokens", "totalStaked", "owner"] dat
    currFoldNodeF <- pletFieldsC @'["key", "next"] datF.currNode

    txOuts <- pletC info.outputs
    let ownOutput = ptryOwnOutput # txOuts # ownValHash
    ownOutputF <- pletFieldsC @'["address", "value", "datum"] ownOutput
    (POutputDatum foldOutputDatum) <- pmatchC ownOutputF.datum

    let foldOutDatum = pfromPDatum @PRewardFoldDatum # (pfield @"outputDatum" # foldOutputDatum)
    foldOutDatumF <- pletFieldsC @'["currNode", "totalRewardTokens", "totalStaked", "owner"] foldOutDatum
    oldTotalRewardTokens <- pletC datF.totalRewardTokens
    oldTotalStaked <- pletC datF.totalStaked
    nodeCSS <- pletC $ pfromData rewardConfigF.nodeCS
    let nodeInputs =
          pfilter @PBuiltinList
            # plam (\inp -> (pvalueOfOneScott # nodeCSS # (pfield @"value" # (pfield @"resolved" # inp))))
            # info.inputs
    nodeInputF <- pletFieldsC @'["address", "value", "datum"] (pfield @"resolved" # (pheadSingleton # nodeInputs))
    (POutputDatum nodeInpDatum) <- pmatchC nodeInputF.datum

    nodeInputValue <- pletC nodeInputF.value

    nodeStake <- pletC $ pvalueOf # nodeInputValue # rewardConfigF.stakeCS # rewardConfigF.stakeTN
    owedRewardTkns <- pletC $ pdiv # (nodeStake * oldTotalRewardTokens) # oldTotalStaked
    -- doesn't work with no decimal tokens
    let nodeInpDat = pfromPDatum @PStakingSetNode #$ pfield @"outputDatum" # nodeInpDatum
    nodeInpDatF <- pletFieldsC @'["key", "next"] nodeInpDat

    let nodeOutput = ptryOutputToAddress # txOuts # nodeInputF.address
    nodeOutputF <- pletFieldsC @'["value"] nodeOutput
    
    mkRewardValue <- pletC $ Value.psingleton # rewardConfigF.rewardCS # rewardConfigF.rewardTN
    mkAdaValue <- pletC $ Value.psingleton # padaSymbol # padaToken
    distributedValue <- pletC $ mkRewardValue # (-owedRewardTkns)
    posDistributedValue <- pletC $ mkRewardValue # owedRewardTkns
    collectedAdaValue <- pletC $ mkAdaValue # (-foldingFee)

    let correctOwnOutput = (pforgetPositive ownInputF.value) <> distributedValue
        correctNodeOutput = (pforgetPositive nodeInputValue <> posDistributedValue) <> collectedAdaValue
        
    let foldChecks =
          pand'List
            [ currFoldNodeF.next #== nodeInpDatF.key
            , foldOutDatumF.currNode #== nodeInpDat
            , foldOutDatumF.totalStaked #== oldTotalStaked
            , foldOutDatumF.totalRewardTokens #== oldTotalRewardTokens
            , foldOutDatumF.owner #== datF.owner
            , (pnoAdaValue # correctOwnOutput) #== (pnoAdaValue #$ pforgetPositive ownOutputF.value)
            , correctNodeOutput #== (pforgetPositive nodeOutputF.value)
            ]
    pure $ pif foldChecks (popaque (pconstant ())) perror
