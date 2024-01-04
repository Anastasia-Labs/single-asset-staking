{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Mint.Common (
  PStakingCommon (..),
  makeCommon,
  pInit,
  pDeinit,
  pRemove,
  pInsert,
  pClaim,
) where

import Mint.Helpers (
  correctNodeTokenMinted,
  correctNodeTokensMinted,
  coversKey,
 )
import Plutarch.Api.V1.Value (plovelaceValueOf, pnormalize, pvalueOf)
import Plutarch.Api.V2 (
  AmountGuarantees (..),
  KeyGuarantees (..),
  PCurrencySymbol,
  PInterval,
  POutputDatum (..),
  PPOSIXTime,
  PPubKeyHash,
  PScriptContext,
  PScriptHash (..),
  PScriptPurpose (PMinting),
  PTxInInfo,
  PTxOut,
  PValue,
 )
import Plutarch.Extra.Interval (pafter, pbefore)
import Plutarch.Extra.ScriptContext (pfromPDatum, ptryFromInlineDatum)
import Plutarch.Internal (Config (..))
import Plutarch.List (pconvertLists)
import Plutarch.Monadic qualified as P
import Plutarch.Positive (PPositive)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2 (CurrencySymbol)
import Types.Constants (minAda, minCommitment, pcorrNodeTN, pnodeKeyTN, poriginNodeTN, pparseNodeKey)
import Types.StakingSet (
  PNodeKey (..),
  PStakingConfig,
  PStakingSetNode,
  asPredecessorOf,
  asSuccessorOf,
  isEmptySet,
  isFirstNode,
  isLastNode,
  isNothing,
  validNode,
 )
import Utils (
  pand'List,
  passert,
  paysToAddress,
  paysToCredential,
  pcheck,
  pcountOfUniqueTokens,
  pfindCurrencySymbolsByTokenPrefix,
  pfindWithRest,
  phasCS,
  phasDataCS,
  pheadSingleton,
  pmapMaybe,
  pmustFind,
  psingletonOfCS,
  pvalueOfOne,
  (#>),
  (#>=),
 )
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pguardC)

pdivideCeil :: Term s (PInteger :--> PInteger :--> PInteger)
pdivideCeil = phoistAcyclic $ plam $ \a b -> (pdiv # a # b) + pif ((pmod # a # b) #> 0) 1 0

nodeInputUtxoDatum ::
  ClosedTerm
    ( PAsData PCurrencySymbol
        :--> PTxOut
        :--> PMaybe (PAsData PStakingSetNode)
    )
nodeInputUtxoDatum = phoistAcyclic $
  plam $ \nodeCS out -> P.do
    txOut <- pletFields @'["datum", "value"] out
    let value = pfromData txOut.value
    pcheck (phasDataCS # nodeCS # value) $
      punsafeCoerce $
        ptryFromInlineDatum # txOut.datum

nodeInputUtxoDatumUnsafe ::
  ClosedTerm
    ( PTxOut
        :--> PPair (PValue 'Sorted 'Positive) (PAsData PStakingSetNode)
    )
nodeInputUtxoDatumUnsafe = phoistAcyclic $
  plam $ \out -> pletFields @'["value", "datum"] out $ \outF ->
    plet (punsafeCoerce $ ptryFromInlineDatum # outF.datum) $ \nodeDat ->
      pcon (PPair (pfromData outF.value) nodeDat)

parseNodeOutputUtxo ::
  ClosedTerm
    ( PAsData PCurrencySymbol
        :--> PTxOut
        :--> PPair (PValue 'Sorted 'Positive) (PAsData PStakingSetNode)
    )
parseNodeOutputUtxo = phoistAcyclic $
  plam $ \nodeCS out -> P.do
    txOut <- pletFields @'["address", "value", "datum"] out
    value <- plet $ pfromData $ txOut.value
    PPair tn amount <- pmatch $ psingletonOfCS # nodeCS # value
    POutputDatum od <- pmatch $ pfromData $ txOut.datum
    datum <- plet $ pfromPDatum #$ pfield @"outputDatum" # od
    let nodeKey = pparseNodeKey # tn
        datumKey = pmatch (pfield @"key" # datum) $ \case
          PEmpty _ -> pcon PNothing
          PKey ((pfield @"_0" #) -> key) -> pcon $ PJust key

    -- Prevents TokenDust attack
    passert "All FSN tokens from node policy" $
      pheadSingleton # (pfindCurrencySymbolsByTokenPrefix # value # pconstant "FSN") #== nodeCS
    passert "Too many assets" $ pcountOfUniqueTokens # value #== 2
    passert "Incorrect number of nodeTokens" $ amount #== 1
    passert "node is not ordered" $ validNode # datum
    passert "Incorrect token name" $ nodeKey #== datumKey
    passert "Does not hold nodeAda" $
      plovelaceValueOf # value #>= minCommitment
    pcon (PPair value datum)

makeCommon ::
  forall {r :: PType} {s :: S}.
  Term s PScriptContext ->
  TermCont @r
    s
    ( PStakingCommon s
    , Term s (PBuiltinList PTxInInfo)
    , Term s (PBuiltinList PTxOut)
    , Term s (PBuiltinList (PAsData PPubKeyHash))
    , Term s (PInterval PPOSIXTime)
    )
makeCommon ctx' = do
  ------------------------------
  -- Preparing info needed for validation:
  ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
  info <-
    tcont $
      pletFields
        @'["inputs", "outputs", "mint", "referenceInputs", "signatories", "validRange"]
        ctx.txInfo

  ownCS <- tcont . plet $ P.do
    PMinting mintRecord <- pmatch $ ctx.purpose
    pfield @"_0" # mintRecord

  mint <- tcont . plet $ pnormalize #$ pfromData info.mint
  asOuts <- tcont . plet $ pmap # plam (pfield @"resolved" #)
  -- refInsAsOuts <- tcont . plet $ asOuts # pfromData info.referenceInputs
  hasNodeTk <- tcont . plet $ phasDataCS # ownCS
  insAsOuts <- tcont . plet $ asOuts # pfromData info.inputs
  onlyAtNodeVal <- tcont . plet $ pfilter # plam (\txo -> (hasNodeTk # (pfield @"value" # txo)))
  fromNodeValidator <- tcont . plet $ onlyAtNodeVal # insAsOuts
  toNodeValidator <- tcont . plet $ onlyAtNodeVal # info.outputs
  ------------------------------

  let atNodeValidator =
        pelimList
          ( \x xs -> plet (paysToAddress # (pfield @"address" # x)) $ \isSameAddr ->
              pand'List
                [ (pall # isSameAddr # xs)
                , (pall # isSameAddr # toNodeValidator)
                ]
          )
          (pconstant True)
          fromNodeValidator

  pguardC "all same origin" atNodeValidator

  nodeInputs <- tcont . plet $ pmap # nodeInputUtxoDatumUnsafe #$ pconvertLists # fromNodeValidator

  nodeOutputs <-
    tcont . plet $
      pmap
        # (parseNodeOutputUtxo # ownCS)
        #$ pconvertLists
        # toNodeValidator

  let common =
        MkCommon
          { ownCS = (pfromData ownCS)
          , mint
          , nodeInputs
          , nodeOutputs
          }

  pure
    ( common
    , info.inputs
    , info.outputs
    , info.signatories
    , info.validRange
    )

pInit :: forall (s :: S). PStakingCommon s -> Term s PUnit
pInit common = P.do
  -- Input Checks
  passert "Init must not spend Nodes" $ pnull # common.nodeInputs
  -- Output Checks:
  PPair _ otherNodes <-
    pmatch $
      pfindWithRest # plam (\nodePair -> pmatch nodePair (\(PPair _ nodeDat) -> isEmptySet # nodeDat)) # common.nodeOutputs
  passert "Init output exactly one Node" $
    pnull # otherNodes
  -- Mint checks:
  passert "Incorrect mint for Init" $
    correctNodeTokenMinted # common.ownCS # poriginNodeTN # 1 # common.mint

  pconstant ()

-- TODO add deadline check
pDeinit :: forall s. PStakingCommon s -> Term s PUnit
pDeinit common = P.do
  -- Input Checks
  -- The following commented code should be used instead for protocols where node removal
  -- needs to preserve the integrity of the linked list.
  PPair _ otherNodes <- pmatch $ pfindWithRest # plam (\nodePair -> pmatch nodePair (\(PPair _ dat) -> isEmptySet # dat)) # common.nodeInputs
  -- PPair _ otherNodes <- pmatch $ pfindWithRest # plam (\nodePair -> pmatch nodePair (\(PPair _ dat) -> isNothing # (pfield @"key" # dat))) # common.nodeInputs
  passert "Deinit must spend exactly one node" $ pnull # otherNodes
  -- Output Checks:
  passert "Deinit must not output nodes" $ pnull # common.nodeOutputs

  -- Mint checks:
  passert "Incorrect mint for DeInit" $
    correctNodeTokenMinted # common.ownCS # poriginNodeTN # (-1) # common.mint

  pconstant ()

pInsert ::
  forall (s :: S).
  PStakingCommon s ->
  Term s (PAsData PPubKeyHash :--> PAsData PStakingSetNode :--> PUnit)
pInsert common = plam $ \pkToInsert node -> P.do
  keyToInsert <- plet . pto . pfromData $ pkToInsert
  passert "Node should cover inserting key" $
    coversKey # node # keyToInsert
  -- Input Checks
  PPair coveringNode otherNodes <-
    pmatch $
      pfindWithRest # plam (\nodePair -> pmatch nodePair (\(PPair _ dat) -> node #== dat)) # common.nodeInputs
  passert "Insert must spend exactly one node" $
    pnull # otherNodes
  -- Output Checks:
  PPair coveringValue _coveringDatum <- pmatch coveringNode
  prevNodeOutDatum <- plet $ pdata $ asPredecessorOf # node # keyToInsert
  let nodeOutDatum = pdata $ asSuccessorOf # keyToInsert # node

  hasDatumInOutputs <- plet $
    plam $ \datum ->
      pany # plam (\nodePair -> pmatch nodePair (\(PPair _ dat) -> datum #== dat)) # common.nodeOutputs

  passert "Incorrect node outputs for Insert" $
    pany
      # plam (\nodePair -> pmatch nodePair (\(PPair val dat) -> val #== coveringValue #&& prevNodeOutDatum #== dat))
      # common.nodeOutputs
      #&& hasDatumInOutputs
      # nodeOutDatum

  -- Mint checks:
  passert "Incorrect mint for Insert" $
    correctNodeTokenMinted # common.ownCS # (pnodeKeyTN # keyToInsert) # 1 # common.mint

  pconstant ()

pRemove ::
  forall (s :: S).
  PStakingCommon s ->
  Term s (PInterval PPOSIXTime) ->
  Term s PStakingConfig ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s (PAsData PPubKeyHash :--> PAsData PStakingSetNode :--> PUnit)
pRemove common vrange discConfig outs sigs = plam $ \pkToRemove node -> P.do
  keyToRemove <- plet . pto . pfromData $ pkToRemove
  passert "Node does not cover key to remove" $
    coversKey # node # keyToRemove
  -- Input Checks
  let prevNodeInDatum = pdata $ asPredecessorOf # node # keyToRemove
      nodeInDatum = pdata $ asSuccessorOf # keyToRemove # node
  findNodeInDatumInRest <- plet $
    plam $ \datum inputs ->
      pfindWithRest
        # plam (\nodePair -> pmatch nodePair (\(PPair _ dat) -> datum #== dat))
        # inputs
  PPair stayNode rest <- pmatch $ findNodeInDatumInRest # prevNodeInDatum # common.nodeInputs
  PPair removedNode extraNodes <- pmatch $ findNodeInDatumInRest # nodeInDatum # rest
  passert "Remove must spend exactly two nodes" $
    pnull # extraNodes

  nodeToRemoveTN <- plet (pnodeKeyTN # keyToRemove)

  -- Output Checks:

  PPair stayValue _stayDatum <- pmatch stayNode
  PPair removedValue _removedDatum <- pmatch removedNode
  {- This check has weak constraints due to the fact that the only way
    To provide more node outputs would be to mint more node tokens.
    Therefore we can safely assure that this is the only node Output.

    Error is more explicit simply for debugging
  -}
  passert "There must be exactly one output with update node" $
    pany # plam (\nodePair -> pmatch nodePair (\(PPair val dat) -> node #== dat #&& stayValue #== val)) # common.nodeOutputs

  passert "Incorrect mint for Remove" $
    correctNodeTokenMinted # common.ownCS # nodeToRemoveTN # (-1) # common.mint

  passert "signed by user." (pelem # pkToRemove # sigs)

  configF <- pletFields @'["stakingDeadline", "penaltyAddress"] discConfig

  let ownInputLovelace = plovelaceValueOf # removedValue
      ownInputFee = pdivideCeil # ownInputLovelace # 4
      discDeadline = configF.stakingDeadline

  let finalCheck =
        -- user committing before deadline
        ( pif
            (pafter # (discDeadline - 24 * 60 * 60 * 1000) # vrange) -- user committing before 24 hours before deadline
            (pconstant True)
            ( pany
                # plam
                  ( \out ->
                      pfield @"address"
                        # out
                        #== configF.penaltyAddress
                        #&& ownInputFee
                        #<= plovelaceValueOf
                        # (pfield @"value" # out)
                  )
                # outs -- must pay 25% fee
            )
        )

  passert "Removal broke Phase Rules." finalCheck

  pconstant ()

pClaim ::
  forall (s :: S).
  PStakingCommon s ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s (PAsData PPubKeyHash :--> PUnit)
pClaim common outs sigs = plam $ \pkToRemove -> P.do
  keyToRemove <- plet . pto . pfromData $ pkToRemove

  -- Input Checks
  PPair removedValue _removedDatum <- pmatch (pheadSingleton # common.nodeInputs)

  nodeToRemoveTN <- plet (pnodeKeyTN # keyToRemove)

  passert "Incorrect node UTxO for Remove" $
    pvalueOf # removedValue # common.ownCS # nodeToRemoveTN #== 1

  passert "Incorrect mint for Remove" $
    correctNodeTokenMinted # common.ownCS # nodeToRemoveTN # (-1) # common.mint

  passert "signed by user." (pelem # pkToRemove # sigs)

  -- verify that this node has been processed by the rewards fold by checking that count of tokens is 3.
  passert "Claim broke phase rules." (pcountOfUniqueTokens # removedValue #>= 3)

  pconstant ()

-- Common information shared between all redeemers.
data PStakingCommon (s :: S) = MkCommon
  { ownCS :: Term s PCurrencySymbol
  -- ^ state token (own) CS
  , mint :: Term s (PValue 'Sorted 'NonZero)
  -- ^ value minted in current Tx
  , nodeInputs :: Term s (PList (PPair (PValue 'Sorted 'Positive) (PAsData PStakingSetNode)))
  -- ^ current Tx outputs to AuctionValidator
  , nodeOutputs :: Term s (PList (PPair (PValue 'Sorted 'Positive) (PAsData PStakingSetNode)))
  -- ^ current Tx inputs
  }
  deriving stock (Generic)
