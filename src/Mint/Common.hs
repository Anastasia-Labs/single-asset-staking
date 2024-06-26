{-# OPTIONS_GHC -Wno-unused-do-bind #-}
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
  coversKey,
 )
import Plutarch.Api.V1.Value (PTokenName, plovelaceValueOf, pnormalize, pvalueOf)
import Plutarch.Api.V2 (
  AmountGuarantees (..),
  KeyGuarantees (..),
  PCurrencySymbol,
  PInterval,
  POutputDatum (..),
  PPOSIXTime,
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose (PMinting),
  PTxInInfo,
  PTxOut,
  PValue,
 )
import Plutarch.Extra.Interval (pafter)
import Plutarch.Extra.ScriptContext (pfromPDatum, ptryFromInlineDatum)
import Plutarch.List (pconvertLists)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Types.Constants (nodeAda, pnodeKeyTN, poriginNodeTN, pparseNodeKey)
import Types.StakingSet (
  PNodeKey (..),
  PStakingConfig,
  PStakingSetNode,
  asPredecessorOf,
  asSuccessorOf,
  isEmptySet,
  isNothing,
  validNode,
 )
import Utils (
  pand'List,
  passert,
  paysToAddress,
  pcheck,
  pcountOfUniqueTokens,
  pfindCurrencySymbolsByTokenPrefix,
  pfindWithRest,
  phasDataCS,
  pheadSingleton,
  psingletonOfCS,
  (#/=),
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

parseNodeOutputUtxo ::
  ClosedTerm
    ( PStakingConfig
        :--> PTokenName
        :--> PAsData PCurrencySymbol
        :--> PTxOut
        :--> PPair (PValue 'Sorted 'Positive) (PAsData PStakingSetNode)
    )
parseNodeOutputUtxo = phoistAcyclic $
  plam $ \config configTN nodeCS out -> P.do
    configF <- pletFields @'["stakeCS", "stakeTN", "minimumStake"] config
    txOut <- pletFields @'["address", "value", "datum"] out
    value <- plet $ pfromData $ txOut.value
    PPair tn amount <- pmatch $ psingletonOfCS # nodeCS # value
    POutputDatum od <- pmatch $ pfromData $ txOut.datum
    datum <- plet $ pfromPDatum #$ pfield @"outputDatum" # od
    datumF <- pletFields @'["key", "configTN"] datum
    let nodeKey = pparseNodeKey # tn
        datumKey = pmatch datumF.key $ \case
          PEmpty _ -> pcon PNothing
          PKey ((pfield @"_0" #) -> key) -> pcon $ PJust key

    -- Prevents TokenDust attack
    passert "All FSN tokens from node policy" $
      pheadSingleton # (pfindCurrencySymbolsByTokenPrefix # value # pconstant "FSN") #== nodeCS
    passert "Incorrect ConfigTN" $ configTN #== datumF.configTN
    passert "Insufficient stake" $
      pvalueOf # value # configF.stakeCS # configF.stakeTN #>= configF.minimumStake
    passert "Too many assets" $ pcountOfUniqueTokens # value #== 3
    passert "Incorrect number of nodeTokens" $ amount #== 1
    passert "node is not ordered" $ validNode # datum
    passert "Incorrect token name" $ nodeKey #== datumKey
    passert "ADA value must be nodeAda value" $
      plovelaceValueOf # value #== nodeAda
    -- todo maxStakeCommitment?
    pcon (PPair value datum)

parseNodeInputUtxo ::
  ClosedTerm
    ( PTokenName
        :--> PTxOut
        :--> PPair (PValue 'Sorted 'Positive) (PAsData PStakingSetNode)
    )
parseNodeInputUtxo = phoistAcyclic $
  plam $ \configTN input -> P.do
    inputF <- pletFields @'["value", "datum"] input
    datum <- plet $ pfromPDatum #$ ptryFromInlineDatum # inputF.datum

    passert "Incorrect ConfigTN" $ configTN #== (pfield @"configTN" # datum)

    pcon $ PPair inputF.value datum

makeCommon ::
  forall {r :: PType} {s :: S}.
  Term s PStakingConfig ->
  Term s PTokenName ->
  Term s PScriptContext ->
  TermCont @r
    s
    ( PStakingCommon s
    , Term s (PBuiltinList PTxInInfo)
    , Term s (PBuiltinList PTxOut)
    , Term s (PBuiltinList (PAsData PPubKeyHash))
    , Term s (PInterval PPOSIXTime)
    )
makeCommon config configTN ctx' = do
  -- Preparing info needed for validation
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
  onlyAtNodeVal <- tcont . plet $ pfilter # plam (\utxo -> (hasNodeTk # (pfield @"value" # utxo)))
  fromNodeValidator <- tcont . plet $ onlyAtNodeVal # insAsOuts
  toNodeValidator <- tcont . plet $ onlyAtNodeVal # info.outputs

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

  nodeInputs <-
    tcont . plet $
      pmap
        # (parseNodeInputUtxo # configTN)
        #$ pconvertLists
        # fromNodeValidator

  nodeOutputs <-
    tcont . plet $
      pmap
        # (parseNodeOutputUtxo # config # configTN # ownCS)
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

pDeinit :: forall s. PStakingCommon s -> Term s PStakingConfig -> Term s (PBuiltinList PTxOut) -> Term s PUnit
pDeinit common config outs = P.do
  -- Input Checks
  -- The following commented code should be used instead for protocols where node removal
  -- needs to preserve the integrity of the linked list.
  -- PPair _ otherNodes <- pmatch $ pfindWithRest # plam (\nodePair -> pmatch nodePair (\(PPair _ dat) -> isEmptySet # dat)) # common.nodeInputs
  PPair headNode otherNodes <- pmatch $ pfindWithRest # plam (\nodePair -> pmatch nodePair (\(PPair _ dat) -> isNothing # (pfield @"key" # dat))) # common.nodeInputs
  passert "Deinit must spend exactly one node" $ pnull # otherNodes

  PPair nodeValue _ <- pmatch headNode
  {- Folding fee taken from head node during Reward Fold Init. nodeAda - foldingFee != nodeAda
     Once reward fold has started head node is not required. Claim can be processed for a node
     after it has undergone rewards fold (with the same minAda check) without any need to preserve
     integrity of the linked list (i.e without modifying the covering node).
  -}
  passert "Rewards fold has not started yet" $ plovelaceValueOf # nodeValue #/= nodeAda

  -- Output Checks:
  passert "Deinit must not output nodes" $ pnull # common.nodeOutputs
  -- Mint checks:
  passert "Incorrect mint for DeInit" $
    correctNodeTokenMinted # common.ownCS # poriginNodeTN # (-1) # common.mint

  configF <- pletFields @'["penaltyAddress", "stakeCS", "stakeTN"] config

  let stakeAmount = plam $ \val -> pvalueOf # val # configF.stakeCS # configF.stakeTN

  let returnsStake = plam $ \out ->
        pletFields @["address", "value"] out $ \outF ->
          outF.address
            #== configF.penaltyAddress
            #&& stakeAmount
            # nodeValue
            #<= stakeAmount
            # outF.value

  passert "Deinit must return stake back to penalty address" (pany # returnsStake # outs)

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
pRemove common vrange config outs sigs = plam $ \pkToRemove node -> P.do
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
  passert "There must be exactly one output with updated node" $
    pany # plam (\nodePair -> pmatch nodePair (\(PPair val dat) -> node #== dat #&& stayValue #== val)) # common.nodeOutputs

  passert "Incorrect mint for Remove" $
    correctNodeTokenMinted # common.ownCS # nodeToRemoveTN # (-1) # common.mint

  passert "Incorrect signature" (pelem # pkToRemove # sigs)

  configF <- pletFields @'["freezeStake", "penaltyAddress", "stakeCS", "stakeTN"] config

  let ownInputStake = pvalueOf # removedValue # configF.stakeCS # configF.stakeTN
      ownInputFee = pdivideCeil # ownInputStake # 4 -- Penalty fee is 25% of stake
      penaltyFeePaid = plam $ \out ->
        pfield @"address"
          # out
          #== configF.penaltyAddress
          #&& ownInputFee
          #<= pvalueOf
          # (pfield @"value" # out)
          # configF.stakeCS
          # configF.stakeTN

  let finalCheck =
        -- check if user is unstaking before or after freezeStake
        ( pif
            (pafter # configF.freezeStake # vrange) -- user unstaking before stake is frozen
            (pconstant True)
            (pany # penaltyFeePaid # outs) -- must pay 25% penalty fee
        )

  passert "Removal broke Phase Rules." finalCheck

  pconstant ()

pClaim ::
  forall (s :: S).
  PStakingCommon s ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s (PAsData PPubKeyHash) ->
  Term s PUnit
pClaim common sigs pkToRemove = P.do
  keyToRemove <- plet . pto . pfromData $ pkToRemove

  -- Input Checks
  PPair removedValue _removedDatum <- pmatch (pheadSingleton # common.nodeInputs)

  nodeToRemoveTN <- plet (pnodeKeyTN # keyToRemove)

  passert "Incorrect node UTxO for Remove" $ pvalueOf # removedValue # common.ownCS # nodeToRemoveTN #== 1

  passert "Incorrect mint for Remove" $
    correctNodeTokenMinted # common.ownCS # nodeToRemoveTN # (-1) # common.mint

  passert "Incorrect signature" (pelem # pkToRemove # sigs)

  {-
    Verify that this node has been processed by the rewards fold.
    This cannot be accomplished by checking count of unique tokens in the node,
    as it would fail in the scenario when stake token is same as reward token.
    Check if the node has paid folding fee to confirm it has undergone rewards fold.
    nodeAda - foldingFee != nodeAda
  -}
  passert "Claim broke phase rules" (plovelaceValueOf # removedValue #/= nodeAda)

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
