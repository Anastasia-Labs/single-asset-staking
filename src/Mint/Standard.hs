{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Mint.Standard (
  mkStakingNodeMP,
  mkStakingNodeMPW,
) where

import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptContext,
  PTxOutRef,
 )
import Plutarch.Extra.Interval (pafter, pbefore)

import Mint.Common (
  PStakingCommon (mint, ownCS),
  makeCommon,
  pClaim,
  pDeinit,
  pInit,
  pInsert,
  pRemove,
 )
import Mint.Helpers (
  hasUtxoWithRef,
 )
import Plutarch.Internal (Config (..))
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)

import Plutarch.Prelude
import Types.StakingSet (PStakingConfig (..), PStakingNodeAction (..))
import Utils (pand'List, passert, pcond)

--------------------------------
-- FinSet Node Minting Policy:
--------------------------------

mkStakingNodeMP ::
  ClosedTerm
    ( PStakingConfig
        :--> PStakingNodeAction
        :--> PScriptContext
        :--> PUnit
    )
mkStakingNodeMP = plam $ \discConfig redm ctx -> P.do
  configF <- pletFields @'["initUTxO"] discConfig

  (common, inputs, outs, sigs, vrange) <-
    runTermCont $
      makeCommon ctx

  pmatch redm $ \case
    PInit _ -> P.do
      passert "Init must consume TxOutRef" $
        hasUtxoWithRef # configF.initUTxO # inputs
      pInit common
    PDeinit _ ->
      -- TODO deinit must check that reward fold has been completed
      pDeinit common
    PInsert action -> P.do
      act <- pletFields @'["keyToInsert", "coveringNode"] action
      let insertChecks =
            pand'List
              [ pafter # (pfield @"stakingDeadline" # discConfig) # vrange
              , pelem # act.keyToInsert # sigs
              ]
      pif insertChecks (pInsert common # act.keyToInsert # act.coveringNode) perror
    PRemove action -> P.do
      configF <- pletFields @'["stakingDeadline"] discConfig
      act <- pletFields @'["keyToRemove", "coveringNode"] action
      discDeadline <- plet configF.stakingDeadline
      pcond
        [ ((pbefore # discDeadline # vrange), (pClaim common outs sigs # act.keyToRemove))
        , ((pafter # discDeadline # vrange), (pRemove common vrange discConfig outs sigs # act.keyToRemove # act.coveringNode))
        ]
        perror

mkStakingNodeMPW ::
  ClosedTerm
    ( PStakingConfig
        :--> PMintingPolicy
    )
mkStakingNodeMPW = phoistAcyclic $ plam $ \discConfig redm ctx ->
  let red = punsafeCoerce @_ @_ @PStakingNodeAction redm
   in popaque $ mkStakingNodeMP # discConfig # red # ctx
