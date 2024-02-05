{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Mint.Standard (
  mkStakingNodeMP,
  mkStakingNodeMPW,
) where

import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptContext,
 )
import Plutarch.Extra.Interval (pafter, pbefore)

import Mint.Common (
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
import Plutarch.Monadic qualified as P

import Conversions (pconvert)
import Plutarch.Prelude
import Types.StakingSet (PStakingConfig (..), PStakingNodeAction (..))
import Utils (pand'List, passert)

--------------------------------
-- Staking Node Minting Policy
--------------------------------

mkStakingNodeMP ::
  ClosedTerm
    ( PStakingConfig
        :--> PStakingNodeAction
        :--> PScriptContext
        :--> PUnit
    )
mkStakingNodeMP = plam $ \config redm ctx -> P.do
  configF <- pletFields @'["initUTxO", "freezeStake", "endStaking"] config

  (common, inputs, outs, sigs, vrange) <-
    runTermCont $
      makeCommon config ctx

  pmatch redm $ \case
    PInit _ -> P.do
      passert "Init must consume TxOutRef" $
        hasUtxoWithRef # configF.initUTxO # inputs
      pInit common
    PDeinit _ ->
      pDeinit common
    PInsert action -> P.do
      act <- pletFields @'["keyToInsert", "coveringNode"] action
      let insertChecks =
            pand'List
              [ pafter # configF.freezeStake # vrange
              , pelem # act.keyToInsert # sigs
              ]
      pif insertChecks (pInsert common # act.keyToInsert # act.coveringNode) perror
    PRemove action -> P.do
      act <- pletFields @'["keyToRemove", "coveringNode"] action
      passert "Cannot remove after endStaking" (pafter # configF.endStaking # vrange)
      pRemove common vrange config outs sigs # act.keyToRemove # act.coveringNode
    PClaim action -> P.do
      act <- pletFields @'["keyToRemove"] action
      passert "Cannot claim before endStaking" (pbefore # configF.endStaking # vrange)
      pClaim common sigs act.keyToRemove

mkStakingNodeMPW ::
  ClosedTerm
    ( PStakingConfig
        :--> PMintingPolicy
    )
mkStakingNodeMPW = phoistAcyclic $ plam $ \config redm ctx ->
  let red = pconvert @PStakingNodeAction redm
   in popaque $ mkStakingNodeMP # config # red # ctx
