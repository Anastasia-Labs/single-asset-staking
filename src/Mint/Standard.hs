{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Mint.Standard (
  mkStakingNodeMP,
  mkStakingNodeMPW,
) where

import Plutarch.Api.V2 (
  PCurrencySymbol,
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
import Types.StakingSet (PStakingNodeAction (..))
import Utils (fetchConfigDetails, pand'List, passert)

--------------------------------
-- Staking Node Minting Policy
--------------------------------

mkStakingNodeMP ::
  ClosedTerm
    ( PCurrencySymbol
        :--> PStakingNodeAction
        :--> PScriptContext
        :--> PUnit
    )
mkStakingNodeMP = plam $ \configCS redm ctx -> P.do
  ctxF <- pletFields @'["txInfo"] ctx
  PPair configTN config <- pmatch $ fetchConfigDetails # configCS # (pfield @"referenceInputs" # ctxF.txInfo)

  configF <- pletFields @'["stakingInitUTxO", "freezeStake", "endStaking"] config

  (common, inputs, outs, sigs, vrange) <-
    runTermCont $
      makeCommon config configTN ctx

  pmatch redm $ \case
    PInit _ -> P.do
      passert "Init must consume TxOutRef" $
        hasUtxoWithRef # configF.stakingInitUTxO # inputs
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
    ( PCurrencySymbol
        :--> PMintingPolicy
    )
mkStakingNodeMPW = phoistAcyclic $ plam $ \configCS redm ctx ->
  let red = pconvert @PStakingNodeAction redm
   in popaque $ mkStakingNodeMP # configCS # red # ctx
