{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Validator (
  pStakingSetValidator,
  pDiscoverGlobalLogicW,
) where

import Data.ByteString (ByteString)

import Conversions
import Plutarch (Config)
import Plutarch.Api.V1 (
  PCredential (..),
 )
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Value (PValue (..), plovelaceValueOf, pnoAdaValue, pvalueOf)
import Plutarch.Api.V2 (
  PCurrencySymbol (..),
  POutputDatum (..),
  PScriptPurpose (PSpending),
  PStakeValidator,
  PValidator,
 )
import Plutarch.Extra.Interval (pafter, pbefore)
import Plutarch.Extra.ScriptContext (pfromPDatum)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Types.Constants (rewardFoldTN)
import Types.StakingSet (PNodeValidatorAction (..), PStakingLaunchConfig (..), PStakingSetNode (..))
import Utils (passert, pcontainsCurrencySymbols, pfindCurrencySymbolsByTokenPrefix, phasCS, pheadSingleton, ptryOwnInput, ptryOwnOutput)

pDiscoverGlobalLogicW :: Term s (PAsData PCurrencySymbol :--> PStakeValidator)
pDiscoverGlobalLogicW = phoistAcyclic $ plam $ \rewardFoldCS' _redeemer ctx -> P.do
  -- let rewardsIdx = pconvert @(PAsData PInteger) redeemer
  ctxF <- pletFields @'["txInfo"] ctx
  infoF <- pletFields @'["inputs"] ctxF.txInfo
  rewardFoldCS <- plet $ pfromData rewardFoldCS'
  let hasFoldToken = pany @PBuiltinList # plam (\inp -> phasCS # (pfield @"value" # (pfield @"resolved" # inp)) # rewardFoldCS) # infoF.inputs
  -- let rewardInp = pelemAt @PBuiltinList # pfromData rewardsIdx # infoF.inputs
  --     hasFoldToken = pvalueOf # (pfield @"value" # (pfield @"resolved" # rewardInp)) # pfromData rewardFoldCS # rewardFoldTN #== 1
  pif hasFoldToken (popaque $ pconstant ()) perror

pStakingSetValidator ::
  Config ->
  ByteString ->
  ClosedTerm (PStakingLaunchConfig :--> PValidator)
pStakingSetValidator cfg prefix = plam $ \config dat redmn ctx' ->
  let redeemer = pconvert @PNodeValidatorAction redmn
      oldDatum = pconvert @PStakingSetNode dat
   in pmatch redeemer $ \case
        PRewardFoldAct _ ->
          let stakeCerts = pfield @"wdrl" # (pfield @"txInfo" # ctx')
              stakeScript = pfromData $ pfield @"globalCred" # config
           in pmatch (AssocMap.plookup # stakeScript # stakeCerts) $ \case
                PJust _ -> (popaque $ pconstant ())
                PNothing -> perror
        otherRedeemers -> P.do
          ctx <- pletFields @'["txInfo", "purpose"] ctx'
          info <- pletFields @'["inputs", "outputs", "mint", "validRange"] ctx.txInfo
          txInputs <- plet info.inputs

          let ownInput = P.do
                PSpending ((pfield @"_0" #) -> ref) <- pmatch ctx.purpose
                ptryOwnInput # txInputs # ref
          ownInputF <- pletFields @'["value", "address"] ownInput

          let ownInputValue = pfromData ownInputF.value
              -- all those CSs has tokens that prefixed by Node prefix
              -- any of those can be actual Node CS
              potentialNodeCSs = pfindCurrencySymbolsByTokenPrefix # ownInputValue # pconstant prefix

          case otherRedeemers of
            PLinkedListAct _ -> P.do
              -- TODO: Currently launchpad token cannot start with FSN
              passert
                "Must mint/burn for any StakingSet input"
                (pcontainsCurrencySymbols # pfromData info.mint # potentialNodeCSs)
              (popaque $ pconstant ())
            PModifyCommitment _ -> P.do
              PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatch (pfield @"credential" # ownInputF.address)
              configF <- pletFields @'["stakingDeadline"] config 
              let ownOutput = ptryOwnOutput # info.outputs # ownValHash
              ownOutputF <- pletFields @'["value", "datum"] ownOutput
              POutputDatum ((pfield @"outputDatum" #) -> ownOutputDatum) <- pmatch ownOutputF.datum
              let newDatum = pfromPDatum @PStakingSetNode # ownOutputDatum
              passert "Cannot modify datum when committing" (newDatum #== oldDatum)
              passert "Cannot modify non-ada value" (pnoAdaValue # ownInputF.value #== pnoAdaValue # ownOutputF.value) -- todo non-ada value except stake-token
              passert "Cannot reduce ada value" (plovelaceValueOf # ownInputF.value #< plovelaceValueOf # ownOutputF.value + 10_000_000) -- todo all value except stake-token to remain same
              passert "No tokens minted" (pfromData info.mint #== mempty)
              passert "deadline passed" ((pafter # (pfromData configF.stakingDeadline - 86_400) # info.validRange))
              (popaque $ pconstant ())
