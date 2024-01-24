{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Validator (
  pStakingSetValidator,
  pDiscoverGlobalLogicW,
) where

import Data.ByteString (ByteString)

import Conversions
import Plutarch.Api.V1 (
  PCredential (..),
  PPubKeyHash (PPubKeyHash),
 )
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2 (
  PCurrencySymbol (..),
  POutputDatum (..),
  PScriptPurpose (PSpending),
  PStakeValidator,
  PValidator,
 )
import Plutarch.Extra.Interval (pafter)
import Plutarch.Extra.ScriptContext (pfromPDatum)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Types.StakingSet (PNodeKey (..), PNodeValidatorAction (..), PStakingLaunchConfig (..), PStakingSetNode (..))
import Utils (passert, pcontainsCurrencySymbols, pfilterCSFromValue, pfindCurrencySymbolsByTokenPrefix, phasCS, ptryOwnInput, ptryOwnOutput, (#>=))

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
  ByteString ->
  ClosedTerm (PStakingLaunchConfig :--> PValidator)
pStakingSetValidator prefix = plam $ \config dat red ctx' ->
  let redeemer = pconvert @PNodeValidatorAction red
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
          info <- pletFields @'["inputs", "outputs", "mint", "validRange", "signatories"] ctx.txInfo
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
            PModifyStake _ -> P.do
              PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatch (pfield @"credential" # ownInputF.address)
              configF <- pletFields @'["freezeStake", "stakeCS", "stakeTN", "minimumStake"] config
              PKey ((pfield @"_0" #) -> ownerHash) <- pmatch (pfield @"key" # oldDatum)

              let ownOutput = ptryOwnOutput # info.outputs # ownValHash
              ownOutputF <- pletFields @'["value", "datum"] ownOutput
              POutputDatum ((pfield @"outputDatum" #) -> ownOutputDatum) <- pmatch ownOutputF.datum

              let
                newDatum = pfromPDatum @PStakingSetNode # ownOutputDatum
                newStake = pvalueOf # ownOutputF.value # configF.stakeCS # configF.stakeTN
                inputWithoutStake = pfilterCSFromValue # ownInputF.value # configF.stakeCS
                outputWithoutStake = pfilterCSFromValue # ownOutputF.value # configF.stakeCS

              passert "Cannot change datum when modifying stake" (newDatum #== oldDatum)
              passert "Cannot modify anything except stake" (inputWithoutStake #== outputWithoutStake)
              passert "Insufficient stake" (newStake #>= configF.minimumStake)
              passert "No tokens should be minted" (pfromData info.mint #== mempty)
              passert "Stake has been frozen" ((pafter # (pfromData configF.freezeStake) # info.validRange))
              passert "Incorrect signature" (pelem # (pdata $ pcon $ PPubKeyHash ownerHash) # info.signatories)
              (popaque $ pconstant ())
