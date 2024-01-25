{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module RewardTokenHolder (
  prewardTokenHolder,
  pmintRewardTokenHolder,
) where

import Conversions (pconvert)
import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Api.V2 (
  PCurrencySymbol,
  PMintingPolicy,
  PScriptContext,
  PScriptPurpose (PMinting, PSpending),
  PTxOutRef,
  PValidator,
 )
import Plutarch.Prelude
import Types.Constants (rewardTokenHolderTN)
import Utils (pand'List, pfindCurrencySymbolsByTokenName, phasInput, pheadSingleton, ptryLookupValue, ptryOwnInput)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
 )

data PTokenHolderMintAct (s :: S)
  = PMintHolder (Term s (PDataRecord '[]))
  | PBurnHolder (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PTokenHolderMintAct where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PTokenHolderMintAct

instance PTryFrom PData (PAsData PTokenHolderMintAct)

pmintRewardTokenHolder :: Term s (PTxOutRef :--> PMintingPolicy)
pmintRewardTokenHolder = phoistAcyclic $
  plam $ \oref redm ctx ->
    let red = pconvert @PTokenHolderMintAct redm
     in pmatch red $ \case
          PMintHolder _ -> popaque $ pmintTokenHolder # oref # ctx
          PBurnHolder _ -> popaque $ pburnTokenHolder # ctx

pburnTokenHolder :: Term s (PScriptContext :--> PUnit)
pburnTokenHolder = phoistAcyclic $
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

pmintTokenHolder :: Term s (PTxOutRef :--> PScriptContext :--> PUnit)
pmintTokenHolder = phoistAcyclic $
  plam $ \oref ctx -> unTermCont $ do
    contextFields <- pletFieldsC @'["txInfo", "purpose"] ctx
    PMinting policy <- pmatchC contextFields.purpose
    ownPolicyId <- pletC $ pfield @"_0" # policy

    info <- pletFieldsC @'["inputs", "mint"] contextFields.txInfo
    tkPairs <- pletC $ ptryLookupValue # ownPolicyId # (pnormalize # info.mint)
    tkPair <- pletC (pheadSingleton # tkPairs)
    let numMinted = psndBuiltin # tkPair
        tkMinted = pfstBuiltin # tkPair
        mintChecks =
          pand'List
            [ pfromData numMinted #== 1
            , rewardTokenHolderTN #== pfromData tkMinted
            , phasInput # info.inputs # oref
            ]
    pure $
      pif (mintChecks) (pconstant ()) perror

prewardTokenHolder :: Term s (PAsData PCurrencySymbol :--> PValidator)
prewardTokenHolder = phoistAcyclic $ plam $ \rewardFoldCS _dat _redeemer ctx -> unTermCont $ do
  ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
  infoF <- pletFieldsC @'["inputs", "mint"] ctxF.txInfo

  PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose
  let ownInput = ptryOwnInput # infoF.inputs # ownRef
  ownInputF <- pletFieldsC @'["value"] ownInput

  mintedValue <- pletC (pnormalize # infoF.mint)
  let possibleCSs = pfindCurrencySymbolsByTokenName # ownInputF.value # rewardTokenHolderTN
      pthCS = pheadSingleton # possibleCSs

  let tkhPairs = ptryLookupValue # pthCS # mintedValue
      tkhPair = (pheadSingleton # tkhPairs)
      thkMinted = psndBuiltin # tkhPair

  tkPairs <- pletC $ ptryLookupValue # rewardFoldCS # mintedValue
  tkPair <- pletC (pheadSingleton # tkPairs)
  let rewardTkMinted = psndBuiltin # tkPair

  pure $
    pif (pfromData rewardTkMinted #== 1 #&& pfromData thkMinted #== -1) (popaque $ pconstant ()) perror
