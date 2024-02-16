{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Config (
  pmintConfigToken,
) where

import Conversions (pconvert)
import Plutarch.Api.V1.Value (pnormalize, pvalueOf, PTokenName (PTokenName))
import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptPurpose (PMinting),
  PTxOutRef, PAddress,
 )
import Plutarch.Prelude
import Utils (pand'List, phasInput, pheadSingleton, ptryLookupValue)
import Plutarch.Monadic qualified as P
import Types.StakingSet (PStakingConfig)
import Plutarch.Extra.ScriptContext (pfromPDatum, ptryFromInlineDatum)

pmintConfigToken :: Term s (PAddress :--> PMintingPolicy)
pmintConfigToken = phoistAcyclic $
  plam $ \address red ctx -> P.do
    let oref = pconvert @PTxOutRef red
    ctxF <- pletFields @'["txInfo", "purpose"] ctx
    infoF <- pletFields @'["inputs", "outputs", "mint"] ctxF.txInfo

    PMinting ((pfield @"_0" #) -> ownPolicyId) <- pmatch ctxF.purpose
    tkPairs <- plet $ ptryLookupValue # ownPolicyId # (pnormalize # infoF.mint)
    tkPair <- plet (pheadSingleton # tkPairs)

    orefF <- pletFields @'["id", "idx"] oref

    let tnMinted = pfromData $ pfstBuiltin # tkPair
        numMinted = pfromData $ psndBuiltin # tkPair
        -- Expected token name is a hash of concatenated TxIdx and TxId
        calculatedTN = pcon $ PTokenName $ psha3_256 # (pconsBS # orefF.idx #$ pfield @"_0" # orefF.id)
        configOutput =
          pheadSingleton
            # ( pfilter @PBuiltinList
                  # plam (\out -> pvalueOf # (pfield @"value" # out) # pfromData ownPolicyId # calculatedTN #== 1)
                  # infoF.outputs
              )
    
    configOutputF <- pletFields @'["address", "datum"] configOutput
    -- Check that an inline datum of type PStakingConfig is present in Config Output
    let _configDatum = pfromPDatum @PStakingConfig # (ptryFromInlineDatum # configOutputF.datum)

    pif 
      (pand'List [ptraceIfFalse "Can only mint one config token" $ numMinted #== 1
      , ptraceIfFalse "Incorrect Token Name" $ tnMinted #== calculatedTN
      , ptraceIfFalse "Incorrect Config Output Address" $ configOutputF.address #== address
      , ptraceIfFalse "Required Input Missing" $ phasInput # infoF.inputs # oref
      ]) 
      (popaque $ pconstant ()) 
      perror