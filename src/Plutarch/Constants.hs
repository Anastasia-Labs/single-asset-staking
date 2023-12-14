{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Plutarch.Constants where

import Plutarch
import Plutarch.Api.V1 (PTokenName (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Utils (passert, pisPrefixOf)
import PlutusLedgerApi.V1 (TokenName)

poriginNodeTN :: Term s PTokenName
poriginNodeTN =
  let tn :: TokenName
      tn = "FSN"
   in pconstant tn

psetNodePrefix :: ClosedTerm PByteString
psetNodePrefix = pconstant "FSN"

pnodeKeyTN :: ClosedTerm (PByteString :--> PTokenName)
pnodeKeyTN = phoistAcyclic $
  plam $
    \nodeKey -> pcon $ PTokenName $ psetNodePrefix <> nodeKey

pparseNodeKey :: ClosedTerm (PTokenName :--> PMaybe PByteString)
pparseNodeKey = phoistAcyclic $
  plam $ \(pto -> tn) -> P.do
    let prefixLength = 3
        tnLength = plengthBS # tn
        key = psliceBS # prefixLength # (tnLength - prefixLength) # tn
    passert "incorrect node prefix" $ pisPrefixOf # psetNodePrefix # tn
    pif (prefixLength #< tnLength) (pcon $ PJust key) (pcon PNothing)

nodeAda :: Term s PInteger
nodeAda = pconstant 3_000_000

minCommitment :: Term s PInteger
minCommitment = pconstant 4_000_000
