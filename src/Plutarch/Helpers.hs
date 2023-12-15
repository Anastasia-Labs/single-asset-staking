module Plutarch.Helpers (
  coversKey,
  hasUtxoWithRef,
  correctNodeTokenMinted,
)
where

import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2 (
  AmountGuarantees (NonZero),
  KeyGuarantees (Sorted),
  PCurrencySymbol,
  PTokenName,
  PTxInInfo,
  PTxOutRef,
  PValue,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Types (PDiscoverySetNode, PNodeKey (PEmpty, PKey))

-- | Checks that key is 'covered' by the Node
coversKey :: ClosedTerm (PAsData PDiscoverySetNode :--> PByteString :--> PBool)
coversKey = phoistAcyclic $
  plam $ \datum keyToCover -> P.do
    nodeDatum <- pletFields @'["key", "next"] datum
    let moreThanKey = pmatch (nodeDatum.key) $ \case
          PEmpty _ -> pcon PTrue
          PKey (pfromData . (pfield @"_0" #) -> key) -> key #< keyToCover
        lessThanNext = pmatch (nodeDatum.next) $ \case
          PEmpty _ -> pcon PTrue
          PKey (pfromData . (pfield @"_0" #) -> next) -> keyToCover #< next
    moreThanKey #&& lessThanNext

{- | @hasUtxoWithRef # oref # inputs@
 ensures that in @inputs@ there is an input having @TxOutRef@ @oref@ .
-}
hasUtxoWithRef ::
  ClosedTerm
    ( PTxOutRef
        :--> PBuiltinList PTxInInfo
        :--> PBool
    )
hasUtxoWithRef = phoistAcyclic $
  plam $ \oref inInputs ->
    pany # plam (\input -> oref #== (pfield @"outRef" # input)) # inInputs

{- | Ensures that the minted amount of the FinSet CS is exactly the specified
    tokenName and amount
-}
correctNodeTokenMinted ::
  ClosedTerm
    ( PCurrencySymbol
        :--> PTokenName
        :--> PInteger
        :--> PValue 'Sorted 'NonZero
        :--> PBool
    )
correctNodeTokenMinted = phoistAcyclic $
  plam $ \nodeCS tokenName amount mint -> P.do
    PJust nodeMint <- pmatch $ AssocMap.plookup # nodeCS # pto mint
    let tokenMap = AssocMap.psingleton # tokenName # amount
    tokenMap #== nodeMint
