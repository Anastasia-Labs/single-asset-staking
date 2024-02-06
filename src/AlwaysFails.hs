{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module AlwaysFails (pAlwaysFails) where

import Plutarch.Api.V2
import Plutarch.Prelude

pAlwaysFails ::
  ClosedTerm PValidator
pAlwaysFails = plam $ \_dat _redmn _ctx' -> popaque $ perror
