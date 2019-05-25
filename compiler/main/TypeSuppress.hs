module TypeSuppress where

import GhcPrelude

import Lens

-- TODO consistent "print" or "suppress"
data TypeSuppress = TypeSuppress
  { typeSuppress_printEqualityRelations :: Bool
  , typeSuppress_printExplicitKinds :: Bool
  , typeSuppress_printExplicitCoercions :: Bool
  , typeSuppress_printExplicitRuntimeReps :: Bool
  , typeSuppress_printExplicitForalls :: Bool
  , typeSuppress_suppressTypeApplications :: Bool
  , typeSuppress_suppressTypeSignatures :: Bool
  , typeSuppress_suppressVarKinds :: Bool
  }

_typeSuppress_printExplicitKinds :: Lens' TypeSuppress Bool
_typeSuppress_printExplicitKinds f ts = fmap
  (\x -> ts { typeSuppress_suppressVarKinds = x })
  (f $ typeSuppress_suppressVarKinds ts)

_typeSuppress_printExplicitRuntimeReps :: Lens' TypeSuppress Bool
_typeSuppress_printExplicitRuntimeReps f ts = fmap
  (\x -> ts { typeSuppress_suppressVarKinds = x })
  (f $ typeSuppress_suppressVarKinds ts)

class HasTypeSuppress c where
  typeSuppress :: Lens' c TypeSuppress
