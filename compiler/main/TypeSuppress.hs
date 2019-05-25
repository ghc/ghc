module TypeSuppress where

import GhcPrelude

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

class HasTypeSuppress c where
  getTypeSuppress :: c -> TypeSuppress
