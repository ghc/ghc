module TypeSuppress where

import GhcPrelude

data TypeSuppress = TypeSuppress
  { typeSuppress_typeApplications :: Bool
  , typeSuppress_typeSignatures :: Bool
  , typeSuppress_varKinds :: Bool
  }

class HasTypeSuppress c where
  getTypeSuppress :: c -> TypeSuppress

