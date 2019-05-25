module NameSuppress where

import GhcPrelude

data NameSuppress = NameSuppress
  { nameSuppress_modulePrefixes :: Bool
  , nameSuppress_uniques :: Bool
  }

class HasNameSuppress c where
  getNameSuppress :: c -> NameSuppress

