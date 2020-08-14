module GHC.Types.Name.Occurrence where

import GHC.Prelude ()

data OccName

class HasOccName name where
  occName :: name -> OccName
