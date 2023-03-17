module GHC.Types.Name.Occurrence where

import GHC.Data.FastString ( FastString )

data OccName

class HasOccName name where
  occName :: name -> OccName

occNameFS :: OccName -> FastString
mkVarOccFS :: FastString -> OccName
