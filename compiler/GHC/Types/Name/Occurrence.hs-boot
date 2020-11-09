module GHC.Types.Name.Occurrence where

import GHC.Prelude (String)
import GHC.Data.FastString

data OccName

class HasOccName name where
  occName :: name -> OccName

occNameString :: OccName -> String
mkRecFldSelOcc :: String -> OccName
mkVarOccFS :: FastString -> OccName
