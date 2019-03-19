module Lift_String where

import Language.Haskell.TH

foo :: String -> Q (TExp String)
foo x = [|| x ||]
