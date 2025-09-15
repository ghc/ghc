module TH_lookupName_Lib where

import Language.Haskell.TH

f :: String
f = "TH_lookupName_Lib.f"

lookup_f :: Q Exp
lookup_f = do { Just n <- lookupValueName "f"; varE n }
