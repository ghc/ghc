module T5555_Lib(s) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

s :: QuasiQuoter
s = QuasiQuoter expr undefined undefined undefined

expr :: String -> Q Exp
expr = stringE
