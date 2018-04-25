module QQ where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

myq = QuasiQuoter { quoteExp = \s -> litE $ stringL $ s ++ " world"}
