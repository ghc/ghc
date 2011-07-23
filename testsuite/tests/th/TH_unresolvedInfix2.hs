module TH_unresolvedInfix2 where

import TH_unresolvedInfix_Lib
import Language.Haskell.TH

expr = $( infixE Nothing plus (Just $ n +? n) )
