module GHC.StgToCmm.Expr ( cgExpr ) where

import GHC.Stg.Syntax
import GHC.StgToCmm.Monad

cgExpr :: CgStgExpr -> FCode ReturnKind
