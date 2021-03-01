module GHC.StgToCmm.Expr where

import GHC.Cmm.Expr
import GHC.StgToCmm.Monad
import GHC.Types.Literal

cgLit :: Literal -> FCode CmmExpr
