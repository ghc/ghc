module DsExpr where
import HsSyn    ( HsExpr, LHsExpr, HsLocalBinds, SyntaxExpr )
import Var      ( Id )
import DsMonad  ( DsM )
import CoreSyn  ( CoreExpr )

dsExpr  :: HsExpr  Id -> DsM CoreExpr
dsLExpr :: LHsExpr Id -> DsM CoreExpr
dsSyntaxExpr :: SyntaxExpr Id -> [CoreExpr] -> DsM CoreExpr
dsLocalBinds :: HsLocalBinds Id -> CoreExpr -> DsM CoreExpr
