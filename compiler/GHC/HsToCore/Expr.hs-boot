module GHC.HsToCore.Expr where
import GHC.Hs             ( HsExpr, LHsExpr, HsLocalBinds, SyntaxExpr )
import GHC.HsToCore.Monad ( DsM )
import GHC.Core           ( CoreExpr )
import GHC.Hs.Extension   ( GhcTc)

dsExpr  :: HsExpr GhcTc -> DsM CoreExpr
dsLExpr, dsLExprNoLP :: LHsExpr GhcTc -> DsM CoreExpr
dsSyntaxExpr :: SyntaxExpr GhcTc -> [CoreExpr] -> DsM CoreExpr
dsLocalBinds :: HsLocalBinds GhcTc -> CoreExpr -> DsM CoreExpr
