module GHC.HsToCore.Expr where
import GHC.Hs             ( HsExpr, LHsExpr, LHsLocalBinds, SyntaxExpr )
import GHC.HsToCore.Monad ( DsM )
import GHC.Core           ( CoreExpr )
import GHC.Hs.Extension.GhcPass ( GhcTc)

dsExpr  :: HsExpr GhcTc -> DsM CoreExpr
dsLExpr, dsLExprNoLP :: LHsExpr GhcTc -> DsM CoreExpr
dsSyntaxExpr :: SyntaxExpr GhcTc -> [CoreExpr] -> DsM CoreExpr
dsLocalBinds :: LHsLocalBinds GhcTc -> CoreExpr -> DsM CoreExpr
