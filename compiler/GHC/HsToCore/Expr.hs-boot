module GHC.HsToCore.Expr where
import GHC.Hs             ( HsExpr, LHsExpr, HsLocalBinds, SyntaxExpr )
import GHC.HsToCore.Monad ( DsM )
import GHC.Core           ( CoreExpr )
import GHC.Hs.Extension ( GhcTc)
import GHC.Stack ( HasCallStack )

dsExpr  :: HsExpr GhcTc -> DsM CoreExpr
dsLExpr :: LHsExpr GhcTc -> DsM CoreExpr
dsSyntaxExpr :: SyntaxExpr GhcTc -> [CoreExpr] -> DsM CoreExpr
dsLocalBinds :: HasCallStack => HsLocalBinds GhcTc -> CoreExpr -> DsM CoreExpr
