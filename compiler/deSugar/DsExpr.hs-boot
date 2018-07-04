module DsExpr where
import HsSyn       ( HsExpr, LHsExpr, LHsLocalBinds, SyntaxExpr )
import DsMonad     ( DsM )
import CoreSyn     ( CoreExpr )
import HsExtension ( GhcTc)
import Util (HasCallStack)

dsExpr  :: HasCallStack => HsExpr GhcTc -> DsM CoreExpr
dsLExpr, dsLExprNoLP :: HasCallStack => LHsExpr GhcTc -> DsM CoreExpr
dsSyntaxExpr :: SyntaxExpr GhcTc -> [CoreExpr] -> DsM CoreExpr
dsLocalBinds :: LHsLocalBinds GhcTc -> CoreExpr -> DsM CoreExpr
