module DsExpr where
import GHC.Hs      ( HsExpr, LHsExpr, LHsLocalBinds, SyntaxExpr )
import DsMonad     ( DsM )
import CoreSyn     ( CoreExpr )
import GHC.Hs.Extension ( GhcTc)
import GHC.Stack

dsExpr  :: HasCallStack => HsExpr GhcTc -> DsM CoreExpr
dsLExpr :: HasCallStack => LHsExpr GhcTc -> DsM CoreExpr

dsLExprNoLP :: HasCallStack => LHsExpr GhcTc -> DsM CoreExpr
dsSyntaxExpr :: SyntaxExpr GhcTc -> [CoreExpr] -> DsM CoreExpr
dsLocalBinds :: LHsLocalBinds GhcTc -> CoreExpr -> DsM CoreExpr
