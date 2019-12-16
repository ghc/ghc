module GHC.HsToCore.Expr where

import GHC.Core           ( CoreExpr )
import GHC.Hs             ( HsExpr, LHsExpr, LHsLocalBinds, LPat, SyntaxExpr, FailOperator )
import GHC.Hs.Extension   ( GhcTc)
import GHC.HsToCore.Monad ( DsM, MatchResult )
import GHC.Tc.Utils.TcType ( Type )

dsExpr  :: HsExpr GhcTc -> DsM CoreExpr
dsLExpr, dsLExprNoLP :: LHsExpr GhcTc -> DsM CoreExpr
dsSyntaxExpr :: SyntaxExpr GhcTc -> [CoreExpr] -> DsM CoreExpr
dsLocalBinds :: LHsLocalBinds GhcTc -> CoreExpr -> DsM CoreExpr

dsHandleMonadicFailure :: Type
                       -> LPat GhcTc
                       -> MatchResult CoreExpr
                       -> FailOperator GhcTc
                       -> DsM CoreExpr
