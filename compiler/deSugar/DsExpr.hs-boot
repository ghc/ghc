module DsExpr where

import GhcPrelude  ( Maybe )

import GHC.Hs      ( HsExpr, LHsExpr, LHsLocalBinds, LPat, SyntaxExpr )
import TcType      ( Type )
import DsMonad     ( DsM, MatchResult )
import CoreSyn     ( CoreExpr )
import GHC.Hs.Extension ( GhcTc)

dsExpr  :: HsExpr GhcTc -> DsM CoreExpr
dsLExpr, dsLExprNoLP :: LHsExpr GhcTc -> DsM CoreExpr
dsSyntaxExpr :: SyntaxExpr GhcTc -> [CoreExpr] -> DsM CoreExpr
dsLocalBinds :: LHsLocalBinds GhcTc -> CoreExpr -> DsM CoreExpr

dsHandleMonadicFailure :: Type -> LPat GhcTc -> MatchResult -> Maybe (SyntaxExpr GhcTc) -> DsM CoreExpr
