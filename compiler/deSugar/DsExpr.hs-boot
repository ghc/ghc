module DsExpr where

import GhcPrelude  ( Maybe )

import GHC.Hs      ( HsExpr, LHsExpr, LHsLocalBinds, LPat, SyntaxExpr )
import DsMonad     ( DsM, MatchResult )
import CoreSyn     ( CoreExpr )
import GHC.Hs.Extension ( GhcTc)

dsExpr  :: HsExpr GhcTc -> DsM CoreExpr
dsLExpr, dsLExprNoLP :: LHsExpr GhcTc -> DsM CoreExpr
dsSyntaxExpr :: SyntaxExpr GhcTc -> [CoreExpr] -> DsM CoreExpr
dsLocalBinds :: LHsLocalBinds GhcTc -> CoreExpr -> DsM CoreExpr

dsHandleMonadicFailure :: LPat GhcTc -> MatchResult -> Maybe (SyntaxExpr GhcTc) -> DsM CoreExpr
