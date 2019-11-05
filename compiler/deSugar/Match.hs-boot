module Match where

import GhcPrelude
import Var      ( Id )
import TcType   ( Type )
import DsMonad  ( DsM, EquationInfo, MatchResult )
import CoreSyn  ( CoreExpr )
import GHC.Hs   ( LPat, HsMatchContext, MatchGroup, LHsExpr )
import GHC.Hs.Extension ( GhcRn, GhcTc )

match   :: [Id]
        -> Type
        -> [EquationInfo]
        -> DsM MatchResult

matchWrapper
        :: HsMatchContext GhcRn
        -> Maybe (LHsExpr GhcTc)
        -> MatchGroup GhcTc (LHsExpr GhcTc)
        -> DsM ([Id], CoreExpr)

matchSimply
        :: CoreExpr
        -> HsMatchContext GhcRn
        -> LPat GhcTc
        -> CoreExpr
        -> CoreExpr
        -> DsM CoreExpr

matchSinglePatVar
        :: Id
        -> HsMatchContext GhcRn
        -> LPat GhcTc
        -> Type
        -> MatchResult
        -> DsM MatchResult
