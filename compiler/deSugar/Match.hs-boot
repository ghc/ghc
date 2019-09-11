module Match where

import GhcPrelude
import Var      ( Id )
import TcType   ( Type )
import DsMonad  ( DsM, EquationInfo, MatchResult )
import CoreSyn  ( CoreExpr )
import GHC.Hs   ( LPat, HsMatchContext, MatchGroup, LHsExpr )
import Name     ( Name )
import GHC.Hs.Extension ( GhcTc )

match   :: [Id]
        -> Type
        -> [EquationInfo]
        -> DsM MatchResult

matchWrapper
        :: HsMatchContext Name
        -> Maybe (LHsExpr GhcTc)
        -> MatchGroup GhcTc (LHsExpr GhcTc)
        -> DsM ([Id], CoreExpr)

matchSimply
        :: CoreExpr
        -> HsMatchContext Name
        -> LPat GhcTc
        -> CoreExpr
        -> CoreExpr
        -> DsM CoreExpr

matchSinglePatVar
        :: Id
        -> HsMatchContext Name
        -> LPat GhcTc
        -> Type
        -> MatchResult
        -> DsM MatchResult
