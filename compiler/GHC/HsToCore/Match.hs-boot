module GHC.HsToCore.Match where

import GHC.Prelude
import GHC.Types.Var ( Id )
import GHC.Tc.Utils.TcType  ( Type )
import GHC.HsToCore.Monad ( DsM, EquationInfo, MatchResult )
import GHC.Core ( CoreExpr )
import GHC.Hs   ( LPat, HsMatchContext, MatchGroup, LHsExpr )
import GHC.Hs.Extension ( GhcTc )

match   :: [Id]
        -> Type
        -> [EquationInfo]
        -> DsM (MatchResult CoreExpr)

matchWrapper
        :: HsMatchContext GhcTc
        -> Maybe [LHsExpr GhcTc]
        -> MatchGroup GhcTc (LPat GhcTc) (LHsExpr GhcTc)
        -> DsM ([Id], CoreExpr)

matchSimply
        :: CoreExpr
        -> HsMatchContext GhcTc
        -> LPat GhcTc
        -> CoreExpr
        -> CoreExpr
        -> DsM CoreExpr

matchSinglePatVar
        :: Id
        -> Maybe CoreExpr
        -> HsMatchContext GhcTc
        -> LPat GhcTc
        -> Type
        -> MatchResult CoreExpr
        -> DsM (MatchResult CoreExpr)
