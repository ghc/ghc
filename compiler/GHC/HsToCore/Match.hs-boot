module GHC.HsToCore.Match where

import GHC.Prelude
import GHC.Types.Var ( Id )
import GHC.Tc.Utils.TcType  ( Type )
import GHC.HsToCore.Monad ( DsM, EquationInfo, MatchResult )
import GHC.Core ( CoreExpr )
import GHC.Hs   ( LMatchPat, HsMatchContext, MatchGroup, LHsExpr )
import GHC.Hs.Extension ( GhcTc, GhcRn )

match   :: [Id]
        -> Type
        -> [EquationInfo]
        -> DsM (MatchResult CoreExpr)

matchWrapper
        :: HsMatchContext GhcRn
        -> Maybe (LHsExpr GhcTc)
        -> MatchGroup GhcTc (LHsExpr GhcTc)
        -> DsM ([Id], CoreExpr)

matchSimply
        :: CoreExpr
        -> HsMatchContext GhcRn
        -> LMatchPat GhcTc
        -> CoreExpr
        -> CoreExpr
        -> DsM CoreExpr

matchSinglePatVar
        :: Id
        -> Maybe CoreExpr
        -> HsMatchContext GhcRn
        -> LMatchPat GhcTc
        -> Type
        -> MatchResult CoreExpr
        -> DsM (MatchResult CoreExpr)
