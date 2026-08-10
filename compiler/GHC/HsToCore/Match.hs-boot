module GHC.HsToCore.Match where

import GHC.Prelude
import GHC.Types.Var ( Id )
import GHC.Tc.Utils.TcType  ( Type )
import GHC.HsToCore.Monad ( DsM, EquationInfo, MatchId, MatchResult )
import GHC.Core ( CoreExpr )
import GHC.Hs   ( LPat, MatchGroup, LHsExpr, Mult )
import GHC.Hs.Expr ( HsMatchContextRn )
import GHC.Hs.Extension ( GhcTc )

match   :: [MatchId]
        -> Type
        -> [EquationInfo]
        -> DsM (MatchResult CoreExpr)

matchWrapper
        :: HsMatchContextRn
        -> Maybe [CoreExpr]
        -> MatchGroup GhcTc (LHsExpr GhcTc)
        -> DsM ([Id], CoreExpr)

matchSimply
        :: CoreExpr
        -> HsMatchContextRn
        -> Mult
        -> LPat GhcTc
        -> CoreExpr
        -> CoreExpr
        -> DsM CoreExpr

matchSinglePatVar
        :: Id
        -> Maybe CoreExpr
        -> HsMatchContextRn
        -> LPat GhcTc
        -> Type
        -> MatchResult CoreExpr
        -> DsM (MatchResult CoreExpr)
