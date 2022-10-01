

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Matching guarded right-hand-sides (GRHSs)
-}

module GHC.HsToCore.GuardedRHSs ( dsGuarded, dsGRHSs, isTrueLHsExpr ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.HsToCore.Expr  ( dsLExpr, dsLocalBinds )
import {-# SOURCE #-} GHC.HsToCore.Match ( matchSinglePatVar )

import GHC.Hs
import GHC.Core.Make
import GHC.Core
import GHC.Core.Utils (bindNonRec)

import GHC.HsToCore.Monad
import GHC.HsToCore.Utils
import GHC.HsToCore.Pmc.Types ( Nablas )
import GHC.Core.Type ( Type )
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Core.Multiplicity
import Data.List.NonEmpty ( NonEmpty )
import qualified GHC.Data.List.NonEmpty as NE

{-
@dsGuarded@ is used for GRHSs.
It desugars:
\begin{verbatim}
        | g1 -> e1
        ...
        | gn -> en
        where binds
\end{verbatim}
producing an expression with a runtime error in the corner case if
necessary.  The type argument gives the type of the @ei@.
-}

dsGuarded :: GRHSs GhcTc (LHsExpr GhcTc) -> Type -> NonEmpty Nablas -> DsM CoreExpr
dsGuarded grhss rhs_ty rhss_nablas = do
    match_result <- dsGRHSs PatBindRhs grhss rhs_ty rhss_nablas
    error_expr <- mkErrorAppDs nON_EXHAUSTIVE_GUARDS_ERROR_ID rhs_ty
                               (text "pattern binding")
    extractMatchResult match_result error_expr

-- In contrast, @dsGRHSs@ produces a @MatchResult CoreExpr@.

dsGRHSs :: HsMatchContextRn
        -> GRHSs GhcTc (LHsExpr GhcTc) -- ^ Guarded RHSs
        -> Type                        -- ^ Type of RHS
        -> NonEmpty Nablas             -- ^ Refined pattern match checking
                                       --   models, one for the pattern part and
                                       --   one for each GRHS.
        -> DsM (MatchResult CoreExpr)
dsGRHSs hs_ctx (GRHSs _ grhss binds) rhs_ty rhss_nablas
  = do { match_results <- assert (length grhss == length rhss_nablas) $
                          NE.zipWithM (dsGRHS hs_ctx rhs_ty) rhss_nablas grhss
       ; nablas <- getPmNablas
       -- We need to remember the Nablas from the particular match context we
       -- are in, which might be different to when dsLocalBinds is actually
       -- called.
       ; let ds_binds      = updPmNablas nablas . dsLocalBinds binds
             match_result1 = foldr1 combineMatchResults match_results
             match_result2 = adjustMatchResultDs ds_binds match_result1
                             -- NB: nested dsLet inside matchResult
       ; return match_result2 }

dsGRHS :: HsMatchContextRn -> Type -> Nablas -> LGRHS GhcTc (LHsExpr GhcTc)
       -> DsM (MatchResult CoreExpr)
dsGRHS hs_ctx rhs_ty rhs_nablas (L _ (GRHS _ guards rhs))
  = matchGuards (map unLoc guards) hs_ctx rhs_nablas rhs rhs_ty

{-
************************************************************************
*                                                                      *
*  matchGuard : make a MatchResult CoreExpr CoreExpr from a guarded RHS                  *
*                                                                      *
************************************************************************
-}

matchGuards :: [GuardStmt GhcTc]     -- Guard
            -> HsMatchContextRn      -- Context
            -> Nablas                -- The RHS's covered set for PmCheck
            -> LHsExpr GhcTc         -- RHS
            -> Type                  -- Type of RHS of guard
            -> DsM (MatchResult CoreExpr)

-- See comments with HsExpr.Stmt re what a BodyStmt means
-- Here we must be in a guard context (not do-expression, nor list-comp)

matchGuards [] _ nablas rhs _
  = do  { core_rhs <- updPmNablas nablas (dsLExpr rhs)
        ; return (cantFailMatchResult core_rhs) }

        -- BodyStmts must be guards
        -- Turn an "otherwise" guard is a no-op.  This ensures that
        -- you don't get a "non-exhaustive eqns" message when the guards
        -- finish in "otherwise".
        -- NB:  The success of this clause depends on the typechecker not
        --      wrapping the 'otherwise' in empty HsTyApp or HsWrap constructors
        --      If it does, you'll get bogus overlap warnings
matchGuards (BodyStmt _ e _ _ : stmts) ctx nablas rhs rhs_ty
  | Just addTicks <- isTrueLHsExpr e = do
    match_result <- matchGuards stmts ctx nablas rhs rhs_ty
    return (adjustMatchResultDs addTicks match_result)
matchGuards (BodyStmt _ expr _ _ : stmts) ctx nablas rhs rhs_ty = do
    match_result <- matchGuards stmts ctx nablas rhs rhs_ty
    pred_expr <- dsLExpr expr
    return (mkGuardedMatchResult pred_expr match_result)

matchGuards (LetStmt _ binds : stmts) ctx nablas rhs rhs_ty = do
    match_result <- matchGuards stmts ctx nablas rhs rhs_ty
    return (adjustMatchResultDs (dsLocalBinds binds) match_result)
        -- NB the dsLet occurs inside the match_result
        -- Reason: dsLet takes the body expression as its argument
        --         so we can't desugar the bindings without the
        --         body expression in hand

matchGuards (BindStmt _ pat bind_rhs : stmts) ctx nablas rhs rhs_ty = do
    let upat = unLoc pat
    match_var <- selectMatchVar ManyTy upat
       -- We only allow unrestricted patterns in guards, hence the `Many`
       -- above. It isn't clear what linear patterns would mean, maybe we will
       -- figure it out in the future.

    match_result <- matchGuards stmts ctx nablas rhs rhs_ty
    core_rhs <- dsLExpr bind_rhs
    match_result' <-
      matchSinglePatVar match_var (Just core_rhs) (StmtCtxt $ PatGuard ctx)
      pat rhs_ty match_result
    return $ bindNonRec match_var core_rhs <$> match_result'

matchGuards (LastStmt  {} : _) _ _ _ _ = panic "matchGuards LastStmt"
matchGuards (ParStmt   {} : _) _ _ _ _ = panic "matchGuards ParStmt"
matchGuards (TransStmt {} : _) _ _ _ _ = panic "matchGuards TransStmt"
matchGuards (RecStmt   {} : _) _ _ _ _ = panic "matchGuards RecStmt"
matchGuards (XStmtLR ApplicativeStmt {} : _) _ _ _ _ =
  panic "matchGuards ApplicativeLastStmt"

{-
Should {\em fail} if @e@ returns @D@
\begin{verbatim}
f x | p <- e', let C y# = e, f y# = r1
    | otherwise          = r2
\end{verbatim}
-}
