{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Matching guarded right-hand-sides (GRHSs)
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.HsToCore.GuardedRHSs ( dsGuarded, dsGRHSs, isTrueLHsExpr ) where

#include "HsVersions.h"

import GhcPrelude

import {-# SOURCE #-} GHC.HsToCore.Expr  ( dsLExpr, dsLocalBinds )
import {-# SOURCE #-} GHC.HsToCore.Match ( matchSinglePatVar )

import GHC.Hs
import GHC.Core.Make
import GHC.Core
import GHC.Core.Utils (bindNonRec)

import GHC.HsToCore.Monad
import GHC.HsToCore.Utils
import GHC.HsToCore.PmCheck.Types ( Deltas, initDeltas )
import GHC.Core.Type ( Type )
import Util
import GHC.Types.SrcLoc
import Outputable
import Control.Monad ( zipWithM )
import Data.List.NonEmpty ( NonEmpty, toList )

{-
@dsGuarded@ is used for pattern bindings.
It desugars:
\begin{verbatim}
        | g1 -> e1
        ...
        | gn -> en
        where binds
\end{verbatim}
producing an expression with a runtime error in the corner if
necessary.  The type argument gives the type of the @ei@.
-}

dsGuarded :: GRHSs GhcTc (LHsExpr GhcTc) -> Type -> Maybe (NonEmpty Deltas) -> DsM CoreExpr
dsGuarded grhss rhs_ty mb_rhss_deltas = do
    match_result <- dsGRHSs dsLExpr PatBindRhs grhss rhs_ty mb_rhss_deltas
    error_expr <- mkErrorAppDs nON_EXHAUSTIVE_GUARDS_ERROR_ID rhs_ty empty
    extractMatchResult match_result error_expr

-- In contrast, @dsGRHSs@ produces a @MatchResult@.

dsGRHSs :: (body -> DsM CoreExpr)      -- ^ How to desugar each RHS
        -> HsMatchContext GhcRn
        -> GRHSs GhcTc body            -- ^ Guarded RHSs
        -> Type                        -- ^ Type of RHS
        -> Maybe (NonEmpty Deltas)     -- ^ Refined pattern match checking
                                       --   models, one for each GRHS. Defaults
                                       --   to 'initDeltas' if 'Nothing'.
        -> DsM MatchResult
dsGRHSs ds_rhs hs_ctx (GRHSs _ grhss binds) rhs_ty mb_rhss_deltas
  = ASSERT( notNull grhss )
    do { match_results <- case toList <$> mb_rhss_deltas of
           Nothing          -> mapM     (dsGRHS ds_rhs hs_ctx rhs_ty initDeltas) grhss
           Just rhss_deltas -> ASSERT( length grhss == length rhss_deltas )
                               zipWithM (dsGRHS ds_rhs hs_ctx rhs_ty) rhss_deltas grhss
       ; let match_result1 = foldr1 combineMatchResults match_results
             match_result2 = adjustMatchResultDs (dsLocalBinds binds) match_result1
                             -- NB: nested dsLet inside matchResult
       ; return match_result2 }

dsGRHS :: (body -> DsM CoreExpr) -> HsMatchContext GhcRn -> Type -> Deltas
       -> LGRHS GhcTc body -> DsM MatchResult
dsGRHS ds_rhs hs_ctx rhs_ty rhs_deltas (L _ (GRHS _ guards rhs))
  = updPmDeltas rhs_deltas $
    matchGuards ds_rhs (map unLoc guards) (PatGuard hs_ctx) rhs rhs_ty

{-
************************************************************************
*                                                                      *
*  matchGuard : make a MatchResult from a guarded RHS                  *
*                                                                      *
************************************************************************
-}

matchGuards :: (body -> DsM CoreExpr) -- How to desugar each RHS
            -> [GuardStmt GhcTc]      -- Guard
            -> HsStmtContext GhcRn    -- Context
            -> body                   -- RHS
            -> Type                   -- Type of RHS of guard
            -> DsM MatchResult

-- See comments with HsExpr.Stmt re what a BodyStmt means
-- Here we must be in a guard context (not do-expression, nor list-comp)

matchGuards ds_rhs [] _ rhs _
  = do  { core_rhs <- ds_rhs rhs
        ; return (cantFailMatchResult core_rhs) }

        -- BodyStmts must be guards
        -- Turn an "otherwise" guard is a no-op.  This ensures that
        -- you don't get a "non-exhaustive eqns" message when the guards
        -- finish in "otherwise".
        -- NB:  The success of this clause depends on the typechecker not
        --      wrapping the 'otherwise' in empty HsTyApp or HsWrap constructors
        --      If it does, you'll get bogus overlap warnings
matchGuards ds_rhs (BodyStmt _ e _ _ : stmts) ctx rhs rhs_ty
  | Just addTicks <- isTrueLHsExpr e = do
    match_result <- matchGuards ds_rhs stmts ctx rhs rhs_ty
    return (adjustMatchResultDs addTicks match_result)
matchGuards ds_rhs (BodyStmt _ expr _ _ : stmts) ctx rhs rhs_ty = do
    match_result <- matchGuards ds_rhs stmts ctx rhs rhs_ty
    pred_expr <- dsLExpr expr
    return (mkGuardedMatchResult pred_expr match_result)

matchGuards ds_rhs (LetStmt _ binds : stmts) ctx rhs rhs_ty = do
    match_result <- matchGuards ds_rhs stmts ctx rhs rhs_ty
    return (adjustMatchResultDs (dsLocalBinds binds) match_result)
        -- NB the dsLet occurs inside the match_result
        -- Reason: dsLet takes the body expression as its argument
        --         so we can't desugar the bindings without the
        --         body expression in hand

matchGuards ds_rhs (BindStmt _ pat bind_rhs : stmts) ctx rhs rhs_ty = do
    let upat = unLoc pat
    match_var <- selectMatchVar upat

    match_result <- matchGuards ds_rhs stmts ctx rhs rhs_ty
    core_rhs <- dsLExpr bind_rhs
    match_result' <- matchSinglePatVar match_var (StmtCtxt ctx) pat rhs_ty
                                       match_result
    pure $ adjustMatchResult (bindNonRec match_var core_rhs) match_result'

matchGuards _ (LastStmt  {} : _) _ _ _ = panic "matchGuards LastStmt"
matchGuards _ (ParStmt   {} : _) _ _ _ = panic "matchGuards ParStmt"
matchGuards _ (TransStmt {} : _) _ _ _ = panic "matchGuards TransStmt"
matchGuards _ (RecStmt   {} : _) _ _ _ = panic "matchGuards RecStmt"
matchGuards _ (ApplicativeStmt {} : _) _ _ _ =
  panic "matchGuards ApplicativeLastStmt"

{-
Should {\em fail} if @e@ returns @D@
\begin{verbatim}
f x | p <- e', let C y# = e, f y# = r1
    | otherwise          = r2
\end{verbatim}
-}
