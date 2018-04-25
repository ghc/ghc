{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Matching guarded right-hand-sides (GRHSs)
-}

{-# LANGUAGE CPP #-}

module DsGRHSs ( dsGuarded, dsGRHSs, dsGRHS, isTrueLHsExpr ) where

#include "HsVersions.h"

import GhcPrelude

import {-# SOURCE #-} DsExpr  ( dsLExpr, dsLocalBinds )
import {-# SOURCE #-} Match   ( matchSinglePat )

import HsSyn
import MkCore
import CoreSyn

import DsMonad
import DsUtils
import TysWiredIn
import PrelNames
import Type   ( Type )
import Module
import Name
import Util
import SrcLoc
import Outputable

{-
@dsGuarded@ is used for both @case@ expressions and pattern bindings.
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

dsGuarded :: GRHSs GhcTc (LHsExpr GhcTc) -> Type -> DsM CoreExpr

dsGuarded grhss rhs_ty = do
    match_result <- dsGRHSs PatBindRhs grhss rhs_ty
    error_expr <- mkErrorAppDs nON_EXHAUSTIVE_GUARDS_ERROR_ID rhs_ty empty
    extractMatchResult match_result error_expr

-- In contrast, @dsGRHSs@ produces a @MatchResult@.

dsGRHSs :: HsMatchContext Name
        -> GRHSs GhcTc (LHsExpr GhcTc)          -- Guarded RHSs
        -> Type                                 -- Type of RHS
        -> DsM MatchResult
dsGRHSs hs_ctx (GRHSs grhss binds) rhs_ty
  = ASSERT( notNull grhss )
    do { match_results <- mapM (dsGRHS hs_ctx rhs_ty) grhss
       ; let match_result1 = foldr1 combineMatchResults match_results
             match_result2 = adjustMatchResultDs (dsLocalBinds binds) match_result1
                             -- NB: nested dsLet inside matchResult
       ; return match_result2 }

dsGRHS :: HsMatchContext Name -> Type -> LGRHS GhcTc (LHsExpr GhcTc)
       -> DsM MatchResult
dsGRHS hs_ctx rhs_ty (L _ (GRHS guards rhs))
  = matchGuards (map unLoc guards) (PatGuard hs_ctx) rhs rhs_ty

{-
************************************************************************
*                                                                      *
*  matchGuard : make a MatchResult from a guarded RHS                  *
*                                                                      *
************************************************************************
-}

matchGuards :: [GuardStmt GhcTc]     -- Guard
            -> HsStmtContext Name    -- Context
            -> LHsExpr GhcTc         -- RHS
            -> Type                  -- Type of RHS of guard
            -> DsM MatchResult

-- See comments with HsExpr.Stmt re what a BodyStmt means
-- Here we must be in a guard context (not do-expression, nor list-comp)

matchGuards [] _ rhs _
  = do  { core_rhs <- dsLExpr rhs
        ; return (cantFailMatchResult core_rhs) }

        -- BodyStmts must be guards
        -- Turn an "otherwise" guard is a no-op.  This ensures that
        -- you don't get a "non-exhaustive eqns" message when the guards
        -- finish in "otherwise".
        -- NB:  The success of this clause depends on the typechecker not
        --      wrapping the 'otherwise' in empty HsTyApp or HsWrap constructors
        --      If it does, you'll get bogus overlap warnings
matchGuards (BodyStmt e _ _ _ : stmts) ctx rhs rhs_ty
  | Just addTicks <- isTrueLHsExpr e = do
    match_result <- matchGuards stmts ctx rhs rhs_ty
    return (adjustMatchResultDs addTicks match_result)
matchGuards (BodyStmt expr _ _ _ : stmts) ctx rhs rhs_ty = do
    match_result <- matchGuards stmts ctx rhs rhs_ty
    pred_expr <- dsLExpr expr
    return (mkGuardedMatchResult pred_expr match_result)

matchGuards (LetStmt binds : stmts) ctx rhs rhs_ty = do
    match_result <- matchGuards stmts ctx rhs rhs_ty
    return (adjustMatchResultDs (dsLocalBinds binds) match_result)
        -- NB the dsLet occurs inside the match_result
        -- Reason: dsLet takes the body expression as its argument
        --         so we can't desugar the bindings without the
        --         body expression in hand

matchGuards (BindStmt pat bind_rhs _ _ _ : stmts) ctx rhs rhs_ty = do
    match_result <- matchGuards stmts ctx rhs rhs_ty
    core_rhs <- dsLExpr bind_rhs
    matchSinglePat core_rhs (StmtCtxt ctx) pat rhs_ty match_result

matchGuards (LastStmt  {} : _) _ _ _ = panic "matchGuards LastStmt"
matchGuards (ParStmt   {} : _) _ _ _ = panic "matchGuards ParStmt"
matchGuards (TransStmt {} : _) _ _ _ = panic "matchGuards TransStmt"
matchGuards (RecStmt   {} : _) _ _ _ = panic "matchGuards RecStmt"
matchGuards (ApplicativeStmt {} : _) _ _ _ =
  panic "matchGuards ApplicativeLastStmt"

isTrueLHsExpr :: LHsExpr GhcTc -> Maybe (CoreExpr -> DsM CoreExpr)

-- Returns Just {..} if we're sure that the expression is True
-- I.e.   * 'True' datacon
--        * 'otherwise' Id
--        * Trivial wappings of these
-- The arguments to Just are any HsTicks that we have found,
-- because we still want to tick then, even it they are always evaluated.
isTrueLHsExpr (L _ (HsVar (L _ v))) |  v `hasKey` otherwiseIdKey
                                    || v `hasKey` getUnique trueDataConId
                                            = Just return
        -- trueDataConId doesn't have the same unique as trueDataCon
isTrueLHsExpr (L _ (HsConLikeOut con)) | con `hasKey` getUnique trueDataCon = Just return
isTrueLHsExpr (L _ (HsTick tickish e))
    | Just ticks <- isTrueLHsExpr e
    = Just (\x -> do wrapped <- ticks x
                     return (Tick tickish wrapped))
   -- This encodes that the result is constant True for Hpc tick purposes;
   -- which is specifically what isTrueLHsExpr is trying to find out.
isTrueLHsExpr (L _ (HsBinTick ixT _ e))
    | Just ticks <- isTrueLHsExpr e
    = Just (\x -> do e <- ticks x
                     this_mod <- getModule
                     return (Tick (HpcTick this_mod ixT) e))

isTrueLHsExpr (L _ (HsPar e))         = isTrueLHsExpr e
isTrueLHsExpr _                       = Nothing

{-
Should {\em fail} if @e@ returns @D@
\begin{verbatim}
f x | p <- e', let C y# = e, f y# = r1
    | otherwise          = r2
\end{verbatim}
-}
