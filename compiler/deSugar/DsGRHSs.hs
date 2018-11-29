{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Matching guarded right-hand-sides (GRHSs)
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module DsGRHSs ( dsGuarded, dsGRHSs, dsGRHS, isTrueLHsExpr ) where

#include "HsVersions.h"

import GhcPrelude

import {-# SOURCE #-} DsExpr  ( dsLExpr, dsLocalBinds )
import {-# SOURCE #-} Match   ( matchSinglePatVar )

import HsSyn
import MkCore
import CoreSyn
import CoreUtils (bindNonRec)

import Check (genCaseTmCs2)
import DsMonad
import DsUtils
import Type   ( Type )
import Name
import Util
import SrcLoc
import Outputable
import Multiplicity

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
dsGRHSs hs_ctx (GRHSs _ grhss binds) rhs_ty
  = ASSERT( notNull grhss )
    do { match_results <- mapM (dsGRHS hs_ctx rhs_ty) grhss
       ; let match_result1 = foldr1 combineMatchResults match_results
             match_result2 = adjustMatchResultDs (dsLocalBinds binds) match_result1
                             -- NB: nested dsLet inside matchResult
       ; return match_result2 }
dsGRHSs _ (XGRHSs _) _ = panic "dsGRHSs"

dsGRHS :: HsMatchContext Name -> Type -> LGRHS GhcTc (LHsExpr GhcTc)
       -> DsM MatchResult
dsGRHS hs_ctx rhs_ty (dL->L _ (GRHS _ guards rhs))
  = matchGuards (map unLoc guards) (PatGuard hs_ctx) rhs rhs_ty
dsGRHS _ _ (dL->L _ (XGRHS _)) = panic "dsGRHS"
dsGRHS _ _ _ = panic "dsGRHS: Impossible Match" -- due to #15884

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
matchGuards (BodyStmt _ e _ _ : stmts) ctx rhs rhs_ty
  | Just addTicks <- isTrueLHsExpr e = do
    match_result <- matchGuards stmts ctx rhs rhs_ty
    return (adjustMatchResultDs addTicks match_result)
matchGuards (BodyStmt _ expr _ _ : stmts) ctx rhs rhs_ty = do
    match_result <- matchGuards stmts ctx rhs rhs_ty
    pred_expr <- dsLExpr expr
    return (mkGuardedMatchResult pred_expr match_result)

matchGuards (LetStmt _ binds : stmts) ctx rhs rhs_ty = do
    match_result <- matchGuards stmts ctx rhs rhs_ty
    return (adjustMatchResultDs (dsLocalBinds binds) match_result)
        -- NB the dsLet occurs inside the match_result
        -- Reason: dsLet takes the body expression as its argument
        --         so we can't desugar the bindings without the
        --         body expression in hand

matchGuards (BindStmt _ pat bind_rhs _ _ : stmts) ctx rhs rhs_ty = do
    let upat = unLoc pat
        dicts = collectEvVarsPat upat
    match_var <- selectMatchVar Omega upat
    tm_cs <- genCaseTmCs2 (Just bind_rhs) [upat] [match_var]
    match_result <- addDictsDs dicts $
                    addTmCsDs tm_cs  $
                      -- See Note [Type and Term Equality Propagation] in Check
                    matchGuards stmts ctx rhs rhs_ty
    core_rhs <- dsLExpr bind_rhs
    match_result' <- matchSinglePatVar match_var (StmtCtxt ctx) pat rhs_ty
                                       match_result
    pure $ adjustMatchResult (bindNonRec match_var core_rhs) match_result'

matchGuards (LastStmt  {} : _) _ _ _ = panic "matchGuards LastStmt"
matchGuards (ParStmt   {} : _) _ _ _ = panic "matchGuards ParStmt"
matchGuards (TransStmt {} : _) _ _ _ = panic "matchGuards TransStmt"
matchGuards (RecStmt   {} : _) _ _ _ = panic "matchGuards RecStmt"
matchGuards (ApplicativeStmt {} : _) _ _ _ =
  panic "matchGuards ApplicativeLastStmt"
matchGuards (XStmtLR {} : _) _ _ _ =
  panic "matchGuards XStmtLR"

{-
Should {\em fail} if @e@ returns @D@
\begin{verbatim}
f x | p <- e', let C y# = e, f y# = r1
    | otherwise          = r2
\end{verbatim}
-}
