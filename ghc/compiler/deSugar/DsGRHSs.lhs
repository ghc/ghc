%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsGRHSs]{Matching guarded right-hand-sides (GRHSs)}

\begin{code}
module DsGRHSs ( dsGuarded, dsGRHSs ) where

#include "HsVersions.h"

import {-# SOURCE #-} DsExpr  ( dsExpr, dsLet )
import {-# SOURCE #-} Match   ( matchSinglePat )

import HsSyn		( Stmt(..), HsExpr(..), GRHSs(..), GRHS(..) )
import TcHsSyn		( TypecheckedGRHSs, TypecheckedPat, TypecheckedStmt )
import CoreSyn		( CoreExpr, Bind(..) )
import Type		( Type )

import DsMonad
import DsUtils
import PrelVals		( nON_EXHAUSTIVE_GUARDS_ERROR_ID )
import Unique		( otherwiseIdKey, trueDataConKey, Uniquable(..) )
import Outputable
\end{code}

@dsGuarded@ is used for both @case@ expressions and pattern bindings.
It desugars:
\begin{verbatim}
	| g1 -> e1
	...
	| gn -> en
	where binds
\end{verbatim}
producing an expression with a runtime error in the corner if
necessary.  The type argument gives the type of the ei.

\begin{code}
dsGuarded :: TypecheckedGRHSs -> DsM CoreExpr

dsGuarded grhss
  = dsGRHSs PatBindMatch [] grhss 				`thenDs` \ (err_ty, match_result) ->
    mkErrorAppDs nON_EXHAUSTIVE_GUARDS_ERROR_ID err_ty ""	`thenDs` \ error_expr ->
    extractMatchResult match_result error_expr
\end{code}

In contrast, @dsGRHSs@ produces a @MatchResult@.

\begin{code}
dsGRHSs :: DsMatchKind -> [TypecheckedPat]	-- These are to build a MatchContext from
	-> TypecheckedGRHSs			-- Guarded RHSs
	-> DsM (Type, MatchResult)

dsGRHSs kind pats (GRHSs grhss binds (Just ty))
  = mapDs (dsGRHS kind pats) grhss		`thenDs` \ match_results ->
    let 
	match_result1 = foldr1 combineMatchResults match_results
	match_result2 = adjustMatchResultDs (dsLet binds) match_result1
		-- NB: nested dsLet inside matchResult
    in
    returnDs (ty, match_result2)

dsGRHS kind pats (GRHS guard locn)
  = matchGuard guard (DsMatchContext kind pats locn)
\end{code}


%************************************************************************
%*									*
%*  matchGuard : make a MatchResult from a guarded RHS			*
%*									*
%************************************************************************

\begin{code}
matchGuard :: [TypecheckedStmt] 	-- Guard
           -> DsMatchContext            -- Context
	   -> DsM MatchResult

matchGuard (ExprStmt expr locn : should_be_null) ctx 
  = putSrcLocDs locn (dsExpr expr) 	`thenDs` \ core_expr ->
    returnDs (cantFailMatchResult core_expr)

	-- Turn an "otherwise" guard is a no-op
matchGuard (GuardStmt (HsVar v) _ : stmts) ctx
  |  uniq == otherwiseIdKey
  || uniq == trueDataConKey
  = matchGuard stmts ctx
  where
    uniq = getUnique v

matchGuard (GuardStmt expr locn : stmts) ctx
  = matchGuard stmts ctx 		`thenDs` \ match_result ->
    putSrcLocDs locn (dsExpr expr)	`thenDs` \ pred_expr ->
    returnDs (mkGuardedMatchResult pred_expr match_result)

matchGuard (LetStmt binds : stmts) ctx
  = matchGuard stmts ctx 	`thenDs` \ match_result ->
    returnDs (adjustMatchResultDs (dsLet binds) match_result)
	-- NB the dsLet occurs inside the match_result

matchGuard (BindStmt pat rhs locn : stmts) ctx
  = matchGuard stmts ctx 		`thenDs` \ match_result ->
    putSrcLocDs locn (dsExpr rhs)	`thenDs` \ core_rhs ->
    matchSinglePat core_rhs ctx pat match_result
\end{code}

-- Should *fail* if e returns D

f x | p <- e', let C y# = e, f y# = r1
    | otherwise 	 = r2 
