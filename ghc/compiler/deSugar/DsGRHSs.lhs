%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsGRHSs]{Matching guarded right-hand-sides (GRHSs)}

\begin{code}
module DsGRHSs ( dsGuarded, dsGRHSs ) where

#include "HsVersions.h"

import {-# SOURCE #-} DsExpr  ( dsExpr, dsLet )
import {-# SOURCE #-} Match   ( matchSinglePat )

import HsSyn		( GRHSsAndBinds(..), Stmt(..), HsExpr(..), GRHS(..) )
import TcHsSyn		( TypecheckedGRHSsAndBinds, TypecheckedGRHS,
			  TypecheckedPat, TypecheckedStmt
			)
import CoreSyn		( CoreExpr, Bind(..) )

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
dsGuarded :: TypecheckedGRHSsAndBinds
	  -> DsM CoreExpr

dsGuarded (GRHSsAndBindsOut grhss binds err_ty)
  = dsGRHSs PatBindMatch [] grhss 				`thenDs` \ match_result ->
    mkErrorAppDs nON_EXHAUSTIVE_GUARDS_ERROR_ID err_ty ""	`thenDs` \ error_expr ->
    extractMatchResult match_result error_expr			`thenDs` \ body ->
    dsLet binds body
\end{code}

Desugar a list of (grhs, expr) pairs [grhs = guarded
right-hand-side], as in:
\begin{verbatim}
p | g1 = e1
  | g2 = e2
  ...
  | gm = em
\end{verbatim}
We supply a @CoreExpr@ for the case in which all of
the guards fail.

\begin{code}
dsGRHSs :: DsMatchKind -> [TypecheckedPat]	-- These are to build a MatchContext from
	-> [TypecheckedGRHS]			-- Guarded RHSs
	-> DsM MatchResult

dsGRHSs kind pats [grhs] = dsGRHS kind pats grhs

dsGRHSs kind pats (grhs:grhss)
  = dsGRHS kind pats grhs	`thenDs` \ match_result1 ->
    dsGRHSs kind pats grhss	`thenDs` \ match_result2 ->
    returnDs (combineMatchResults match_result1 match_result2)

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
