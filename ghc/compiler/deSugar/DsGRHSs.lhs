%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsGRHSs]{Matching guarded right-hand-sides (GRHSs)}

\begin{code}
module DsGRHSs ( dsGuarded, dsGRHSs ) where

#include "HsVersions.h"

import {-# SOURCE #-} DsExpr  ( dsExpr, dsLet )
import {-# SOURCE #-} Match   ( matchSinglePat )

import HsSyn		( Stmt(..), HsExpr(..), GRHSs(..), GRHS(..), HsMatchContext(..) )
import TcHsSyn		( TypecheckedGRHSs, TypecheckedPat, TypecheckedStmt, TypecheckedMatchContext )
import CoreSyn		( CoreExpr )
import Type		( Type )

import DsMonad
import DsUtils
import Unique		( Uniquable(..) )
import PrelInfo		( nON_EXHAUSTIVE_GUARDS_ERROR_ID )
import TysWiredIn	( trueDataConId )
import PrelNames	( otherwiseIdKey, hasKey )
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
necessary.  The type argument gives the type of the @ei@.

\begin{code}
dsGuarded :: TypecheckedGRHSs -> DsM CoreExpr

dsGuarded grhss
  = dsGRHSs PatBindRhs [] grhss 				`thenDs` \ (err_ty, match_result) ->
    mkErrorAppDs nON_EXHAUSTIVE_GUARDS_ERROR_ID err_ty ""	`thenDs` \ error_expr ->
    extractMatchResult match_result error_expr
\end{code}

In contrast, @dsGRHSs@ produces a @MatchResult@.

\begin{code}
dsGRHSs :: TypecheckedMatchContext -> [TypecheckedPat]	-- These are to build a MatchContext from
	-> TypecheckedGRHSs				-- Guarded RHSs
	-> DsM (Type, MatchResult)

dsGRHSs kind pats (GRHSs grhss binds ty)
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

-- See comments with HsExpr.Stmt re what an ExprStmt means
-- Here we must be in a guard context (not do-expression, nor list-comp)	

matchGuard [ResultStmt expr locn] ctx 
  = putSrcLocDs locn (dsExpr expr) 	`thenDs` \ core_expr ->
    returnDs (cantFailMatchResult core_expr)

	-- ExprStmts must be guards
	-- Turn an "otherwise" guard is a no-op
matchGuard (ExprStmt (HsVar v) _ _ : stmts) ctx
  |  v `hasKey` otherwiseIdKey
  || v `hasKey` getUnique trueDataConId	
	-- trueDataConId doesn't have the same 
	-- unique as trueDataCon
  = matchGuard stmts ctx

matchGuard (ExprStmt expr _ locn : stmts) ctx
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

Should {\em fail} if @e@ returns @D@
\begin{verbatim}
f x | p <- e', let C y# = e, f y# = r1
    | otherwise 	 = r2 
\end{verbatim}
