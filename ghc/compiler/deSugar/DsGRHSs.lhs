%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsGRHSs]{Matching guarded right-hand-sides (GRHSs)}

\begin{code}
module DsGRHSs ( dsGuarded, dsGRHSs ) where

#include "HsVersions.h"

import {-# SOURCE #-} DsExpr  ( dsLExpr, dsLet )
import {-# SOURCE #-} Match   ( matchSinglePat )

import HsSyn		( Stmt(..), HsExpr(..), GRHSs(..), GRHS(..), 
			  LHsExpr, HsMatchContext(..), Pat(..) )
import CoreSyn		( CoreExpr )
import Var		( Id )
import Type		( Type )

import DsMonad
import DsUtils
import Unique		( Uniquable(..) )
import PrelInfo		( nON_EXHAUSTIVE_GUARDS_ERROR_ID )
import TysWiredIn	( trueDataConId )
import PrelNames	( otherwiseIdKey, hasKey )
import Name		( Name )
import SrcLoc		( unLoc, Located(..) )
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
dsGuarded :: GRHSs Id -> Type -> DsM CoreExpr

dsGuarded grhss rhs_ty
  = dsGRHSs PatBindRhs [] grhss rhs_ty 				`thenDs` \ match_result ->
    mkErrorAppDs nON_EXHAUSTIVE_GUARDS_ERROR_ID rhs_ty ""	`thenDs` \ error_expr ->
    extractMatchResult match_result error_expr
\end{code}

In contrast, @dsGRHSs@ produces a @MatchResult@.

\begin{code}
dsGRHSs :: HsMatchContext Name -> [Pat Id]	-- These are to build a MatchContext from
	-> GRHSs Id				-- Guarded RHSs
	-> Type					-- Type of RHS
	-> DsM MatchResult

dsGRHSs kind pats (GRHSs grhss binds) rhs_ty
  = mappM (dsGRHS kind pats rhs_ty) grhss	`thenDs` \ match_results ->
    let 
	match_result1 = foldr1 combineMatchResults match_results
	match_result2 = adjustMatchResultDs (dsLet binds) match_result1
		-- NB: nested dsLet inside matchResult
    in
    returnDs match_result2

dsGRHS kind pats rhs_ty (L loc (GRHS guards rhs))
  = matchGuard (map unLoc guards) (DsMatchContext kind pats loc)
	       rhs rhs_ty
\end{code}


%************************************************************************
%*									*
%*  matchGuard : make a MatchResult from a guarded RHS			*
%*									*
%************************************************************************

\begin{code}
matchGuard :: [Stmt Id] 	-- Guard
           -> DsMatchContext	-- Context
	   -> LHsExpr Id	-- RHS
	   -> Type		-- Type of RHS of guard
	   -> DsM MatchResult

-- See comments with HsExpr.Stmt re what an ExprStmt means
-- Here we must be in a guard context (not do-expression, nor list-comp)	

matchGuard [] ctx rhs rhs_ty
  = do	{ core_rhs <- dsLExpr rhs
	; return (cantFailMatchResult core_rhs) }

	-- ExprStmts must be guards
	-- Turn an "otherwise" guard is a no-op
matchGuard (ExprStmt (L _ (HsVar v)) _ _ : stmts) ctx rhs rhs_ty
  |  v `hasKey` otherwiseIdKey
  || v `hasKey` getUnique trueDataConId	
	-- trueDataConId doesn't have the same 
	-- unique as trueDataCon
  = matchGuard stmts ctx rhs rhs_ty

matchGuard (ExprStmt expr _ _ : stmts) ctx rhs rhs_ty
  = matchGuard stmts ctx rhs rhs_ty	`thenDs` \ match_result ->
    dsLExpr expr			`thenDs` \ pred_expr ->
    returnDs (mkGuardedMatchResult pred_expr match_result)

matchGuard (LetStmt binds : stmts) ctx rhs rhs_ty
  = matchGuard stmts ctx rhs rhs_ty	`thenDs` \ match_result ->
    returnDs (adjustMatchResultDs (dsLet binds) match_result)
	-- NB the dsLet occurs inside the match_result
	-- Reason: dsLet takes the body expression as its argument
	--	   so we can't desugar the bindings without the
	--	   body expression in hand

matchGuard (BindStmt pat bind_rhs _ _ : stmts) ctx rhs rhs_ty
  = matchGuard stmts ctx rhs rhs_ty	`thenDs` \ match_result ->
    dsLExpr bind_rhs			`thenDs` \ core_rhs ->
    matchSinglePat core_rhs ctx pat rhs_ty match_result
\end{code}

Should {\em fail} if @e@ returns @D@
\begin{verbatim}
f x | p <- e', let C y# = e, f y# = r1
    | otherwise 	 = r2 
\end{verbatim}
