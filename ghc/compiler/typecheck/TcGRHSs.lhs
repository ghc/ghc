%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcGRHSs]{Typecheck guarded right-hand-sides}

\begin{code}
module TcGRHSs ( tcGRHSsAndBinds, tcStmts ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcExpr )

import HsSyn		( HsBinds(..), GRHSsAndBinds(..), GRHS(..), StmtCtxt(..), 
			  Stmt(..)
			)
import RnHsSyn		( RenamedGRHSsAndBinds, RenamedGRHS, RenamedStmt )
import TcHsSyn		( TcGRHSsAndBinds, TcGRHS, TcStmt )

import TcEnv		( tcExtendGlobalTyVars, tcExtendEnvWithPat )
import TcMonad
import Inst		( LIE, plusLIE )
import TcBinds		( tcBindsAndThen )
import TcSimplify	( tcSimplifyAndCheck )
import TcPat		( tcPat )
import TcMonoType	( checkSigTyVars, noSigs, existentialPatCtxt )
import TcType		( TcType, newTyVarTy ) 
import TysWiredIn	( boolTy )
import Type		( tyVarsOfType, openTypeKind, boxedTypeKind )
import BasicTypes	( RecFlag(..) )
import VarSet
import Bag
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{GRHSs}
%*									*
%************************************************************************

\begin{code}
tcGRHSs :: [RenamedGRHS] -> TcType s -> StmtCtxt -> TcM s ([TcGRHS s], LIE s)

tcGRHSs [grhs] expected_ty ctxt
  = tcGRHS grhs expected_ty ctxt	`thenTc` \ (grhs', lie) ->
    returnTc ([grhs'], lie)

tcGRHSs (grhs:grhss) expected_ty ctxt
  = tcGRHS  grhs  expected_ty ctxt 	`thenTc` \ (grhs',  lie1) ->
    tcGRHSs grhss expected_ty ctxt 	`thenTc` \ (grhss', lie2) ->
    returnTc (grhs' : grhss', lie1 `plusLIE` lie2)

tcGRHS (GRHS guarded locn) expected_ty ctxt
  = tcAddSrcLoc locn					$
    tcStmts ctxt (\ty -> ty) guarded expected_ty	`thenTc` \ (guarded', lie) ->
    returnTc (GRHS guarded' locn, lie)
\end{code}


%************************************************************************
%*									*
\subsection{GRHSsAndBinds}
%*									*
%************************************************************************

@tcGRHSsAndBinds@ typechecks (grhss where binds), returning suitable
pieces.

\begin{code}
tcGRHSsAndBinds :: RenamedGRHSsAndBinds
		-> TcType s			-- Expected type of RHSs
		-> StmtCtxt 
		-> TcM s (TcGRHSsAndBinds s, LIE s)

tcGRHSsAndBinds (GRHSsAndBindsIn grhss binds) expected_ty ctxt
  = tcBindsAndThen
	 combiner binds
	 (tcGRHSs grhss expected_ty ctxt	`thenTc` \ (grhss, lie) ->
	  returnTc (GRHSsAndBindsOut grhss EmptyBinds expected_ty, lie))
  where
    combiner is_rec mbinds (GRHSsAndBindsOut grhss binds expected_ty)
 	= GRHSsAndBindsOut grhss (MonoBind mbinds [] is_rec `ThenBinds` binds) expected_ty
\end{code}


%************************************************************************
%*									*
\subsection{Record bindings}
%*									*
%************************************************************************


\begin{code}
tcStmts :: StmtCtxt
        -> (TcType s -> TcType s)	-- m, the relationship type of pat and rhs in pat <- rhs
        -> [RenamedStmt]
	-> TcType s			-- elt_ty, where type of the comprehension is (m elt_ty)
        -> TcM s ([TcStmt s], LIE s)

tcStmts do_or_lc m (stmt@(ReturnStmt exp) : stmts) elt_ty
  = ASSERT( null stmts )
    tcSetErrCtxt (stmtCtxt do_or_lc stmt) 	$
    tcExpr exp elt_ty				`thenTc`    \ (exp', exp_lie) ->
    returnTc ([ReturnStmt exp'], exp_lie)

	-- ExprStmt at the end
tcStmts do_or_lc m [stmt@(ExprStmt exp src_loc)] elt_ty
  = tcSetErrCtxt (stmtCtxt do_or_lc stmt) 	$
    tcExpr exp (m elt_ty)			`thenTc`    \ (exp', exp_lie) ->
    returnTc ([ExprStmt exp' src_loc], exp_lie)

	-- ExprStmt not at the end
tcStmts do_or_lc m (stmt@(ExprStmt exp src_loc) : stmts) elt_ty
  = ASSERT( isDoStmt do_or_lc )
    tcAddSrcLoc src_loc 		(
	tcSetErrCtxt (stmtCtxt do_or_lc stmt)	$
	    -- exp has type (m tau) for some tau (doesn't matter what)
  	newTyVarTy openTypeKind			`thenNF_Tc` \ any_ty ->
  	tcExpr exp (m any_ty)
    )					`thenTc` \ (exp', exp_lie) ->
    tcStmts do_or_lc m stmts elt_ty	`thenTc` \ (stmts', stmts_lie) ->
    returnTc (ExprStmt exp' src_loc : stmts',
  	      exp_lie `plusLIE` stmts_lie)

tcStmts do_or_lc m (stmt@(GuardStmt exp src_loc) : stmts) elt_ty
  = ASSERT( not (isDoStmt do_or_lc) )
    tcSetErrCtxt (stmtCtxt do_or_lc stmt) (
	tcAddSrcLoc src_loc 		$
  	tcExpr exp boolTy
    )					`thenTc` \ (exp', exp_lie) ->
    tcStmts do_or_lc m stmts elt_ty	`thenTc` \ (stmts', stmts_lie) ->
    returnTc (GuardStmt exp' src_loc : stmts',
  	      exp_lie `plusLIE` stmts_lie)

tcStmts do_or_lc m (stmt@(BindStmt pat exp src_loc) : stmts) elt_ty
  = tcAddSrcLoc src_loc		(
	tcSetErrCtxt (stmtCtxt do_or_lc stmt)	$
    	newTyVarTy boxedTypeKind		`thenNF_Tc` \ pat_ty ->
  	tcPat noSigs pat pat_ty			`thenTc` \ (pat', pat_lie, pat_tvs, pat_ids, avail) ->  
      	tcExpr exp (m pat_ty)			`thenTc` \ (exp', exp_lie) ->
  	returnTc (pat', exp',
		  pat_lie `plusLIE` exp_lie,
		  pat_tvs, pat_ids, avail)
    )					`thenTc` \ (pat', exp', lie_req, pat_tvs, pat_ids, lie_avail) ->

	-- Do the rest; we don't need to add the pat_tvs to the envt
	-- because they all appear in the pat_ids's types
    tcExtendEnvWithPat pat_ids (
       tcStmts do_or_lc m stmts elt_ty
    )						`thenTc` \ (stmts', stmts_lie) ->


	-- Reinstate context for existential checks
    tcSetErrCtxt (stmtCtxt do_or_lc stmt)		$
    tcExtendGlobalTyVars (tyVarsOfType (m elt_ty))	$
    tcAddErrCtxtM (existentialPatCtxt pat_tvs pat_ids)	$

    checkSigTyVars (bagToList pat_tvs)			`thenTc` \ zonked_pat_tvs ->

    tcSimplifyAndCheck 
	(text ("the existential context of a data constructor"))
	(mkVarSet zonked_pat_tvs)
	lie_avail stmts_lie			`thenTc` \ (final_lie, dict_binds) ->

    returnTc (BindStmt pat' exp' src_loc : 
	        LetStmt (MonoBind dict_binds [] Recursive) :
	          stmts',
  	      lie_req `plusLIE` final_lie)

tcStmts do_or_lc m (LetStmt binds : stmts) elt_ty
     = tcBindsAndThen		-- No error context, but a binding group is
  	combine			-- rather a large thing for an error context anyway
  	binds
  	(tcStmts do_or_lc m stmts elt_ty)
     where
      	combine is_rec binds' stmts' = LetStmt (MonoBind binds' [] is_rec) : stmts'


isDoStmt DoStmt = True
isDoStmt other  = False

stmtCtxt do_or_lc stmt
  = hang (ptext SLIT("In") <+> what <> colon)
         4 (ppr stmt)
  where
    what = case do_or_lc of
		ListComp -> ptext SLIT("a list-comprehension qualifier")
		DoStmt   -> ptext SLIT("a do statement:")
		PatBindRhs -> thing <+> ptext SLIT("a pattern binding")
		FunRhs f   -> thing <+> ptext SLIT("an equation for") <+> quotes (ppr f)
		CaseAlt	   -> thing <+> ptext SLIT("a case alternative")
		LambdaBody -> thing <+> ptext SLIT("a lambda abstraction")
    thing = case stmt of
		BindStmt _ _ _ -> ptext SLIT("a pattern guard for")
		GuardStmt _ _  -> ptext SLIT("a guard for")
		ExprStmt _ _   -> ptext SLIT("the right-hand side of")
\end{code}
