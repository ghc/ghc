%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcGRHSs]{Typecheck guarded right-hand-sides}

\begin{code}
module TcGRHSs ( tcGRHSsAndBinds, tcStmt ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcExpr )

import HsSyn		( HsBinds(..), GRHSsAndBinds(..), GRHS(..), DoOrListComp(..), 
			  Stmt(..),
			  collectPatBinders
			)
import RnHsSyn		( RenamedGRHSsAndBinds, RenamedGRHS, RenamedStmt )
import TcHsSyn		( TcGRHSsAndBinds, TcGRHS, TcStmt )

import TcMonad
import Inst		( Inst, LIE, plusLIE )
import TcBinds		( tcBindsAndThen )
import TcPat		( tcPat )
import TcType		( TcType, newTyVarTy ) 
import TcEnv		( newMonoIds )
import TysWiredIn	( boolTy )
import Kind		( mkTypeKind, mkBoxedTypeKind )
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{GRHSs}
%*									*
%************************************************************************

\begin{code}
tcGRHSs :: TcType s -> [RenamedGRHS] -> TcM s ([TcGRHS s], LIE s)

tcGRHSs expected_ty [grhs]
  = tcGRHS expected_ty grhs		`thenTc` \ (grhs', lie) ->
    returnTc ([grhs'], lie)

tcGRHSs expected_ty (grhs:grhss)
  = tcGRHS  expected_ty grhs	`thenTc` \ (grhs',  lie1) ->
    tcGRHSs expected_ty grhss	`thenTc` \ (grhss', lie2) ->
    returnTc (grhs' : grhss', lie1 `plusLIE` lie2)

tcGRHS expected_ty (GRHS guard expr locn)
  = tcAddSrcLoc locn		$
    tcStmts guard		`thenTc` \ ((guard', expr'), lie) ->
    returnTc (GRHS guard' expr' locn, lie)
  where
    tcStmts []		 = tcExpr expr expected_ty	  `thenTc`    \ (expr2, expr_lie) ->
	                   returnTc (([], expr2), expr_lie)
    tcStmts (stmt:stmts) = tcStmt Guard (\x->x) combine stmt $
			   tcStmts stmts

    combine stmt _ (stmts, expr) = (stmt:stmts, expr)
\end{code}


%************************************************************************
%*									*
\subsection{GRHSsAndBinds}
%*									*
%************************************************************************

@tcGRHSsAndBinds@ typechecks (grhss where binds), returning suitable
pieces.

\begin{code}
tcGRHSsAndBinds :: TcType s			-- Expected type of RHSs
		-> RenamedGRHSsAndBinds
		-> TcM s (TcGRHSsAndBinds s, LIE s)

-- Shortcut for common case
tcGRHSsAndBinds expected_ty (GRHSsAndBindsIn grhss EmptyBinds)	
  = tcGRHSs expected_ty grhss	       `thenTc` \ (grhss', lie) ->
    returnTc (GRHSsAndBindsOut grhss' EmptyBinds expected_ty, lie)

tcGRHSsAndBinds expected_ty (GRHSsAndBindsIn grhss binds)
  = tcBindsAndThen
	 combiner binds
	 (tcGRHSs expected_ty grhss)
  where
    combiner is_rec binds grhss
 	= GRHSsAndBindsOut grhss (MonoBind binds [] is_rec) expected_ty
\end{code}


%************************************************************************
%*									*
\subsection{Record bindings}
%*									*
%************************************************************************


\begin{code}
tcStmt :: DoOrListComp
       -> (TcType s -> TcType s)		-- Relationship type of pat and rhs in pat <- rhs
       -> (TcStmt s -> Maybe (TcType s) -> thing -> thing)
       -> RenamedStmt
       -> TcM s (thing, LIE s)
       -> TcM s (thing, LIE s)

tcStmt do_or_lc m combine stmt@(ReturnStmt exp) do_next
  = ASSERT( case do_or_lc of { DoStmt -> False; ListComp -> True; Guard -> True } )
    tcSetErrCtxt (stmtCtxt do_or_lc stmt) (
        newTyVarTy mkTypeKind                `thenNF_Tc` \ exp_ty ->
	tcExpr exp exp_ty		     `thenTc`    \ (exp', exp_lie) ->
	returnTc (ReturnStmt exp', exp_lie, m exp_ty)
    )					`thenTc` \ (stmt', stmt_lie, stmt_ty) ->
    do_next				`thenTc` \ (thing', thing_lie) ->
    returnTc (combine stmt' (Just stmt_ty) thing',
  	      stmt_lie `plusLIE` thing_lie)

tcStmt do_or_lc m combine stmt@(GuardStmt exp src_loc) do_next
  = ASSERT( case do_or_lc of { DoStmt -> False; ListComp -> True; Guard -> True } )
    newTyVarTy mkTypeKind                    `thenNF_Tc` \ exp_ty ->
    tcAddSrcLoc src_loc 		(
    tcSetErrCtxt (stmtCtxt do_or_lc stmt) (
  	tcExpr exp boolTy 		`thenTc`    \ (exp', exp_lie) ->
  	returnTc (GuardStmt exp' src_loc, exp_lie)
    ))					`thenTc` \ (stmt', stmt_lie) ->
    do_next				`thenTc` \ (thing', thing_lie) ->
    returnTc (combine stmt' Nothing thing',
  	      stmt_lie `plusLIE` thing_lie)

tcStmt do_or_lc m combine stmt@(ExprStmt exp src_loc) do_next
  = ASSERT( case do_or_lc of { DoStmt -> True; ListComp -> False; Guard -> False } )
    newTyVarTy mkTypeKind                    `thenNF_Tc` \ exp_ty ->
    tcAddSrcLoc src_loc 		(
    tcSetErrCtxt (stmtCtxt do_or_lc stmt)	(
  	newTyVarTy mkTypeKind		`thenNF_Tc` \ tau ->
	let
	    -- exp has type (m tau) for some tau (doesn't matter what)
	    exp_ty = m tau
	in
  	tcExpr exp exp_ty		`thenTc`    \ (exp', exp_lie) ->
  	returnTc (ExprStmt exp' src_loc, exp_lie, exp_ty)
    ))					`thenTc` \ (stmt',  stmt_lie, stmt_ty) ->
    do_next				`thenTc` \ (thing', thing_lie) ->
    returnTc (combine stmt' (Just stmt_ty) thing',
  	      stmt_lie `plusLIE` thing_lie)

tcStmt do_or_lc m combine stmt@(BindStmt pat exp src_loc) do_next
  = newMonoIds (collectPatBinders pat) mkBoxedTypeKind $ \ _ ->
    tcAddSrcLoc src_loc		(
    tcSetErrCtxt (stmtCtxt do_or_lc stmt)	(
  	tcPat pat			`thenTc`    \ (pat', pat_lie, pat_ty) ->  
      	tcExpr exp (m pat_ty)		`thenTc`    \ (exp', exp_lie) ->

  	-- NB: the environment has been extended with the new binders
  	-- which the rhs can't "see", but the renamer should have made
  	-- sure that everything is distinct by now, so there's no problem.
  	-- Putting the tcExpr before the newMonoIds messes up the nesting
  	-- of error contexts, so I didn't  bother

  	returnTc (BindStmt pat' exp' src_loc, pat_lie `plusLIE` exp_lie)
    ))					`thenTc` \ (stmt', stmt_lie) ->
    do_next				`thenTc` \ (thing', thing_lie) ->
    returnTc (combine stmt' Nothing thing',
  	      stmt_lie `plusLIE` thing_lie)

tcStmt do_or_lc m combine (LetStmt binds) do_next
     = tcBindsAndThen		-- No error context, but a binding group is
  	combine'		-- rather a large thing for an error context anyway
  	binds
  	do_next
     where
      	combine' is_rec binds' thing' = combine (LetStmt (MonoBind binds' [] is_rec)) Nothing thing'


stmtCtxt do_or_lc stmt
  = hang (ptext SLIT("In a") <+> whatever <> colon)
         4 (ppr stmt)
  where
    whatever = case do_or_lc of
		 ListComp -> ptext SLIT("list-comprehension qualifier")
		 DoStmt   -> ptext SLIT("do statement")
		 Guard	  -> ptext SLIT("guard")
\end{code}
