%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcGRHSs]{Typecheck guarded right-hand-sides}

\begin{code}
#include "HsVersions.h"

module TcGRHSs ( tcGRHSsAndBinds ) where

IMP_Ubiq(){-uitous-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(TcLoop) -- for paranoia checking
#endif

import HsSyn		( GRHSsAndBinds(..), GRHS(..), MonoBinds, Stmt, DoOrListComp(..),
			  HsExpr, HsBinds(..), InPat, OutPat, Sig, Fake )
import RnHsSyn		( SYN_IE(RenamedGRHSsAndBinds), SYN_IE(RenamedGRHS) )
import TcHsSyn		( SYN_IE(TcGRHSsAndBinds), SYN_IE(TcGRHS), TcIdOcc(..) )

import TcMonad
import Inst		( Inst, SYN_IE(LIE), plusLIE )
import TcBinds		( tcBindsAndThen )
import TcExpr		( tcExpr, tcStmt )
import TcType		( SYN_IE(TcType) ) 
import Unify		( unifyTauTy )

import TysWiredIn	( boolTy )
\end{code}

\begin{code}
tcGRHSs :: [RenamedGRHS] -> TcM s ([TcGRHS s], LIE s, TcType s)

tcGRHSs [grhs]
  = tcGRHS grhs		`thenTc` \ (grhs', lie, ty) ->
    returnTc ([grhs'], lie, ty)

tcGRHSs (grhs:grhss)
  = tcGRHS  grhs	`thenTc` \ (grhs',  lie1, ty1) ->
    tcGRHSs grhss	`thenTc` \ (grhss', lie2, ty2) ->
    unifyTauTy ty1 ty2	`thenTc_`
    returnTc (grhs' : grhss', lie1 `plusLIE` lie2, ty1)


tcGRHS (OtherwiseGRHS expr locn)
  = tcAddSrcLoc locn	 $
    tcExpr expr	`thenTc` \ (expr, lie, ty) ->
    returnTc (OtherwiseGRHS expr locn, lie, ty)

tcGRHS (GRHS guard expr locn)
  = tcAddSrcLoc locn		$
    tc_stmts  guard	`thenTc` \ ((guard', expr', ty), lie) ->
    returnTc (GRHS guard' expr' locn, lie, ty)
  where
    tc_stmts []		  = tcExpr expr 	`thenTc` \ (expr2, expr_lie, expr_ty) ->
			    returnTc (([], expr2, expr_ty), expr_lie)
    tc_stmts (stmt:stmts) = tcStmt tcExpr ListComp (\x->x) combine stmt $
			    tc_stmts stmts

    combine stmt _ (stmts, expr, ty) = (stmt:stmts, expr, ty)
\end{code}


@tcGRHSsAndBinds@ typechecks (grhss where binds), returning suitable
pieces.

\begin{code}
tcGRHSsAndBinds :: RenamedGRHSsAndBinds
		-> TcM s (TcGRHSsAndBinds s, LIE s, TcType s)

tcGRHSsAndBinds (GRHSsAndBindsIn grhss binds)
  = tcBindsAndThen
	 combiner binds
	 (tcGRHSs grhss		`thenTc` \ (grhss', lie, ty) ->
	  returnTc (GRHSsAndBindsOut grhss' EmptyBinds ty, lie)
	 )			`thenTc` \ (grhss_and_binds'@(GRHSsAndBindsOut _ _ result_ty), lie) ->
    returnTc (grhss_and_binds', lie, result_ty)
  where
    combiner is_rec binds1 (GRHSsAndBindsOut grhss binds2 ty) 
 	= GRHSsAndBindsOut grhss ((MonoBind binds1 [] is_rec) `ThenBinds` binds2) ty
\end{code}
