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
import TcHsSyn		( SYN_IE(TcGRHSsAndBinds), SYN_IE(TcGRHS) )

import TcMonad
import Inst		( Inst, SYN_IE(LIE), plusLIE )
import Kind             ( mkTypeKind )
import TcBinds		( tcBindsAndThen )
import TcExpr		( tcExpr, tcStmt )
import TcType		( SYN_IE(TcType), TcIdOcc(..), newTyVarTy ) 

import TysWiredIn	( boolTy )
\end{code}

\begin{code}
tcGRHSs :: TcType s -> [RenamedGRHS] -> TcM s ([TcGRHS s], LIE s)

tcGRHSs expected_ty [grhs]
  = tcGRHS expected_ty grhs		`thenTc` \ (grhs', lie) ->
    returnTc ([grhs'], lie)

tcGRHSs expected_ty (grhs:grhss)
  = tcGRHS  expected_ty grhs	`thenTc` \ (grhs',  lie1) ->
    tcGRHSs expected_ty grhss	`thenTc` \ (grhss', lie2) ->
    returnTc (grhs' : grhss', lie1 `plusLIE` lie2)


tcGRHS expected_ty (OtherwiseGRHS expr locn)
  = tcAddSrcLoc locn	 $
    tcExpr expr	expected_ty        `thenTc`    \ (expr, lie) ->
    returnTc (OtherwiseGRHS expr locn, lie)

tcGRHS expected_ty (GRHS guard expr locn)
  = tcAddSrcLoc locn		$
    tc_stmts  guard	`thenTc` \ ((guard', expr'), lie) ->
    returnTc (GRHS guard' expr' locn, lie)
  where
    tc_stmts []		  = tcExpr expr expected_ty	  `thenTc`    \ (expr2, expr_lie) ->
			    returnTc (([], expr2), expr_lie)
    tc_stmts (stmt:stmts) = tcStmt tcExpr ListComp (\x->x) combine stmt $
			    tc_stmts stmts

    combine stmt _ (stmts, expr) = (stmt:stmts, expr)
\end{code}


@tcGRHSsAndBinds@ typechecks (grhss where binds), returning suitable
pieces.

\begin{code}
tcGRHSsAndBinds :: TcType s			-- Expected type of RHSs
		-> RenamedGRHSsAndBinds
		-> TcM s (TcGRHSsAndBinds s, LIE s)

tcGRHSsAndBinds expected_ty (GRHSsAndBindsIn grhss binds)
  = tcBindsAndThen
	 combiner binds
	 (tcGRHSs expected_ty grhss	`thenTc` \ (grhss', lie) ->
	  returnTc (GRHSsAndBindsOut grhss' EmptyBinds expected_ty, lie)
	 )
  where
    combiner is_rec binds1 (GRHSsAndBindsOut grhss binds2 ty)
 	= GRHSsAndBindsOut grhss ((MonoBind binds1 [] is_rec) `ThenBinds` binds2) ty
\end{code}
