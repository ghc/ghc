%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcGRHSs]{Typecheck guarded right-hand-sides}

\begin{code}
module TcGRHSs ( tcGRHSsAndBinds ) where

#include "HsVersions.h"

import HsSyn		( HsBinds(..), GRHSsAndBinds(..), GRHS(..), DoOrListComp(..) )
import RnHsSyn		( RenamedGRHSsAndBinds, RenamedGRHS )
import TcHsSyn		( TcGRHSsAndBinds, TcGRHS )

import TcMonad
import Inst		( Inst, LIE, plusLIE )
import TcBinds		( tcBindsAndThen )
import TcExpr		( tcExpr, tcStmt )
import TcType		( TcType, newTyVarTy ) 
import TcEnv		( TcIdOcc(..) )

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

tcGRHS expected_ty (GRHS guard expr locn)
  = tcAddSrcLoc locn		$
    tcStmts guard		`thenTc` \ ((guard', expr'), lie) ->
    returnTc (GRHS guard' expr' locn, lie)
  where
    tcStmts []		 = tcExpr expr expected_ty	  `thenTc`    \ (expr2, expr_lie) ->
	                   returnTc (([], expr2), expr_lie)
    tcStmts (stmt:stmts) = tcStmt tcExpr Guard (\x->x) combine stmt $
			   tcStmts stmts

    combine stmt _ (stmts, expr) = (stmt:stmts, expr)
\end{code}


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
