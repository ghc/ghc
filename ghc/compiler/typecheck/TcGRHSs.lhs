%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcGRHSs]{Typecheck guarded right-hand-sides}

\begin{code}
module TcGRHSs ( tcGRHSsAndBinds ) where

import TcMonad		-- typechecking monad machinery
import AbsSyn		-- the stuff being typechecked

import AbsPrel		( boolTy )
import E		( growE_LVE, E, LVE(..), TCE(..), UniqFM, CE(..) )
			-- TCE and CE for pragmas only
import Errors		( UnifyErrContext(..) )
import LIE		( plusLIE, LIE )
import TcBinds		( tcLocalBindsAndThen )
import TcExpr		( tcExpr )
import Unify		( unifyTauTy )
import Util		-- pragmas only
\end{code}

\begin{code}
tcGRHSs :: E -> [RenamedGRHS] -> TcM ([TypecheckedGRHS], LIE, UniType)

tcGRHSs e [grhs]
  = tcGRHS e grhs	`thenTc` \ (grhs', lie, ty) ->
    returnTc ([grhs'], lie, ty)

tcGRHSs e gs@(grhs:grhss)
  = tcGRHS  e grhs	`thenTc` \ (grhs',  lie1, ty1) ->
    tcGRHSs e grhss	`thenTc` \ (grhss', lie2, ty2) ->

    unifyTauTy ty1 ty2 (GRHSsBranchCtxt gs) `thenTc_`

    returnTc (grhs' : grhss', lie1 `plusLIE` lie2, ty1)


tcGRHS e (OtherwiseGRHS expr locn)
  = addSrcLocTc locn	 (
    tcExpr e expr	`thenTc` \ (expr, lie, ty) ->
    returnTc (OtherwiseGRHS expr locn, lie, ty)
    )

tcGRHS e (GRHS guard expr locn)
  = addSrcLocTc locn		 (
    tcExpr e guard		`thenTc` \ (guard2, guard_lie, guard_ty) ->

    unifyTauTy guard_ty boolTy (GRHSsGuardCtxt guard) `thenTc_`

    tcExpr e expr		`thenTc` \ (expr2, expr_lie, expr_ty) ->

    returnTc (GRHS guard2 expr2 locn, plusLIE guard_lie expr_lie, expr_ty)
    )
\end{code}


@tcGRHSsAndBinds@ typechecks (grhss where binds), returning suitable
pieces.

\begin{code}
tcGRHSsAndBinds :: E 
		-> RenamedGRHSsAndBinds
		-> TcM (TypecheckedGRHSsAndBinds, LIE, UniType)

tcGRHSsAndBinds e (GRHSsAndBindsIn grhss binds)
  = tcLocalBindsAndThen e 
	 combiner binds 
	 (\e -> tcGRHSs e grhss		`thenTc` (\ (grhss', lie, ty) ->
		returnTc (GRHSsAndBindsOut grhss' EmptyBinds ty, lie, ty) 
		)
	 )
  where
    combiner binds1 (GRHSsAndBindsOut grhss binds2 ty)
	= GRHSsAndBindsOut grhss (binds1 `ThenBinds` binds2) ty
\end{code}
