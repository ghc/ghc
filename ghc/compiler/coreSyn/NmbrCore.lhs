%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[NmbrCore]{Renumber Core for printing}

\begin{code}
#include "HsVersions.h"

module NmbrCore where

IMP_Ubiq(){-uitous-}

import PprEnv		( NmbrEnv )
\end{code}

\begin{code}
nmbrCoreBindings :: [CoreBinding] -> NmbrEnv -> (NmbrEnv, [CoreBinding])

nmbr_bind :: CoreBinding -> NmbrEnv -> (NmbrEnv, CoreBinding)
nmbr_expr :: CoreExpr    -> NmbrEnv -> (NmbrEnv, CoreExpr)
nmbr_arg  :: CoreArg     -> NmbrEnv -> (NmbrEnv, CoreArg)

nmbrCoreBindings nenv [] = (nenv, [])
nmbrCoreBindings nenv (b:bs)
  = let
	(new_nenv, new_b)  = nmbr_bind        nenv     b
	(fin_nenv, new_bs) = nmbrCoreBindings new_nenv bs
    in
    (fin_nenv, new_b : new_bs)

nmbr_bind nenv (NonRec binder rhs)
    -- remember, binder cannot appear in rhs
  = let
	(_,     new_rhs)    = nmbr_expr nenv rhs
	(nenv2, new_binder) = addId     nenv binder
    in
    (nenv2, NonRec new_binder new_rhs)

nmbr_bind nenv (Rec binds)
  = -- for letrec, we plug in new bindings BEFORE cloning rhss
    let
	(binders, rhss)	     = unzip binds

	(nenv2, new_binders) = mapAccumL addId nenv binders

	(_, new_rhss)	     = mapAndUnzip (nmbr_expr nenv2) rhss
    in
    returnUs (nenv2, Rec (zipEqual "nmbr_bind" new_binders new_rhss))
\end{code}

\begin{code}
nmbr_arg nenv (VarArg v)
  = let
	(nenv2, new_v) = nmbrId nenv v
    in
    (nenv2, VarArg new_v)

nmbr_arg nenv (TyArg ty)
  = let
	(nenv2, new_ty) = nmbrType nenv ty
    in
    (nenv2, TyArg new_ty)

nmbr_arg nenv (UsageArg use)
  = let
	(nenv2, new_use) = nmbrUsage nenv use
    in
    (nenv2, UsageArg new_use)
\end{code}

\begin{code}
nmbr_expr :: NmbrEnv
	    -> TypeEnv
	    -> CoreExpr
	    -> UniqSM CoreExpr

nmbr_expr nenv tenv orig_expr@(Var var)
  = returnUs (
      case (lookupIdEnv nenv var) of
	Nothing	    -> --false:ASSERT(toplevelishId var) (SIGH)
		       orig_expr
	Just expr   -> expr
    )

nmbr_expr nenv tenv e@(Lit _) = returnUs e

nmbr_expr nenv tenv (Con con as)
  = mapUs  (nmbr_arg nenv tenv) as `thenUs`  \ new_as ->
    mkCoCon con new_as

nmbr_expr nenv tenv (Prim op as)
  = mapUs  (nmbr_arg nenv tenv) as 	`thenUs`  \ new_as ->
    do_PrimOp op			`thenUs`  \ new_op ->
    mkCoPrim new_op new_as
  where
    do_PrimOp (CCallOp label is_asm may_gc arg_tys result_ty)
      = let
	    new_arg_tys   = map (applyTypeEnvToTy tenv) arg_tys
	    new_result_ty = applyTypeEnvToTy tenv result_ty
	in
	returnUs (CCallOp label is_asm may_gc new_arg_tys new_result_ty)

    do_PrimOp other_op = returnUs other_op

nmbr_expr nenv tenv (Lam binder expr)
  = dup_binder tenv binder `thenUs` \(new_binder, (old,new)) ->
    let  new_nenv = addOneToIdEnv nenv old new  in
    nmbr_expr new_nenv tenv expr  `thenUs` \ new_expr ->
    returnUs (Lam new_binder new_expr)

nmbr_expr nenv tenv (App expr arg)
  = nmbr_expr nenv tenv expr	`thenUs` \ new_expr ->
    nmbr_arg  nenv tenv arg   `thenUs` \ new_arg  ->
    mkCoApps new_expr [new_arg] -- ToDo: more efficiently?

nmbr_expr nenv tenv (Case expr alts)
  = nmbr_expr nenv tenv expr	    `thenUs` \ new_expr ->
    do_alts nenv tenv alts	    `thenUs` \ new_alts ->
    returnUs (Case new_expr new_alts)
  where
    do_alts nenv tenv (AlgAlts alts deflt)
      = mapUs (do_boxed_alt nenv tenv) alts `thenUs` \ new_alts ->
    	do_default nenv tenv deflt	    `thenUs` \ new_deflt ->
	returnUs (AlgAlts new_alts new_deflt)
      where
	do_boxed_alt nenv tenv (con, binders, expr)
	  = mapAndUnzipUs (dup_binder tenv) binders `thenUs` \ (new_binders, new_vmaps) ->
	    let  new_nenv = growIdEnvList nenv new_vmaps  in
	    nmbr_expr new_nenv tenv expr  `thenUs` \ new_expr ->
	    returnUs (con, new_binders, new_expr)


    do_alts nenv tenv (PrimAlts alts deflt)
      = mapUs (do_unboxed_alt nenv tenv) alts `thenUs` \ new_alts ->
    	do_default nenv tenv deflt	      `thenUs` \ new_deflt ->
	returnUs (PrimAlts new_alts new_deflt)
      where
	do_unboxed_alt nenv tenv (lit, expr)
	  = nmbr_expr nenv tenv expr	`thenUs` \ new_expr ->
	    returnUs (lit, new_expr)

    do_default nenv tenv NoDefault = returnUs NoDefault

    do_default nenv tenv (BindDefault binder expr)
      =	dup_binder tenv binder		`thenUs` \ (new_binder, (old, new)) ->
	let  new_nenv = addOneToIdEnv nenv old new  in
	nmbr_expr new_nenv tenv expr	`thenUs` \ new_expr ->
	returnUs (BindDefault new_binder new_expr)

nmbr_expr nenv tenv (Let core_bind expr)
  = nmbr_bind nenv tenv core_bind	`thenUs` \ (new_bind, new_nenv) ->
    -- and do the body of the let
    nmbr_expr new_nenv tenv expr  	`thenUs` \ new_expr ->
    returnUs (Let new_bind new_expr)

nmbr_expr nenv tenv (SCC label expr)
  = nmbr_expr nenv tenv expr	    	`thenUs` \ new_expr ->
    returnUs (SCC label new_expr)

nmbr_expr nenv tenv (Coerce c ty expr)
  = nmbr_expr nenv tenv expr	    	`thenUs` \ new_expr ->
    returnUs (Coerce c ty new_expr)
\end{code}
