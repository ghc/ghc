%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcExpr]{TcExpr}

\begin{code}
#include "HsVersions.h"

module TcExpr (
	tcExpr
#ifdef DPH
	, tcExprs
#endif
    ) where

import TcMonad		-- typechecking monad machinery
import TcMonadFns	( newPolyTyVarTy, newOpenTyVarTy,
			  newDict, newMethod, newOverloadedLit,
			  applyTcSubstAndCollectTyVars,
			  mkIdsWithPolyTyVarTys
			)
import AbsSyn		-- the stuff being typechecked


import AbsPrel		( intPrimTy, charPrimTy, doublePrimTy,
			  floatPrimTy, addrPrimTy, addrTy,
			  boolTy, charTy, stringTy, mkFunTy, mkListTy,
			  mkTupleTy, mkPrimIoTy
#ifdef DPH
			 ,mkProcessorTy, mkPodTy,toPodId,
			  processorClass,pidClass
#endif {- Data Parallel Haskell -}
			)
import AbsUniType
import E
import CE		( lookupCE )

import Errors
import GenSpecEtc	( checkSigTyVars )
import Id		( mkInstId, getIdUniType, Id )
import Inst
import LIE		( nullLIE, unitLIE, plusLIE, unMkLIE, mkLIE, LIE )
import ListSetOps	( unionLists )
import Maybes		( Maybe(..) )
import TVE		( nullTVE, TVE(..) )
import Spec		( specId, specTy )
import TcBinds		( tcLocalBindsAndThen )
import TcMatches	( tcMatchesCase, tcMatch )
import TcPolyType	( tcPolyType )
import TcQuals		( tcQuals )
import TcSimplify	( tcSimplifyAndCheck, tcSimplifyRank2 )
#ifdef DPH
import TcParQuals
#endif {- Data Parallel Haskell -}
import Unify		( unifyTauTy, unifyTauTyList, unifyTauTyLists )
import UniqFM		( emptyUFM ) -- profiling, pragmas only
import Unique		-- *Key stuff
import Util

tcExpr :: E -> RenamedExpr -> TcM (TypecheckedExpr, LIE, UniType)
\end{code}

%************************************************************************
%*									*
\subsection{The TAUT rules for variables}
%*									*
%************************************************************************

\begin{code}
tcExpr e (Var name)
  = specId (lookupE_Value e name) `thenNF_Tc` \ stuff@(expr, lie, ty) ->

	-- Check that there's no lurking rank-2 polymorphism
	-- isTauTy is over-paranoid, because we don't expect
	-- any submerged polymorphism other than rank-2 polymorphism

    getSrcLocTc			  `thenNF_Tc` \ loc ->
    checkTc (not (isTauTy ty)) (lurkingRank2Err name ty loc) `thenTc_`
 
    returnTc stuff
\end{code}

%************************************************************************
%*									*
\subsection{Literals}
%*									*
%************************************************************************

Overloaded literals.

\begin{code}
tcExpr e (Lit lit@(IntLit i))
  = getSrcLocTc			`thenNF_Tc` \ loc ->
    newPolyTyVarTy		`thenNF_Tc` \ ty ->
    let
	from_int     = lookupE_ClassOpByKey e numClassKey SLIT("fromInt")
	from_integer = lookupE_ClassOpByKey e numClassKey SLIT("fromInteger")
    in
    newOverloadedLit (LiteralOrigin lit loc)
		     (OverloadedIntegral i from_int from_integer)
		     ty
				`thenNF_Tc` \ over_lit ->

    returnTc (Var (mkInstId over_lit), unitLIE over_lit, ty)

tcExpr e (Lit lit@(FracLit f))
  = getSrcLocTc			`thenNF_Tc` \ loc ->
    newPolyTyVarTy		`thenNF_Tc` \ ty ->
    let
	from_rational = lookupE_ClassOpByKey e fractionalClassKey SLIT("fromRational")
    in
    newOverloadedLit (LiteralOrigin lit loc)
		     (OverloadedFractional f from_rational)
		     ty
				`thenNF_Tc` \ over_lit ->

    returnTc (Var (mkInstId over_lit), unitLIE over_lit, ty)

tcExpr e (Lit lit@(LitLitLitIn s))
  = getSrcLocTc			`thenNF_Tc` \ loc ->
    let
	-- Get the callable class.  Rather turgid and a HACK (ToDo).
	ce 		 = getE_CE e
	cCallableClass   = lookupCE ce (PreludeClass cCallableClassKey   bottom)
	bottom		 = panic "tcExpr:LitLitLit"
    in
    newPolyTyVarTy		`thenNF_Tc` \ ty ->
  
    newDict (LitLitOrigin loc (_UNPK_ s)) cCallableClass ty `thenNF_Tc` \ dict ->

    returnTc (Lit (LitLitLit s ty), mkLIE [dict], ty)
\end{code}

Primitive literals:

\begin{code}
tcExpr e (Lit (CharPrimLit c))
  = returnTc (Lit (CharPrimLit c), nullLIE, charPrimTy)

tcExpr e (Lit (StringPrimLit s))
  = returnTc (Lit (StringPrimLit s), nullLIE, addrPrimTy)

tcExpr e (Lit (IntPrimLit i))
  = returnTc (Lit (IntPrimLit i), nullLIE, intPrimTy)

tcExpr e (Lit (FloatPrimLit f))
  = returnTc (Lit (FloatPrimLit f), nullLIE, floatPrimTy)

tcExpr e (Lit (DoublePrimLit d))
  = returnTc (Lit (DoublePrimLit d), nullLIE, doublePrimTy)
\end{code}

Unoverloaded literals:

\begin{code}
tcExpr e (Lit (CharLit c))
  = returnTc (Lit (CharLit c), nullLIE, charTy)

tcExpr e (Lit (StringLit str))
  = returnTc (Lit (StringLit str), nullLIE, stringTy)
\end{code}

%************************************************************************
%*									*
\subsection{Other expression forms}
%*									*
%************************************************************************

\begin{code}
tcExpr e (Lam match)
  = tcMatch e match	`thenTc` \ (match',lie,ty) ->
    returnTc (Lam match',lie,ty)

tcExpr e (App e1 e2) = accum e1 [e2]
	where
	  accum (App e1 e2) args = accum e1 (e2:args)
	  accum fun         args = tcApp (foldl App) e fun args

-- equivalent to (op e1) e2:
tcExpr e (OpApp e1 op e2)
  = tcApp (\fun [arg1,arg2] -> OpApp arg1 fun arg2) e op [e1,e2]
\end{code}

Note that the operators in sections are expected to be binary, and
a type error will occur if they aren't.

\begin{code}
-- equivalent to 
--	\ x -> e op x, 
-- or
--	\ x -> op e x, 
-- or just
-- 	op e

tcExpr e (SectionL expr op)
  = tcApp (\ fun [arg] -> SectionL arg fun) e op [expr]

-- equivalent to \ x -> x op expr, or
--	\ x -> op x expr

tcExpr e (SectionR op expr)
  = tcExpr e op			`thenTc`    \ (op',  lie1, op_ty) ->
    tcExpr e expr		`thenTc`    \ (expr',lie2, expr_ty) ->
    newOpenTyVarTy		`thenNF_Tc` \ ty1 ->
    newOpenTyVarTy		`thenNF_Tc` \ ty2 ->
    let
	result_ty = mkFunTy ty1 ty2
    in
    unifyTauTy op_ty (mkFunTy ty1 (mkFunTy expr_ty ty2))
		     (SectionRAppCtxt op expr) `thenTc_`

    returnTc (SectionR op' expr', plusLIE lie1 lie2, result_ty)
\end{code}

The interesting thing about @ccall@ is that it is just a template
which we instantiate by filling in details about the types of its
argument and result (ie minimal typechecking is performed).  So, the
basic story is that we allocate a load of type variables (to hold the
arg/result types); unify them with the args/result; and store them for
later use.

\begin{code}
tcExpr e (CCall lbl args may_gc is_asm ignored_fake_result_ty)
  = getSrcLocTc						`thenNF_Tc` \ src_loc ->
    let
	-- Get the callable and returnable classes.  Rather turgid (ToDo).
	ce 		 = getE_CE e
	cCallableClass   = lookupCE ce (PreludeClass cCallableClassKey   bottom)
	cReturnableClass = lookupCE ce (PreludeClass cReturnableClassKey bottom)
	bottom		 = panic "tcExpr:CCall"

	new_arg_dict (arg, arg_ty) = newDict (CCallOrigin src_loc (_UNPK_ lbl) (Just arg)) 
					     cCallableClass arg_ty

	result_origin = CCallOrigin src_loc (_UNPK_ lbl) Nothing {- Not an arg -}
    in
  
	-- Arguments
    tcExprs e args			`thenTc` \ (args', args_lie, arg_tys) ->

	-- The argument types can be unboxed or boxed; the result
	-- type must, however, be boxed since it's an argument to the PrimIO 
	-- type constructor.
    newPolyTyVarTy			  		`thenNF_Tc` \ result_ty ->

	-- Construct the extra insts, which encode the
	-- constraints on the argument and result types.
    mapNF_Tc new_arg_dict (args `zip` arg_tys)			`thenNF_Tc` \ arg_dicts ->
    newDict result_origin cReturnableClass result_ty		`thenNF_Tc` \ res_dict ->
	
    returnTc (CCall lbl args' may_gc is_asm result_ty, 
	      args_lie `plusLIE` mkLIE (res_dict : arg_dicts), 
	      mkPrimIoTy result_ty)
\end{code}

\begin{code}
tcExpr e (SCC label expr)
  = tcExpr e expr		`thenTc` \ (expr', lie, expr_ty) ->
	 -- No unification. Give SCC the type of expr
    returnTc (SCC label expr', lie, expr_ty)

tcExpr e (Let binds expr)
  = tcLocalBindsAndThen e 
	Let 				-- The combiner
	binds 				-- Bindings to check
	(\ e -> tcExpr e expr)		-- Typechecker for the expression

tcExpr e (Case expr matches)
  = tcExpr e expr		`thenTc`    \ (expr',lie1,expr_ty) ->
    tcMatchesCase e matches	`thenTc`    \ (matches',lie2,match_ty) ->
    newOpenTyVarTy		`thenNF_Tc` \ result_ty ->

    unifyTauTy (mkFunTy expr_ty result_ty) match_ty
		(CaseCtxt expr matches)	`thenTc_`

    returnTc (Case expr' matches', plusLIE lie1 lie2, result_ty)

tcExpr e (If pred b1 b2)
  = tcExpr e pred		`thenTc`    \ (pred',lie1,predTy) ->

    unifyTauTy predTy boolTy (PredCtxt pred) `thenTc_`

    tcExpr e b1			`thenTc`    \ (b1',lie2,result_ty) ->
    tcExpr e b2			`thenTc`    \ (b2',lie3,b2Ty) ->

    unifyTauTy result_ty b2Ty (BranchCtxt b1 b2) `thenTc_`

    returnTc (If pred' b1' b2', plusLIE lie1 (plusLIE lie2 lie3), result_ty)

tcExpr e (ListComp expr quals)
  = mkIdsWithPolyTyVarTys binders	`thenNF_Tc` \ lve ->
	 -- Binders of a list comprehension must be boxed.
    let
	new_e = growE_LVE e lve
    in
    tcQuals new_e quals			`thenTc` \ (quals',lie1) ->
    tcExpr  new_e expr			`thenTc` \ (expr', lie2, ty) ->
    returnTc (ListComp expr' quals', plusLIE lie1 lie2, mkListTy ty)
  where
    binders = collectQualBinders quals
\end{code}

\begin{code}
tcExpr e (ExplicitList [])
  = newPolyTyVarTy			`thenNF_Tc` \ tyvar_ty ->
    returnTc (ExplicitListOut tyvar_ty [], nullLIE, mkListTy tyvar_ty)


tcExpr e (ExplicitList exprs)		-- Non-empty list
  = tcExprs e exprs			`thenTc` \ (exprs', lie, tys@(elt_ty:_)) ->
    unifyTauTyList tys (ListCtxt exprs) `thenTc_`
    returnTc (ExplicitListOut elt_ty exprs', lie, mkListTy elt_ty)

tcExpr e (ExplicitTuple exprs)
  = tcExprs e exprs			`thenTc` \ (exprs', lie, tys) ->
    returnTc (ExplicitTuple exprs', lie, mkTupleTy (length tys) tys)

tcExpr e (ArithSeqIn seq@(From expr))
  = getSrcLocTc			`thenNF_Tc` \ loc ->
    tcExpr e expr		`thenTc`    \ (expr', lie, ty) ->
    let
	enum_from_id = lookupE_ClassOpByKey e enumClassKey SLIT("enumFrom")
    in
    newMethod (ArithSeqOrigin seq loc)
	      enum_from_id [ty]	`thenNF_Tc` \ enum_from ->

    returnTc (ArithSeqOut (Var (mkInstId enum_from)) (From expr'),
	      plusLIE (unitLIE enum_from) lie,
	       mkListTy ty)

tcExpr e (ArithSeqIn seq@(FromThen expr1 expr2))
  = getSrcLocTc			`thenNF_Tc` \ loc ->
    tcExpr e expr1		`thenTc`    \ (expr1',lie1,ty1) ->
    tcExpr e expr2		`thenTc`    \ (expr2',lie2,ty2) ->

    unifyTauTyList [ty1, ty2] (ArithSeqCtxt (ArithSeqIn seq)) `thenTc_`
    let
	enum_from_then_id = lookupE_ClassOpByKey e enumClassKey SLIT("enumFromThen")
    in
    newMethod (ArithSeqOrigin seq loc)
	      enum_from_then_id [ty1]	`thenNF_Tc` \ enum_from_then ->

    returnTc (ArithSeqOut (Var (mkInstId enum_from_then))
			   (FromThen expr1' expr2'),
	     (unitLIE enum_from_then) `plusLIE` lie1 `plusLIE` lie2,
	      mkListTy ty1)

tcExpr e (ArithSeqIn seq@(FromTo expr1 expr2))
  = getSrcLocTc			`thenNF_Tc` \ loc ->
    tcExpr e expr1		`thenTc`    \ (expr1',lie1,ty1) ->
    tcExpr e expr2		`thenTc`    \ (expr2',lie2,ty2) ->

    unifyTauTyList [ty1,ty2] (ArithSeqCtxt (ArithSeqIn seq)) `thenTc_`
    let
	enum_from_to_id = lookupE_ClassOpByKey e enumClassKey SLIT("enumFromTo")
    in
    newMethod (ArithSeqOrigin seq loc)
	      enum_from_to_id [ty1]	 `thenNF_Tc` \ enum_from_to ->
    returnTc (ArithSeqOut (Var (mkInstId enum_from_to))
			   (FromTo expr1' expr2'),
	      (unitLIE enum_from_to) `plusLIE` lie1 `plusLIE` lie2,
	       mkListTy ty1)

tcExpr e (ArithSeqIn seq@(FromThenTo expr1 expr2 expr3))
  = getSrcLocTc			`thenNF_Tc` \ loc ->
    tcExpr e expr1		`thenTc`    \ (expr1',lie1,ty1) ->
    tcExpr e expr2		`thenTc`    \ (expr2',lie2,ty2) ->
    tcExpr e expr3		`thenTc`    \ (expr3',lie3,ty3) ->

    unifyTauTyList [ty1,ty2,ty3] (ArithSeqCtxt (ArithSeqIn seq)) `thenTc_`
    let
	enum_from_then_to_id = lookupE_ClassOpByKey e enumClassKey SLIT("enumFromThenTo")
    in
    newMethod (ArithSeqOrigin seq loc)
	      enum_from_then_to_id [ty1] `thenNF_Tc` \ enum_from_then_to ->

    returnTc (ArithSeqOut (Var (mkInstId enum_from_then_to))
			   (FromThenTo expr1' expr2' expr3'),
	      (unitLIE enum_from_then_to) `plusLIE` lie1 `plusLIE` lie2 `plusLIE` lie3,
	       mkListTy ty1)
\end{code}

%************************************************************************
%*									*
\subsection{Expressions type signatures}
%*									*
%************************************************************************

\begin{code}
tcExpr e (ExprWithTySig expr poly_ty)
 = tcExpr e expr					`thenTc` \ (texpr, lie, tau_ty) ->
   babyTcMtoTcM (tcPolyType (getE_CE e) (getE_TCE e) nullTVE poly_ty)	`thenTc` \ sigma_sig ->

	-- Check the tau-type part
   specTy SignatureOrigin sigma_sig	`thenNF_Tc` \ (sig_tyvars, sig_dicts, sig_tau) ->
   unifyTauTy tau_ty sig_tau (ExprSigCtxt expr sig_tau) `thenTc_`

	-- Check the type variables of the signature
   applyTcSubstAndCollectTyVars (tvOfE e) `thenNF_Tc` \ env_tyvars ->
   checkSigTyVars env_tyvars sig_tyvars sig_tau tau_ty (ExprSigCtxt expr sig_tau)
					`thenTc`    \ sig_tyvars' ->

	-- Check overloading constraints
   tcSimplifyAndCheck
	False {- Not top level -}
	env_tyvars sig_tyvars'
	sig_dicts (unMkLIE lie)
	(ExprSigCtxt expr sigma_sig)		`thenTc_`

	-- If everything is ok, return the stuff unchanged, except for
	-- the effect of any substutions etc.  We simply discard the
	-- result of the tcSimplifyAndCheck, except for any default
	-- resolution it may have done, which is recorded in the
	-- substitution.
   returnTc (texpr, lie, tau_ty)
\end{code}

%************************************************************************
%*									*
\subsection{Data Parallel Expressions (DPH only)}
%*									*
%************************************************************************

Constraints enforced by the Static semantics for ParallelZF
$exp_1$ = << $exp_2$ | quals >>

\begin{enumerate}
\item The type of the expression $exp_1$ is <<$exp_2$>>
\item The type of $exp_2$ must be in the class {\tt Processor}
\end{enumerate}

\begin{code}
#ifdef DPH
tcExpr e (ParallelZF expr quals)
 = let binders = collectParQualBinders quals	    in
   mkIdsWithPolyTyVarTys binders	`thenNF_Tc` (\ lve		->
   let e'      = growE_LVE e lve		    in
   tcParQuals e' quals			`thenTc`    (\ (quals',lie1)	->
   tcExpr e' expr			`thenTc`    (\ (expr', lie2,ty) ->
   getSrcLocTc				`thenNF_Tc` (\ src_loc		->
   if (isProcessorTy ty) then
      returnTc (ParallelZF expr' quals',
		 plusLIE lie1 lie2 ,
		 mkPodTy ty)
   else
      failTc (podCompLhsError ty src_loc)
   ))))
#endif {- Data Parallel Haskell -}
\end{code}

Constraints enforced by the Static semantics for Explicit Pods
exp = << $exp_1$ ... $exp_n$>>	(where $n >= 0$)

\begin{enumerate}
\item The type of the all the expressions in the Pod must be the same.
\item The type of an expression in a POD must be in class {\tt Processor}
\end{enumerate}

\begin{code}
#ifdef DPH
tcExpr e (ExplicitPodIn exprs)
 = panic "Ignoring explicit PODs for the time being"
{-
 = tcExprs e exprs			`thenTc`    (\ (exprs',lie,tys) ->
   newPolyTyVarTy			`thenNF_Tc` (\ elt_ty ->
   newDict processorClass elt_ty 	`thenNF_Tc` (\ procDict ->
   let
      procLie = mkLIEFromDicts procDict
   in
   unifyTauTyList (elt_ty:tys) (PodCtxt exprs) `thenTc_`

   returnTc ((App
		(DictApp
		   (TyApp (Var toPodId) [elt_ty])
		   procDict)
		(ExplicitListOut elt_ty exprs')),
	     plusLIE procLie lie,
	     mkPodTy elt_ty)
   ))) -}
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
#ifdef DPH
tcExpr e (ExplicitProcessor exprs expr)
 = tcPidExprs e exprs		`thenTc`	(\ (exprs',lie1,tys) ->
   tcExpr  e expr		`thenTc`	(\ (expr',lie2,ty)   ->
   returnTc (ExplicitProcessor exprs' expr',
	     plusLIE lie1 lie2,
	     mkProcessorTy tys ty)
   ))
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection{@tcExprs@ typechecks a {\em list} of expressions}
%*									*
%************************************************************************

ToDo: Possibly find a version of a listTc TcM which would pass the
appropriate functions for the LIE.

\begin{code}
tcExprs :: E -> [RenamedExpr] -> TcM ([TypecheckedExpr],LIE,[TauType])

tcExprs e [] = returnTc ([], nullLIE, [])
tcExprs e (expr:exprs)
 = tcExpr e expr			`thenTc` \ (expr',  lie1, ty) ->
   tcExprs e exprs			`thenTc` \ (exprs', lie2, tys) ->
   returnTc (expr':exprs', plusLIE lie1 lie2, ty:tys)
\end{code}


%************************************************************************
%*									*
\subsection{@tcApp@ typchecks an application}
%*									*
%************************************************************************

\begin{code}
tcApp	:: (TypecheckedExpr -> [TypecheckedExpr] -> TypecheckedExpr)	-- Result builder
	-> E
	-> RenamedExpr
	-> [RenamedExpr]
	-> TcM (TypecheckedExpr, LIE, UniType)

tcApp build_result_expression e orig_fun arg_exprs
  = tcExpr' e orig_fun (length arg_exprs)
			`thenTc` \ (fun', lie_fun, fun_ty) ->
    unify_fun 1 fun' lie_fun arg_exprs fun_ty
 where
    -- Used only in the error message
    maybe_fun_id = case orig_fun of
			Var name -> Just (lookupE_Value e name)
			other	 -> Nothing

    unify_args	:: Int			-- Current argument number
		-> TypecheckedExpr	-- Current rebuilt expression
		-> LIE			-- Corresponding LIE
		-> [RenamedExpr]	-- Remaining args
		-> [TauType]		-- Remaining arg types
		-> TauType		-- result type
		-> TcM (TypecheckedExpr, LIE, UniType)

    unify_args arg_no fun lie (arg:args) (arg_ty:arg_tys) fun_res_ty
      = tcExpr e arg		`thenTc` \ (arg', lie_arg, actual_arg_ty) ->

	-- These applyTcSubstToTy's are just to improve the error message...
	applyTcSubstToTy actual_arg_ty	`thenNF_Tc` \ actual_arg_ty' -> 
	applyTcSubstToTy arg_ty		`thenNF_Tc` \ arg_ty' -> 
	let
	    err_ctxt = FunAppCtxt orig_fun maybe_fun_id arg arg_ty' actual_arg_ty' arg_no
	in
	matchArgTy e arg_ty' arg' lie_arg actual_arg_ty' err_ctxt
					`thenTc` \ (arg'', lie_arg') ->

	unify_args (arg_no+1) (App fun arg'') (lie `plusLIE` lie_arg') args arg_tys fun_res_ty

    unify_args arg_no fun lie [] arg_tys fun_res_ty
      = -- We've run out of actual arguments.  Check that none of
	-- arg_tys has a for-all at the top. For example, "build" on
	-- its own is no good; it must be applied to something.
	let
	   result_ty = glueTyArgs arg_tys fun_res_ty
	in
	getSrcLocTc	`thenNF_Tc` \ loc ->
	checkTc (not (isTauTy result_ty))
		(underAppliedTyErr result_ty loc) `thenTc_`
	returnTc (fun, lie, result_ty)

    -- When we run out of arg_tys we go back to unify_fun in the hope
    -- that our unification work may have shown up some more arguments
    unify_args arg_no fun lie args [] fun_res_ty
      = unify_fun arg_no fun lie args fun_res_ty


    unify_fun	:: Int			-- Current argument number
		-> TypecheckedExpr	-- Current rebuilt expression
		-> LIE			-- Corresponding LIE
		-> [RenamedExpr]	-- Remaining args
		-> TauType		-- Remaining function type
		-> TcM (TypecheckedExpr, LIE, UniType)

    unify_fun arg_no fun lie args fun_ty
      =		-- Find out as much as possible about the function
	applyTcSubstToTy fun_ty		`thenNF_Tc` \ fun_ty' ->

		-- Now see whether it has any arguments
	case (splitTyArgs fun_ty') of

	  ([], _) ->		-- Function has no arguments left

		newOpenTyVarTy		`thenNF_Tc` \ result_ty ->
		tcExprs e args		`thenTc`    \ (args', lie_args, arg_tys) ->

		-- At this point, a unification error must mean the function is
		-- being applied to too many arguments.
		unifyTauTy fun_ty' (glueTyArgs arg_tys result_ty)
				(TooManyArgsCtxt orig_fun) `thenTc_`

		returnTc (build_result_expression fun args',
			  lie `plusLIE` lie_args,
			  result_ty)

	  (fun_arg_tys, fun_res_ty) ->	-- Function has non-empty list of argument types

		unify_args arg_no fun lie args fun_arg_tys fun_res_ty
\end{code}

\begin{code}
matchArgTy :: E
	 -> UniType		-- Expected argument type
	 -> TypecheckedExpr	-- Type checked argument
	 -> LIE			-- Actual argument LIE
	 -> UniType		-- Actual argument type
	 -> UnifyErrContext  
	 -> TcM (TypecheckedExpr,	-- The incoming type checked arg,
					--  possibly wrapped in a big lambda
		 LIE)			-- Possibly reduced somewhat

matchArgTy e expected_arg_ty arg_expr actual_arg_lie actual_arg_ty err_ctxt 
  | isForAllTy expected_arg_ty
  = -- Ha!  The argument type of the function is a for-all type,
    -- An example of rank-2 polymorphism.

    -- This applyTcSubstToTy is just to improve the error message..

    applyTcSubstToTy actual_arg_ty		`thenNF_Tc` \ actual_arg_ty' ->

    -- Instantiate the argument type
    -- ToDo: give this a real origin
    specTy UnknownOrigin expected_arg_ty	`thenNF_Tc` \ (arg_tyvars, arg_lie, arg_tau) ->

    if not (null arg_lie) then
	    -- Paranoia check
	    panic "Non-null overloading in tcApp"
    else
	    -- Assert: arg_lie = []

    unifyTauTy arg_tau actual_arg_ty' err_ctxt	`thenTc_`

	-- Check that the arg_tyvars havn't been constrained
	-- The interesting bit here is that we must include the free variables
	-- of the expected arg ty.  Here's an example:
	--	 runST (newVar True)
	-- Here, if we don't make a check, we'll get a type (ST s (MutVar s Bool))
	-- for (newVar True), with s fresh.  Then we unify with the runST's arg type
	-- forall s'. ST s' a. That unifies s' with s, and a with MutVar s Bool.
	-- So now s' isn't unconstrained because it's linked to a.
	-- Conclusion: include the free vars of the expected arg type in the
	-- list of "free vars" for the signature check.
    applyTcSubstAndCollectTyVars 
	(tvOfE e 	`unionLists`
	 extractTyVarsFromTy expected_arg_ty)    `thenNF_Tc` \ free_tyvars ->
    checkSigTyVars free_tyvars arg_tyvars arg_tau actual_arg_ty rank2_err_ctxt
					    `thenTc` \ arg_tyvars' ->

	-- Check that there's no overloading involved
	-- Even if there isn't, there may be some Insts which mention the arg_tyvars,
	-- but which, on simplification, don't actually need a dictionary involving
	-- the tyvar.  So we have to do a proper simplification right here.
    let insts = unMkLIE actual_arg_lie
    in
    applyTcSubstToInsts insts		 `thenNF_Tc` \ insts' ->

    tcSimplifyRank2 arg_tyvars' insts' rank2_err_ctxt	`thenTc` \ (free_insts, inst_binds) ->

	-- This Let binds any Insts which came out of the simplification.
	-- It's a bit out of place here, but using AbsBind involves inventing 
	-- a couple of new names which seems worse. 
    returnTc (TyLam arg_tyvars' (Let (mk_binds inst_binds) arg_expr), mkLIE free_insts)

  | otherwise
  = 	-- The ordinary, non-rank-2 polymorphic case
    unifyTauTy expected_arg_ty actual_arg_ty err_ctxt	`thenTc_`
    returnTc (arg_expr, actual_arg_lie)

  where
    rank2_err_ctxt = Rank2ArgCtxt arg_expr expected_arg_ty

    mk_binds [] 		     = EmptyBinds
    mk_binds ((inst,rhs):inst_binds) = (SingleBind (NonRecBind (VarMonoBind (mkInstId inst) rhs)))
					    `ThenBinds`
					    mk_binds inst_binds
\end{code}

This version only does not check for 2nd order if it is applied.

\begin{code}
tcExpr' :: E -> RenamedExpr -> Int -> TcM (TypecheckedExpr,LIE,UniType)

tcExpr' e v@(Var name) n 
      | n > 0 = specId (lookupE_Value e name)	`thenNF_Tc` \ (expr, lie, ty) ->
    returnTc (expr, lie, ty)
tcExpr' e exp n = tcExpr e exp
\end{code}
