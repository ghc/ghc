%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcExpr]{Typecheck an expression}

\begin{code}
#include "HsVersions.h"

module TcExpr ( tcExpr ) where

import Ubiq

import HsSyn		( HsExpr(..), Qual(..), Stmt(..),
			  HsBinds(..), Bind(..), MonoBinds(..), 
			  ArithSeqInfo(..), HsLit(..), Sig, GRHSsAndBinds,
			  Match, Fake, InPat, OutPat, PolyType,
			  irrefutablePat, collectPatBinders )
import RnHsSyn		( RenamedHsExpr(..), RenamedQual(..),
			  RenamedStmt(..), RenamedRecordBinds(..),
			  RnName{-instance Outputable-}
			)
import TcHsSyn		( TcExpr(..), TcQual(..), TcStmt(..),
			  TcIdOcc(..), TcRecordBinds(..),
			  mkHsTyApp
			)

import TcMonad
import Inst		( Inst, InstOrigin(..), OverloadedLit(..),
			  LIE(..), emptyLIE, plusLIE, plusLIEs, newOverloadedLit,
			  newMethod, newMethodWithGivenTy, newDicts )
import TcBinds		( tcBindsAndThen )
import TcEnv		( tcLookupLocalValue, tcLookupGlobalValue, tcLookupClassByKey,
			  tcLookupGlobalValueByKey, newMonoIds, tcGetGlobalTyVars,
			  tcGlobalOcc
			)
import TcMatches	( tcMatchesCase, tcMatch )
import TcMonoType	( tcPolyType )
import TcPat		( tcPat )
import TcSimplify	( tcSimplifyAndCheck, tcSimplifyRank2 )
import TcType		( TcType(..), TcMaybe(..),
			  tcInstType, tcInstTcType, tcInstTyVars,
			  newTyVarTy, zonkTcTyVars, zonkTcType )
import TcKind		( TcKind )

import Class		( Class(..), getClassSig )
import FieldLabel	( fieldLabelName )
import Id		( Id(..), GenId, idType, dataConFieldLabels )
import Kind		( Kind, mkBoxedTypeKind, mkTypeKind, mkArrowKind )
import GenSpecEtc	( checkSigTyVars, checkSigTyVarsGivenGlobals )
import Name		( Name{-instance Eq-} )
import PrelInfo		( intPrimTy, charPrimTy, doublePrimTy,
			  floatPrimTy, addrPrimTy, addrTy,
			  boolTy, charTy, stringTy, mkListTy,
			  mkTupleTy, mkPrimIoTy )
import Type		( mkFunTy, mkAppTy, mkTyVarTy, mkTyVarTys,
			  getTyVar_maybe, getFunTy_maybe,
			  splitForAllTy, splitRhoTy, splitSigmaTy, splitFunTy,
			  isTauTy, mkFunTys, tyVarsOfType, getForAllTy_maybe,
			  maybeAppDataTyCon
			)
import TyVar		( GenTyVar, TyVarSet(..), unionTyVarSets, mkTyVarSet )
import Unify		( unifyTauTy, unifyTauTyList, unifyTauTyLists, unifyFunTy )
import Unique		( Unique, cCallableClassKey, cReturnableClassKey, 
			  enumFromClassOpKey, enumFromThenClassOpKey,
			  enumFromToClassOpKey, enumFromThenToClassOpKey,
			  monadClassKey, monadZeroClassKey )

--import Name		( Name )		-- Instance 
import Outputable	( interpp'SP )
import PprType		( GenType, GenTyVar )	-- Instances
import Maybes		( maybeToBool )
import Pretty
import Util
\end{code}

\begin{code}
tcExpr :: RenamedHsExpr -> TcM s (TcExpr s, LIE s, TcType s)
\end{code}

%************************************************************************
%*									*
\subsection{The TAUT rules for variables}
%*									*
%************************************************************************

\begin{code}
tcExpr (HsVar name)
  = tcId name		`thenTc` \ (expr', lie, res_ty) ->

    -- Check that the result type doesn't have any nested for-alls.
    -- For example, a "build" on its own is no good; it must be
    -- applied to something.
    checkTc (isTauTy res_ty)
	    (lurkingRank2Err name res_ty) `thenTc_`

    returnTc (expr', lie, res_ty)
\end{code}

%************************************************************************
%*									*
\subsection{Literals}
%*									*
%************************************************************************

Overloaded literals.

\begin{code}
tcExpr (HsLit (HsInt i))
  = newTyVarTy mkBoxedTypeKind	`thenNF_Tc` \ ty ->

    newOverloadedLit (LiteralOrigin (HsInt i))
		     (OverloadedIntegral i)
		     ty					`thenNF_Tc` \ (lie, over_lit_id) ->

    returnTc (HsVar over_lit_id, lie, ty)

tcExpr (HsLit (HsFrac f))
  = newTyVarTy mkBoxedTypeKind	`thenNF_Tc` \ ty ->

    newOverloadedLit (LiteralOrigin (HsFrac f))
		     (OverloadedFractional f)
		     ty					`thenNF_Tc` \ (lie, over_lit_id) ->

    returnTc (HsVar over_lit_id, lie, ty)

tcExpr (HsLit lit@(HsLitLit s))
  = tcLookupClassByKey cCallableClassKey		`thenNF_Tc` \ cCallableClass ->
    newTyVarTy mkBoxedTypeKind				`thenNF_Tc` \ ty ->
    newDicts (LitLitOrigin (_UNPK_ s))
	     [(cCallableClass, ty)]			`thenNF_Tc` \ (dicts, _) ->
    returnTc (HsLitOut lit ty, dicts, ty)
\end{code}

Primitive literals:

\begin{code}
tcExpr (HsLit lit@(HsCharPrim c))
  = returnTc (HsLitOut lit charPrimTy, emptyLIE, charPrimTy)

tcExpr (HsLit lit@(HsStringPrim s))
  = returnTc (HsLitOut lit addrPrimTy, emptyLIE, addrPrimTy)

tcExpr (HsLit lit@(HsIntPrim i))
  = returnTc (HsLitOut lit intPrimTy, emptyLIE, intPrimTy)

tcExpr (HsLit lit@(HsFloatPrim f))
  = returnTc (HsLitOut lit floatPrimTy, emptyLIE, floatPrimTy)

tcExpr (HsLit lit@(HsDoublePrim d))
  = returnTc (HsLitOut lit doublePrimTy, emptyLIE, doublePrimTy)
\end{code}

Unoverloaded literals:

\begin{code}
tcExpr (HsLit lit@(HsChar c))
  = returnTc (HsLitOut lit charTy, emptyLIE, charTy)

tcExpr (HsLit lit@(HsString str))
  = returnTc (HsLitOut lit stringTy, emptyLIE, stringTy)
\end{code}

%************************************************************************
%*									*
\subsection{Other expression forms}
%*									*
%************************************************************************

\begin{code}
tcExpr (HsLam match)
  = tcMatch match	`thenTc` \ (match',lie,ty) ->
    returnTc (HsLam match', lie, ty)

tcExpr (HsApp e1 e2) = accum e1 [e2]
  where
    accum (HsApp e1 e2) args = accum e1 (e2:args)
    accum fun args
      = tcApp fun args 	`thenTc` \ (fun', args', lie, res_ty) ->
	returnTc (foldl HsApp fun' args', lie, res_ty)

-- equivalent to (op e1) e2:
tcExpr (OpApp arg1 op arg2)
  = tcApp op [arg1,arg2]	`thenTc` \ (op', [arg1', arg2'], lie, res_ty) ->
    returnTc (OpApp arg1' op' arg2', lie, res_ty)
\end{code}

Note that the operators in sections are expected to be binary, and
a type error will occur if they aren't.

\begin{code}
-- Left sections, equivalent to
--	\ x -> e op x,
-- or
--	\ x -> op e x,
-- or just
-- 	op e

tcExpr in_expr@(SectionL arg op)
  = tcApp op [arg] 		`thenTc` \ (op', [arg'], lie, res_ty) ->

	-- Check that res_ty is a function type
	-- Without this check we barf in the desugarer on
	-- 	f op = (3 `op`)
	-- because it tries to desugar to
	--	f op = \r -> 3 op r
	-- so (3 `op`) had better be a function!
    newTyVarTy mkTypeKind		`thenNF_Tc` \ ty1 ->
    newTyVarTy mkTypeKind		`thenNF_Tc` \ ty2 ->
    tcAddErrCtxt (sectionLAppCtxt in_expr) $
    unifyTauTy (mkFunTy ty1 ty2) res_ty	`thenTc_`

    returnTc (SectionL arg' op', lie, res_ty)

-- Right sections, equivalent to \ x -> x op expr, or
--	\ x -> op x expr

tcExpr in_expr@(SectionR op expr)
  = tcExpr op			`thenTc`    \ (op',  lie1, op_ty) ->
    tcExpr expr			`thenTc`    \ (expr',lie2, expr_ty) ->

    newTyVarTy mkTypeKind	`thenNF_Tc` \ ty1 ->
    newTyVarTy mkTypeKind	`thenNF_Tc` \ ty2 ->
    tcAddErrCtxt (sectionRAppCtxt in_expr) $
    unifyTauTy op_ty (mkFunTys [ty1, expr_ty] ty2)     `thenTc_`

    returnTc (SectionR op' expr', lie1 `plusLIE` lie2, mkFunTy ty1 ty2)
\end{code}

The interesting thing about @ccall@ is that it is just a template
which we instantiate by filling in details about the types of its
argument and result (ie minimal typechecking is performed).  So, the
basic story is that we allocate a load of type variables (to hold the
arg/result types); unify them with the args/result; and store them for
later use.

\begin{code}
tcExpr (CCall lbl args may_gc is_asm ignored_fake_result_ty)
  = 	-- Get the callable and returnable classes.
    tcLookupClassByKey cCallableClassKey	`thenNF_Tc` \ cCallableClass ->
    tcLookupClassByKey cReturnableClassKey	`thenNF_Tc` \ cReturnableClass ->

    let
	new_arg_dict (arg, arg_ty)
	  = newDicts (CCallOrigin (_UNPK_ lbl) (Just arg))
		     [(cCallableClass, arg_ty)]		`thenNF_Tc` \ (arg_dicts, _) ->
	    returnNF_Tc arg_dicts	-- Actually a singleton bag

	result_origin = CCallOrigin (_UNPK_ lbl) Nothing {- Not an arg -}
    in

	-- Arguments
    tcExprs args			`thenTc` \ (args', args_lie, arg_tys) ->

	-- The argument types can be unboxed or boxed; the result
	-- type must, however, be boxed since it's an argument to the PrimIO
	-- type constructor.
    newTyVarTy mkBoxedTypeKind  		`thenNF_Tc` \ result_ty ->

	-- Construct the extra insts, which encode the
	-- constraints on the argument and result types.
    mapNF_Tc new_arg_dict (args `zip` arg_tys)			`thenNF_Tc` \ ccarg_dicts_s ->
    newDicts result_origin [(cReturnableClass, result_ty)]	`thenNF_Tc` \ (ccres_dict, _) ->

    returnTc (CCall lbl args' may_gc is_asm result_ty,
	      foldr plusLIE ccres_dict ccarg_dicts_s `plusLIE` args_lie,
	      mkPrimIoTy result_ty)
\end{code}

\begin{code}
tcExpr (HsSCC label expr)
  = tcExpr expr		`thenTc` \ (expr', lie, expr_ty) ->
	 -- No unification. Give SCC the type of expr
    returnTc (HsSCC label expr', lie, expr_ty)

tcExpr (HsLet binds expr)
  = tcBindsAndThen
	HsLet 			-- The combiner
	binds 			-- Bindings to check
	(tcExpr expr)		-- Typechecker for the expression

tcExpr in_expr@(HsCase expr matches src_loc)
  = tcAddSrcLoc src_loc	$
    tcExpr expr			`thenTc`    \ (expr',lie1,expr_ty) ->
    newTyVarTy mkTypeKind	`thenNF_Tc` \ result_ty ->

    tcAddErrCtxt (caseCtxt in_expr) $
    tcMatchesCase (mkFunTy expr_ty result_ty) matches	
				`thenTc`    \ (matches',lie2) ->

    returnTc (HsCase expr' matches' src_loc, plusLIE lie1 lie2, result_ty)

tcExpr (HsIf pred b1 b2 src_loc)
  = tcAddSrcLoc src_loc	$
    tcExpr pred			`thenTc`    \ (pred',lie1,predTy) ->

    tcAddErrCtxt (predCtxt pred) (
      unifyTauTy predTy boolTy
    )				`thenTc_`

    tcExpr b1			`thenTc`    \ (b1',lie2,result_ty) ->
    tcExpr b2			`thenTc`    \ (b2',lie3,b2Ty) ->

    tcAddErrCtxt (branchCtxt b1 b2) $
    unifyTauTy result_ty b2Ty				`thenTc_`

    returnTc (HsIf pred' b1' b2' src_loc, plusLIE lie1 (plusLIE lie2 lie3), result_ty)

tcExpr (ListComp expr quals) 
  = tcListComp expr quals	`thenTc` \ ((expr',quals'), lie, ty) ->
    returnTc (ListComp expr' quals', lie, ty)
\end{code}

\begin{code}
tcExpr (HsDo stmts src_loc)
  = 	-- get the Monad and MonadZero classes
	-- create type consisting of a fresh monad tyvar
    tcAddSrcLoc src_loc	$
    newTyVarTy monadKind	`thenNF_Tc` \ m ->
    tcDoStmts False m stmts	`thenTc` \ ((stmts',monad,mzero), lie, do_ty) ->

	-- create dictionaries for monad and possibly monadzero
    (if monad then
	tcLookupClassByKey monadClassKey		`thenNF_Tc` \ monadClass ->
	newDicts DoOrigin [(monadClass, m)]	
    else
	returnNF_Tc (emptyLIE, [panic "TcExpr: MonadZero dictionary"])
    )						`thenNF_Tc` \ (m_lie,  [m_id])  ->
    (if mzero then
	tcLookupClassByKey monadZeroClassKey	`thenNF_Tc` \ monadZeroClass ->
	newDicts DoOrigin [(monadZeroClass, m)]
     else
        returnNF_Tc (emptyLIE, [panic "TcExpr: MonadZero dictionary"])
    )						`thenNF_Tc` \ (mz_lie, [mz_id]) ->

    returnTc (HsDoOut stmts' m_id mz_id src_loc,
	      lie `plusLIE` m_lie `plusLIE` mz_lie,
	      do_ty)
  where
    monadKind = mkArrowKind mkBoxedTypeKind mkBoxedTypeKind
\end{code}

\begin{code}
tcExpr (ExplicitList [])
  = newTyVarTy mkBoxedTypeKind		`thenNF_Tc` \ tyvar_ty ->
    returnTc (ExplicitListOut tyvar_ty [], emptyLIE, mkListTy tyvar_ty)


tcExpr in_expr@(ExplicitList exprs)	-- Non-empty list
  = tcExprs exprs			`thenTc` \ (exprs', lie, tys@(elt_ty:_)) ->
    tcAddErrCtxt (listCtxt in_expr) $
    unifyTauTyList tys 			`thenTc_`
    returnTc (ExplicitListOut elt_ty exprs', lie, mkListTy elt_ty)

tcExpr (ExplicitTuple exprs)
  = tcExprs exprs			`thenTc` \ (exprs', lie, tys) ->
    returnTc (ExplicitTuple exprs', lie, mkTupleTy (length tys) tys)

tcExpr (RecordCon (HsVar con) rbinds)
  = tcGlobalOcc con		`thenNF_Tc` \ (con_id, arg_tys, con_rho) ->
    let
	(con_theta, con_tau) = splitRhoTy con_rho
	(_, record_ty)       = splitFunTy con_tau
	con_expr	     = mkHsTyApp (HsVar (RealId con_id)) arg_tys
    in
	-- TEMPORARY ASSERT
    ASSERT( null con_theta )

	-- Con is syntactically constrained to be a data constructor
    ASSERT( maybeToBool (maybeAppDataTyCon record_ty ) )

    tcRecordBinds record_ty rbinds		`thenTc` \ (rbinds', rbinds_lie) ->

    checkTc (checkRecordFields rbinds con_id)
	    (badFieldsCon con rbinds)		`thenTc_`

    returnTc (RecordCon con_expr rbinds', panic "tcExpr:RecordCon:con_lie???" {-con_lie???-} `plusLIE` rbinds_lie, record_ty)

tcExpr (RecordUpd record_expr rbinds)
  = tcExpr record_expr			`thenTc` \ (record_expr', record_lie, record_ty) ->
    tcRecordBinds record_ty rbinds	`thenTc` \ (rbinds', rbinds_lie) ->

	-- Check that the field names are plausible
    zonkTcType record_ty		`thenNF_Tc` \ record_ty' ->
    let
	maybe_tycon_stuff = maybeAppDataTyCon record_ty'
	Just (tycon, args_tys, data_cons) = maybe_tycon_stuff
    in
    checkTc (maybeToBool maybe_tycon_stuff)
	    (panic "TcExpr:Records:mystery error message") `thenTc_`
    checkTc (any (checkRecordFields rbinds) data_cons)
	    (badFieldsUpd rbinds)		`thenTc_`
    returnTc (RecordUpd record_expr' rbinds', record_lie `plusLIE` rbinds_lie, record_ty)

tcExpr (ArithSeqIn seq@(From expr))
  = tcExpr expr					`thenTc`    \ (expr', lie1, ty) ->

    tcLookupGlobalValueByKey enumFromClassOpKey	`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq)
	      (RealId sel_id) [ty]		`thenNF_Tc` \ (lie2, enum_from_id) ->

    returnTc (ArithSeqOut (HsVar enum_from_id) (From expr'),
	      lie1 `plusLIE` lie2,
	      mkListTy ty)

tcExpr in_expr@(ArithSeqIn seq@(FromThen expr1 expr2))
  = tcExpr expr1		`thenTc`    \ (expr1',lie1,ty1) ->
    tcExpr expr2		`thenTc`    \ (expr2',lie2,ty2) ->

    tcAddErrCtxt (arithSeqCtxt in_expr) $
    unifyTauTyList [ty1, ty2] 				`thenTc_`

    tcLookupGlobalValueByKey enumFromThenClassOpKey	`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq)
	      (RealId sel_id) [ty1]			`thenNF_Tc` \ (lie3, enum_from_then_id) ->

    returnTc (ArithSeqOut (HsVar enum_from_then_id)
			   (FromThen expr1' expr2'),
	      lie1 `plusLIE` lie2 `plusLIE` lie3,
	      mkListTy ty1)

tcExpr in_expr@(ArithSeqIn seq@(FromTo expr1 expr2))
  = tcExpr expr1		`thenTc`    \ (expr1',lie1,ty1) ->
    tcExpr expr2		`thenTc`    \ (expr2',lie2,ty2) ->

    tcAddErrCtxt (arithSeqCtxt in_expr) $
    unifyTauTyList [ty1,ty2]	`thenTc_`

    tcLookupGlobalValueByKey enumFromToClassOpKey	`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq)
	      (RealId sel_id) [ty1] 		`thenNF_Tc` \ (lie3, enum_from_to_id) ->

    returnTc (ArithSeqOut (HsVar enum_from_to_id)
			  (FromTo expr1' expr2'),
	      lie1 `plusLIE` lie2 `plusLIE` lie3,
	       mkListTy ty1)

tcExpr in_expr@(ArithSeqIn seq@(FromThenTo expr1 expr2 expr3))
  = tcExpr expr1		`thenTc`    \ (expr1',lie1,ty1) ->
    tcExpr expr2		`thenTc`    \ (expr2',lie2,ty2) ->
    tcExpr expr3		`thenTc`    \ (expr3',lie3,ty3) ->

    tcAddErrCtxt  (arithSeqCtxt in_expr) $
    unifyTauTyList [ty1,ty2,ty3]			`thenTc_`

    tcLookupGlobalValueByKey enumFromThenToClassOpKey	`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq)
	      (RealId sel_id) [ty1]			`thenNF_Tc` \ (lie4, eft_id) ->

    returnTc (ArithSeqOut (HsVar eft_id)
			   (FromThenTo expr1' expr2' expr3'),
	      lie1 `plusLIE` lie2 `plusLIE` lie3 `plusLIE` lie4,
	      mkListTy ty1)
\end{code}

%************************************************************************
%*									*
\subsection{Expressions type signatures}
%*									*
%************************************************************************

\begin{code}
tcExpr in_expr@(ExprWithTySig expr poly_ty)
 = tcExpr expr			`thenTc` \ (texpr, lie, tau_ty) ->
   tcPolyType  poly_ty		`thenTc` \ sigma_sig ->

	-- Check the tau-type part
   tcSetErrCtxt (exprSigCtxt in_expr)	$
   tcInstType [] sigma_sig		`thenNF_Tc` \ sigma_sig' ->
   let
	(sig_tyvars', sig_theta', sig_tau') = splitSigmaTy sigma_sig'
   in
   unifyTauTy tau_ty sig_tau'		`thenTc_`

	-- Check the type variables of the signature
   checkSigTyVars sig_tyvars' sig_tau'	`thenTc_`

	-- Check overloading constraints
   newDicts SignatureOrigin sig_theta'		`thenNF_Tc` \ (sig_dicts, _) ->
   tcSimplifyAndCheck
	(mkTyVarSet sig_tyvars')
	sig_dicts lie				`thenTc_`

	-- If everything is ok, return the stuff unchanged, except for
	-- the effect of any substutions etc.  We simply discard the
	-- result of the tcSimplifyAndCheck, except for any default
	-- resolution it may have done, which is recorded in the
	-- substitution.
   returnTc (texpr, lie, tau_ty)
\end{code}

%************************************************************************
%*									*
\subsection{@tcApp@ typchecks an application}
%*									*
%************************************************************************

\begin{code}
tcApp :: RenamedHsExpr -> [RenamedHsExpr]   -- Function and args
      -> TcM s (TcExpr s, [TcExpr s],	    -- Translated fun and args
		LIE s,
		TcType s)		    -- Type of the application

tcApp fun args
  = 	-- First type-check the function
	-- In the HsVar case we go straight to tcId to avoid hitting the
	-- rank-2 check, which we check later here anyway
    (case fun of
	HsVar name -> tcId name
	other	   -> tcExpr fun
    )					`thenTc` \ (fun', lie_fun, fun_ty) ->

    tcApp_help fun 1 fun_ty args	`thenTc` \ (args', lie_args, res_ty) ->

    -- Check that the result type doesn't have any nested for-alls.
    -- For example, a "build" on its own is no good; it must be applied to something.
    checkTc (isTauTy res_ty)
	    (lurkingRank2Err fun fun_ty) `thenTc_`

    returnTc (fun', args', lie_fun `plusLIE` lie_args, res_ty)


tcApp_help :: RenamedHsExpr -> Int	-- Function and arg position, used in error message(s)
	   -> TcType s			-- The type of the function
	   -> [RenamedHsExpr]		-- Arguments
	   -> TcM s ([TcExpr s],		-- Typechecked args
		     LIE s,
		     TcType s)		-- Result type of the application

tcApp_help orig_fun arg_no fun_ty []
  = returnTc ([], emptyLIE, fun_ty)

tcApp_help orig_fun arg_no fun_ty all_args@(arg:args)
  = 	-- Expect the function to have type A->B
    tcAddErrCtxt (tooManyArgsCtxt orig_fun) (
	    unifyFunTy fun_ty
    )							`thenTc` \ (expected_arg_ty, result_ty) ->

	-- Type check the argument
    tcAddErrCtxt (funAppCtxt orig_fun arg_no arg) (
		tcArg expected_arg_ty arg
    )					 		`thenTc` \ (arg', lie_arg) ->

	-- Do the other args
    tcApp_help orig_fun (arg_no+1) result_ty args	`thenTc` \ (args', lie_args, res_ty) ->

	-- Done
    returnTc (arg':args', lie_arg `plusLIE` lie_args, res_ty)

\end{code}

\begin{code}
tcArg :: TcType s			-- Expected arg type
      -> RenamedHsExpr			-- Actual argument
      -> TcM s (TcExpr s, LIE s)	-- Resulting argument and LIE

tcArg expected_arg_ty arg
  | not (maybeToBool (getForAllTy_maybe expected_arg_ty))
  = 	-- The ordinary, non-rank-2 polymorphic case
    tcExpr arg					`thenTc` \ (arg', lie_arg, actual_arg_ty) ->
    unifyTauTy expected_arg_ty actual_arg_ty	`thenTc_`
    returnTc (arg', lie_arg)

  | otherwise
  = 	-- Ha!  The argument type of the function is a for-all type,
	-- An example of rank-2 polymorphism.

	-- No need to instantiate the argument type... it's must be the result
	-- of instantiating a function involving rank-2 polymorphism, so there
	-- isn't any danger of using the same tyvars twice
	-- The argument type shouldn't be overloaded type (hence ASSERT)
    let
	(expected_tyvars, expected_theta, expected_tau) = splitSigmaTy expected_arg_ty
    in
    ASSERT( null expected_theta )	-- And expected_tyvars are all DontBind things

	-- Type-check the arg and unify with expected type
    tcExpr arg					`thenTc` \ (arg', lie_arg, actual_arg_ty) ->
    unifyTauTy expected_tau actual_arg_ty	`thenTc_`  (

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
    tcAddErrCtxt (rank2ArgCtxt arg expected_arg_ty) $
    tcGetGlobalTyVars						`thenNF_Tc` \ env_tyvars ->
    zonkTcTyVars (tyVarsOfType expected_arg_ty)			`thenNF_Tc` \ free_tyvars ->
    checkSigTyVarsGivenGlobals
	(env_tyvars `unionTyVarSets` free_tyvars)
	expected_tyvars expected_tau				`thenTc_`

	-- Check that there's no overloading involved
	-- Even if there isn't, there may be some Insts which mention the expected_tyvars,
	-- but which, on simplification, don't actually need a dictionary involving
	-- the tyvar.  So we have to do a proper simplification right here.
    tcSimplifyRank2 (mkTyVarSet expected_tyvars) 
		    lie_arg				`thenTc` \ (free_insts, inst_binds) ->

	-- This HsLet binds any Insts which came out of the simplification.
	-- It's a bit out of place here, but using AbsBind involves inventing
	-- a couple of new names which seems worse.
    returnTc (TyLam expected_tyvars (HsLet (mk_binds inst_binds) arg'), free_insts)
    )
  where

    mk_binds []
	= EmptyBinds
    mk_binds ((inst,rhs):inst_binds)
	= (SingleBind (NonRecBind (VarMonoBind inst rhs)))
		`ThenBinds`
	  mk_binds inst_binds
\end{code}

%************************************************************************
%*									*
\subsection{@tcId@ typchecks an identifier occurrence}
%*									*
%************************************************************************

\begin{code}
tcId :: RnName -> TcM s (TcExpr s, LIE s, TcType s)

tcId name
  = 	-- Look up the Id and instantiate its type
    tcLookupLocalValue name	`thenNF_Tc` \ maybe_local ->

    (case maybe_local of
	Just tc_id -> let
		        (tyvars, rho) = splitForAllTy (idType tc_id)
		      in
		      tcInstTyVars tyvars		`thenNF_Tc` \ (tyvars', arg_tys', tenv)  ->
		      tcInstTcType tenv rho		`thenNF_Tc` \ rho' ->
		      returnNF_Tc (TcId tc_id, arg_tys', rho')

	Nothing ->    tcGlobalOcc name			`thenNF_Tc` \ (id, arg_tys, rho) ->
		      returnNF_Tc (RealId id, arg_tys, rho)

    )					`thenNF_Tc` \ (tc_id_occ, arg_tys, rho) ->

	-- Is it overloaded?
    case splitRhoTy rho of
      ([], tau)    -> 	-- Not overloaded, so just make a type application
		    	returnTc (mkHsTyApp (HsVar tc_id_occ) arg_tys, emptyLIE, tau)

      (theta, tau) ->	-- Overloaded, so make a Method inst
			newMethodWithGivenTy (OccurrenceOf tc_id_occ)
				tc_id_occ arg_tys rho		`thenNF_Tc` \ (lie, meth_id) ->
			returnTc (HsVar meth_id, lie, tau)
\end{code}



%************************************************************************
%*									*
\subsection{@tcQuals@ typchecks list comprehension qualifiers}
%*									*
%************************************************************************

\begin{code}
tcListComp expr []
  = tcExpr expr		`thenTc` \ (expr', lie, ty) ->
    returnTc ((expr',[]), lie, mkListTy ty)

tcListComp expr (qual@(FilterQual filter) : quals)
  = tcAddErrCtxt (qualCtxt qual) (
	tcExpr filter			`thenTc` \ (filter', filter_lie, filter_ty) ->
	unifyTauTy boolTy filter_ty	`thenTc_`
	returnTc (FilterQual filter', filter_lie)
    )					`thenTc` \ (qual', qual_lie) ->

    tcListComp expr quals	`thenTc` \ ((expr',quals'), rest_lie, res_ty) ->

    returnTc ((expr', qual' : quals'), 
	      qual_lie `plusLIE` rest_lie,
	      res_ty)

tcListComp expr (qual@(GeneratorQual pat rhs) : quals)
  = newMonoIds binder_names mkBoxedTypeKind (\ ids ->

      tcAddErrCtxt (qualCtxt qual) (
        tcPat pat				`thenTc` \ (pat',  lie_pat,  pat_ty)  ->
        tcExpr rhs				`thenTc` \ (rhs', lie_rhs, rhs_ty) ->
        unifyTauTy (mkListTy pat_ty) rhs_ty	`thenTc_`
	returnTc (GeneratorQual pat' rhs', 
		  lie_pat `plusLIE` lie_rhs) 
      )						`thenTc` \ (qual', lie_qual) ->

      tcListComp expr quals 			`thenTc` \ ((expr',quals'), lie_rest, res_ty) ->

      returnTc ((expr', qual' : quals'), 
		lie_qual `plusLIE` lie_rest,
	        res_ty)
    )
  where
    binder_names = collectPatBinders pat

tcListComp expr (LetQual binds : quals)
  = tcBindsAndThen		-- No error context, but a binding group is
	combine			-- rather a large thing for an error context anyway
	binds
	(tcListComp expr quals)
  where
    combine binds' (expr',quals') = (expr', LetQual binds' : quals')
\end{code}


%************************************************************************
%*									*
\subsection{@tcDoStmts@ typechecks a {\em list} of do statements}
%*									*
%************************************************************************

\begin{code}
tcDoStmts :: Bool			-- True => require a monad
	  -> TcType s			-- m
	  -> [RenamedStmt]	
	  -> TcM s (([TcStmt s],
		     Bool,		-- True => Monad
		     Bool), 		-- True => MonadZero
		    LIE s,
		    TcType s)
					
tcDoStmts monad m [stmt@(ExprStmt exp src_loc)]
  = tcAddSrcLoc src_loc $
    tcSetErrCtxt (stmtCtxt stmt) $
    tcExpr exp			 	`thenTc`    \ (exp', exp_lie, exp_ty) ->
    (if monad then
	newTyVarTy mkTypeKind		`thenNF_Tc` \ a ->
	unifyTauTy (mkAppTy m a) exp_ty
     else
	returnTc ()
    )					`thenTc_`
    returnTc (([ExprStmt exp' src_loc], monad, False), exp_lie, exp_ty)

tcDoStmts _ m (stmt@(ExprStmt exp src_loc) : stmts)
  = tcAddSrcLoc src_loc 		(
    tcSetErrCtxt (stmtCtxt stmt)	(
    	tcExpr exp			`thenTc`    \ (exp', exp_lie, exp_ty) ->
	newTyVarTy mkTypeKind		`thenNF_Tc` \ a ->
    	unifyTauTy (mkAppTy m a) exp_ty	`thenTc_`
	returnTc (ExprStmt exp' src_loc, exp_lie)
    ))					`thenTc` \ (stmt',  stmt_lie) -> 
    tcDoStmts True m stmts		`thenTc` \ ((stmts', _, mzero), stmts_lie, stmts_ty) ->
    returnTc ((stmt':stmts', True, mzero),
	      stmt_lie `plusLIE` stmts_lie,
	      stmts_ty)

tcDoStmts _ m (stmt@(BindStmt pat exp src_loc) : stmts)
  = tcAddSrcLoc src_loc			(
    tcSetErrCtxt (stmtCtxt stmt)	(
	tcPat pat			`thenTc`    \ (pat', pat_lie, pat_ty) ->  
    	tcExpr exp			`thenTc`    \ (exp', exp_lie, exp_ty) ->
	newTyVarTy mkTypeKind		`thenNF_Tc` \ a ->
	unifyTauTy a pat_ty		`thenTc_`
	unifyTauTy (mkAppTy m a) exp_ty	`thenTc_`
	returnTc (BindStmt pat' exp' src_loc, pat_lie `plusLIE` exp_lie, irrefutablePat pat')
    ))					`thenTc` \ (stmt', stmt_lie, failure_free) -> 
    tcDoStmts True m stmts		`thenTc` \ ((stmts', _, mzero), stmts_lie, stmts_ty) ->
    returnTc ((stmt':stmts', True, mzero || not failure_free),
	      stmt_lie `plusLIE` stmts_lie,
	      stmts_ty)

tcDoStmts monad m (LetStmt binds : stmts)
   = tcBindsAndThen		-- No error context, but a binding group is
	combine			-- rather a large thing for an error context anyway
	binds
	(tcDoStmts monad m stmts)
   where
     combine binds' (stmts', monad, mzero) = ((LetStmt binds' : stmts'), monad, mzero)

\end{code}

Game plan for record bindings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For each binding 
	field = value
1. look up "field", to find its selector Id, which must have type
	forall a1..an. T a1 .. an -> tau
   where tau is the type of the field.  

2. Instantiate this type

3. Unify the (T a1 .. an) part with the "expected result type", which
   is passed in.  This checks that all the field labels come from the
   same type.

4. Type check the value using tcArg, passing tau as the expected
   argument type.

This extends OK when the field types are universally quantified.

Actually, to save excessive creation of fresh type variables,
we 
	
\begin{code}
tcRecordBinds
	:: TcType s		-- Expected type of whole record
	-> RenamedRecordBinds
	-> TcM s (TcRecordBinds s, LIE s)

tcRecordBinds expected_record_ty rbinds
  = mapAndUnzipTc do_bind rbinds	`thenTc` \ (rbinds', lies) ->
    returnTc (rbinds', plusLIEs lies)
  where
    do_bind (field_label, rhs, pun_flag)
      = tcGlobalOcc field_label		`thenNF_Tc` \ (sel_id, _, tau) ->

		-- Record selectors all have type
		-- 	forall a1..an.  T a1 .. an -> tau
	ASSERT( maybeToBool (getFunTy_maybe tau) )
	let
		-- Selector must have type RecordType -> FieldType
	  Just (record_ty, field_ty) = getFunTy_maybe tau
	in
	unifyTauTy expected_record_ty record_ty		`thenTc_`
	tcArg field_ty rhs				`thenTc` \ (rhs', lie) ->
	returnTc ((RealId sel_id, rhs', pun_flag), lie)

checkRecordFields :: RenamedRecordBinds -> Id -> Bool	-- True iff all the fields in
							-- RecordBinds are field of the
							-- specified constructor
checkRecordFields rbinds data_con
  = all ok rbinds
  where 
    data_con_fields = dataConFieldLabels data_con

    ok (field_name, _, _) = any (match (getName field_name)) data_con_fields

    match field_name field_label = field_name == fieldLabelName field_label
\end{code}

%************************************************************************
%*									*
\subsection{@tcExprs@ typechecks a {\em list} of expressions}
%*									*
%************************************************************************

\begin{code}
tcExprs :: [RenamedHsExpr] -> TcM s ([TcExpr s], LIE s, [TcType s])

tcExprs [] = returnTc ([], emptyLIE, [])
tcExprs (expr:exprs)
 = tcExpr  expr			`thenTc` \ (expr',  lie1, ty) ->
   tcExprs exprs		`thenTc` \ (exprs', lie2, tys) ->
   returnTc (expr':exprs', lie1 `plusLIE` lie2, ty:tys)
\end{code}


% =================================================

Errors and contexts
~~~~~~~~~~~~~~~~~~~

Mini-utils:
\begin{code}
pp_nest_hang :: String -> Pretty -> Pretty
pp_nest_hang label stuff = ppNest 2 (ppHang (ppStr label) 4 stuff)
\end{code}

Boring and alphabetical:
\begin{code}
arithSeqCtxt expr sty
  = ppHang (ppStr "In an arithmetic sequence:") 4 (ppr sty expr)

branchCtxt b1 b2 sty
  = ppSep [ppStr "In the branches of a conditional:",
	   pp_nest_hang "`then' branch:" (ppr sty b1),
	   pp_nest_hang "`else' branch:" (ppr sty b2)]

caseCtxt expr sty
  = ppHang (ppStr "In a case expression:") 4 (ppr sty expr)

exprSigCtxt expr sty
  = ppHang (ppStr "In an expression with a type signature:")
	 4 (ppr sty expr)

listCtxt expr sty
  = ppHang (ppStr "In a list expression:") 4 (ppr sty expr)

predCtxt expr sty
  = ppHang (ppStr "In a predicate expression:") 4 (ppr sty expr)

sectionRAppCtxt expr sty
  = ppHang (ppStr "In a right section:") 4 (ppr sty expr)

sectionLAppCtxt expr sty
  = ppHang (ppStr "In a left section:") 4 (ppr sty expr)

funAppCtxt fun arg_no arg sty
  = ppHang (ppCat [ ppStr "In the", speakNth arg_no, ppStr "argument of", ppr sty fun])
	 4 (ppCat [ppStr "namely", ppr sty arg])

qualCtxt qual sty
  = ppHang (ppStr "In a list-comprehension qualifer:") 
         4 (ppr sty qual)

stmtCtxt stmt sty
  = ppHang (ppStr "In a do statement:") 
         4 (ppr sty stmt)

tooManyArgsCtxt f sty
  = ppHang (ppStr "Too many arguments in an application of the function")
	 4 (ppr sty f)

lurkingRank2Err fun fun_ty sty
  = ppHang (ppCat [ppStr "Illegal use of", ppr sty fun])
	 4 (ppAboves [ppStr "It is applied to too few arguments,", 
		      ppStr "so that the result type has for-alls in it"])

rank2ArgCtxt arg expected_arg_ty sty
  = ppHang (ppStr "In a polymorphic function argument:")
	 4 (ppSep [ppBeside (ppr sty arg) (ppStr " ::"),
		   ppr sty expected_arg_ty])

badFieldsUpd rbinds sty
  = ppHang (ppStr "In a record update construct, no constructor has all these fields:")
	 4 (interpp'SP sty fields)
  where
    fields = [field | (field, _, _) <- rbinds]

badFieldsCon con rbinds sty
  = ppHang (ppBesides [ppStr "Inconsistent constructor:", ppr sty con])
	 4 (ppBesides [ppStr "and fields:", interpp'SP sty fields])
  where
    fields = [field | (field, _, _) <- rbinds]
\end{code}
