%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcExpr]{Typecheck an expression}

\begin{code}
#include "HsVersions.h"

module TcExpr ( tcExpr ) where

IMP_Ubiq()

import HsSyn		( HsExpr(..), Qualifier(..), Stmt(..),
			  HsBinds(..), Bind(..), MonoBinds(..), 
			  ArithSeqInfo(..), HsLit(..), Sig, GRHSsAndBinds,
			  Match, Fake, InPat, OutPat, PolyType,
			  failureFreePat, collectPatBinders )
import RnHsSyn		( SYN_IE(RenamedHsExpr), SYN_IE(RenamedQual),
			  SYN_IE(RenamedStmt), SYN_IE(RenamedRecordBinds),
			  RnName{-instance Outputable-}
			)
import TcHsSyn		( SYN_IE(TcExpr), SYN_IE(TcQual), SYN_IE(TcStmt),
			  TcIdOcc(..), SYN_IE(TcRecordBinds),
			  mkHsTyApp
			)

import TcMonad		hiding ( rnMtoTcM )
import Inst		( Inst, InstOrigin(..), OverloadedLit(..),
			  SYN_IE(LIE), emptyLIE, plusLIE, plusLIEs, newOverloadedLit,
			  newMethod, newMethodWithGivenTy, newDicts )
import TcBinds		( tcBindsAndThen )
import TcEnv		( tcLookupLocalValue, tcLookupGlobalValue, tcLookupClassByKey,
			  tcLookupGlobalValueByKey, newMonoIds, tcGetGlobalTyVars,
			  tcExtendGlobalTyVars
			)
import SpecEnv		( SpecEnv )
import TcMatches	( tcMatchesCase, tcMatch )
import TcMonoType	( tcPolyType )
import TcPat		( tcPat )
import TcSimplify	( tcSimplifyAndCheck, tcSimplifyRank2 )
import TcType		( SYN_IE(TcType), TcMaybe(..),
			  tcInstId, tcInstType, tcInstSigTcType,
			  tcInstSigType, tcInstTcType, tcInstTheta,
			  newTyVarTy, zonkTcTyVars, zonkTcType )
import TcKind		( TcKind )

import Class		( SYN_IE(Class), classSig )
import FieldLabel	( fieldLabelName )
import Id		( idType, dataConFieldLabels, dataConSig, SYN_IE(Id), GenId )
import Kind		( Kind, mkBoxedTypeKind, mkTypeKind, mkArrowKind )
import GenSpecEtc	( checkSigTyVars )
import Name		( Name{-instance Eq-} )
import Type		( mkFunTy, mkAppTy, mkTyVarTy, mkTyVarTys, mkRhoTy,
			  getTyVar_maybe, getFunTy_maybe, instantiateTy,
			  splitForAllTy, splitRhoTy, splitSigmaTy, splitFunTy,
			  isTauTy, mkFunTys, tyVarsOfType, getForAllTy_maybe,
			  getAppDataTyCon, maybeAppDataTyCon
			)
import TyVar		( GenTyVar, SYN_IE(TyVarSet), unionTyVarSets, mkTyVarSet )
import TysPrim		( intPrimTy, charPrimTy, doublePrimTy,
			  floatPrimTy, addrPrimTy, realWorldTy
			)
import TysWiredIn	( addrTy,
			  boolTy, charTy, stringTy, mkListTy,
			  mkTupleTy, mkPrimIoTy, stDataCon
			)
import Unify		( unifyTauTy, unifyTauTyList, unifyTauTyLists, unifyFunTy )
import Unique		( Unique, cCallableClassKey, cReturnableClassKey, 
			  enumFromClassOpKey, enumFromThenClassOpKey,
			  enumFromToClassOpKey, enumFromThenToClassOpKey,
			  thenMClassOpKey, zeroClassOpKey
			)
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
  = tcId name		`thenNF_Tc` \ (expr', lie, res_ty) ->

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
tcExpr (HsPar expr) -- preserve parens so printing needn't guess where they go
  = tcExpr expr

tcExpr (NegApp expr neg) = tcExpr (HsApp neg expr)

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
    unifyTauTy (mkFunTys [ty1, expr_ty] ty2) op_ty      `thenTc_`

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
    mapNF_Tc new_arg_dict (zipEqual "tcExpr:CCall" args arg_tys)    `thenNF_Tc` \ ccarg_dicts_s ->
    newDicts result_origin [(cReturnableClass, result_ty)]	    `thenNF_Tc` \ (ccres_dict, _) ->

    returnTc (HsCon stDataCon [realWorldTy, result_ty] [CCall lbl args' may_gc is_asm result_ty],
	      -- do the wrapping in the newtype constructor here
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
      unifyTauTy boolTy predTy
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
tcExpr expr@(HsDo stmts src_loc)
  = tcDoStmts stmts src_loc
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
  = tcId con				`thenNF_Tc` \ (con_expr, con_lie, con_tau) ->
    let
	(_, record_ty) = splitFunTy con_tau
    in
	-- Con is syntactically constrained to be a data constructor
    ASSERT( maybeToBool (maybeAppDataTyCon record_ty ) )

    tcRecordBinds record_ty rbinds		`thenTc` \ (rbinds', rbinds_lie) ->

	-- Check that the record bindings match the constructor
    tcLookupGlobalValue con			`thenNF_Tc` \ con_id ->
    checkTc (checkRecordFields rbinds con_id)
	    (badFieldsCon con rbinds)		`thenTc_`

    returnTc (RecordCon con_expr rbinds', con_lie `plusLIE` rbinds_lie, record_ty)

-- One small complication in RecordUpd is that we have to generate some 
-- dictionaries for the data type context, since we are going to
-- do some construction.
--
-- What dictionaries do we need?  For the moment we assume that all
-- data constructors have the same context, and grab it from the first
-- constructor.  If they have varying contexts then we'd have to 
-- union the ones that could participate in the update.

tcExpr (RecordUpd record_expr rbinds)
  = ASSERT( not (null rbinds) )
    tcAddErrCtxt recordUpdCtxt			$

    tcExpr record_expr			`thenTc` \ (record_expr', record_lie, record_ty) ->
    tcRecordBinds record_ty rbinds	`thenTc` \ (rbinds', rbinds_lie) ->

	-- Check that the field names are plausible
    zonkTcType record_ty		`thenNF_Tc` \ record_ty' ->
    let
	(tycon, inst_tys, data_cons) = --trace "TcExpr.getAppDataTyCon" $
				       getAppDataTyCon record_ty'
	-- The record binds are non-empty (syntax); so at least one field
	-- label will have been unified with record_ty by tcRecordBinds;
	-- field labels must be of data type; hencd the getAppDataTyCon must succeed.
	(tyvars, theta, _, _) = dataConSig (head data_cons)
    in
    tcInstTheta (zipEqual "tcExpr:RecordUpd" tyvars inst_tys) theta `thenNF_Tc` \ theta' ->
    newDicts RecordUpdOrigin theta'				    `thenNF_Tc` \ (con_lie, dicts) ->
    checkTc (any (checkRecordFields rbinds) data_cons)
	    (badFieldsUpd rbinds)		`thenTc_`

    returnTc (RecordUpdOut record_expr' dicts rbinds', 
	      con_lie `plusLIE` record_lie `plusLIE` rbinds_lie, 
	      record_ty)

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
   tcInstSigType sigma_sig		`thenNF_Tc` \ sigma_sig' ->
   let
	(sig_tyvars', sig_theta', sig_tau') = splitSigmaTy sigma_sig'
   in
   unifyTauTy sig_tau' tau_ty		`thenTc_`

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
	HsVar name -> tcId name	`thenNF_Tc` \ stuff -> returnTc stuff
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

	-- To ensure that the forall'd type variables don't get unified with each
	-- other or any other types, we make fresh *signature* type variables
	-- and unify them with the tyvars.
    tcInstSigTcType expected_arg_ty 	`thenNF_Tc` \ (sig_tyvars, sig_rho) ->
    let
	(sig_theta, sig_tau) = splitRhoTy sig_rho
    in
    ASSERT( null sig_theta )	-- And expected_tyvars are all DontBind things
	
	-- Type-check the arg and unify with expected type
    tcExpr arg					`thenTc` \ (arg', lie_arg, actual_arg_ty) ->
    unifyTauTy sig_tau actual_arg_ty		`thenTc_`

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

    tcAddErrCtxt (rank2ArgCtxt arg expected_arg_ty) (
	tcExtendGlobalTyVars (tyVarsOfType expected_arg_ty) (
		checkSigTyVars sig_tyvars sig_tau
	)						`thenTc_`

	    -- Check that there's no overloading involved
	    -- Even if there isn't, there may be some Insts which mention the expected_tyvars,
	    -- but which, on simplification, don't actually need a dictionary involving
	    -- the tyvar.  So we have to do a proper simplification right here.
	tcSimplifyRank2 (mkTyVarSet sig_tyvars) 
			lie_arg				`thenTc` \ (free_insts, inst_binds) ->

	    -- This HsLet binds any Insts which came out of the simplification.
	    -- It's a bit out of place here, but using AbsBind involves inventing
	    -- a couple of new names which seems worse.
	returnTc (TyLam sig_tyvars (HsLet (mk_binds inst_binds) arg'), free_insts)
    )
  where

    mk_binds [] = EmptyBinds
    mk_binds ((inst,rhs):inst_binds)
	= (SingleBind (NonRecBind (VarMonoBind inst rhs))) `ThenBinds`
	  mk_binds inst_binds
\end{code}

%************************************************************************
%*									*
\subsection{@tcId@ typchecks an identifier occurrence}
%*									*
%************************************************************************

\begin{code}
tcId :: RnName -> NF_TcM s (TcExpr s, LIE s, TcType s)

tcId name
  = 	-- Look up the Id and instantiate its type
    tcLookupLocalValue name	`thenNF_Tc` \ maybe_local ->

    case maybe_local of
      Just tc_id -> instantiate_it (TcId tc_id) (idType tc_id)

      Nothing ->    tcLookupGlobalValue name	`thenNF_Tc` \ id ->
		    tcInstType [] (idType id)	`thenNF_Tc` \ inst_ty ->
		    let
			(tyvars, rho) = splitForAllTy inst_ty 
		    in
		    instantiate_it2 (RealId id) tyvars rho

  where
	-- The instantiate_it loop runs round instantiating the Id.
	-- It has to be a loop because we are now prepared to entertain
	-- types like
	--		f:: forall a. Eq a => forall b. Baz b => tau
	-- We want to instantiate this to
	--		f2::tau		{f2 = f1 b (Baz b), f1 = f a (Eq a)}
    instantiate_it tc_id_occ ty
      = tcInstTcType ty		`thenNF_Tc` \ (tyvars, rho) ->
	instantiate_it2 tc_id_occ tyvars rho

    instantiate_it2 tc_id_occ tyvars rho
      | null theta	-- Is it overloaded?
      = returnNF_Tc (mkHsTyApp (HsVar tc_id_occ) arg_tys, emptyLIE, tau)

      | otherwise	-- Yes, it's overloaded
      = newMethodWithGivenTy (OccurrenceOf tc_id_occ)
			     tc_id_occ arg_tys rho	`thenNF_Tc` \ (lie1, meth_id) ->
	instantiate_it meth_id tau			`thenNF_Tc` \ (expr, lie2, final_tau) ->
	returnNF_Tc (expr, lie1 `plusLIE` lie2, final_tau)

      where
        (theta,  tau) = splitRhoTy   rho
	arg_tys	      = mkTyVarTys tyvars
\end{code}

%************************************************************************
%*									*
\subsection{@tcQuals@ typechecks list-comprehension qualifiers}
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
		-- NB: the environment has been extended with the new binders
		-- which the rhs can't "see", but the renamer should have made
		-- sure that everything is distinct by now, so there's no problem.
		-- Putting the tcExpr before the newMonoIds messes up the nesting
		-- of error contexts, so I didn't  bother

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
tcDoStmts stmts src_loc
  =	-- get the Monad and MonadZero classes
	-- create type consisting of a fresh monad tyvar
    tcAddSrcLoc src_loc	$
    newTyVarTy (mkArrowKind mkBoxedTypeKind mkBoxedTypeKind)	`thenNF_Tc` \ m ->


	-- Build the then and zero methods in case we need them
    tcLookupGlobalValueByKey thenMClassOpKey	`thenNF_Tc` \ then_sel_id ->
    tcLookupGlobalValueByKey zeroClassOpKey	`thenNF_Tc` \ zero_sel_id ->
    newMethod DoOrigin
	      (RealId then_sel_id) [m]		`thenNF_Tc` \ (m_lie, then_id) ->
    newMethod DoOrigin
	      (RealId zero_sel_id) [m]		`thenNF_Tc` \ (mz_lie, zero_id) ->

    let
      get_m_arg ty 
	= newTyVarTy mkTypeKind			`thenNF_Tc` \ arg_ty ->
	  unifyTauTy (mkAppTy m arg_ty) ty	`thenTc_`
	  returnTc arg_ty

      go [stmt@(ExprStmt exp src_loc)]
	= tcAddSrcLoc src_loc $
	  tcSetErrCtxt (stmtCtxt stmt) $
	  tcExpr exp			 	`thenTc`    \ (exp', exp_lie, exp_ty) ->
	  returnTc ([ExprStmt exp' src_loc], exp_lie, exp_ty)

      go (stmt@(ExprStmt exp src_loc) : stmts)
	= tcAddSrcLoc src_loc 		(
	  tcSetErrCtxt (stmtCtxt stmt)	(
		tcExpr exp			`thenTc`    \ (exp', exp_lie, exp_ty) ->
		get_m_arg exp_ty 		`thenTc` \ a ->
		returnTc (a, exp', exp_lie)
	  ))					`thenTc` \ (a, exp',  exp_lie) -> 
	  go stmts				`thenTc` \ (stmts', stmts_lie, stmts_ty) ->
	  get_m_arg stmts_ty			`thenTc` \ b ->
	  returnTc (ExprStmtOut exp' src_loc a b : stmts',
		    exp_lie `plusLIE` stmts_lie `plusLIE` m_lie,
		    stmts_ty)

      go (stmt@(BindStmt pat exp src_loc) : stmts)
	= newMonoIds (collectPatBinders pat) mkBoxedTypeKind $ \ _ ->
	  tcAddSrcLoc src_loc		(
	  tcSetErrCtxt (stmtCtxt stmt)	(
		tcPat pat		`thenTc`    \ (pat', pat_lie, pat_ty) ->  
	    	tcExpr exp		`thenTc`    \ (exp', exp_lie, exp_ty) ->
		-- See comments with tcListComp on GeneratorQual

		get_m_arg exp_ty	`thenTc` \ a ->
		unifyTauTy pat_ty a	`thenTc_`
		returnTc (a, pat', exp', pat_lie `plusLIE` exp_lie)
	  ))				`thenTc` \ (a, pat', exp', stmt_lie) ->
	  go stmts			`thenTc` \ (stmts', stmts_lie, stmts_ty) ->
	  get_m_arg stmts_ty		`thenTc` \ b ->
	  returnTc (BindStmtOut pat' exp' src_loc a b : stmts',
		    stmt_lie `plusLIE` stmts_lie `plusLIE` m_lie `plusLIE` 
			(if failureFreePat pat' then emptyLIE else mz_lie),
		    stmts_ty)

      go (LetStmt binds : stmts)
	   = tcBindsAndThen		-- No error context, but a binding group is
		combine			-- rather a large thing for an error context anyway
		binds
		(go stmts)
	   where
	     combine binds' stmts' = LetStmt binds' : stmts'
    in

    go stmts		`thenTc` \ (stmts', final_lie, final_ty) ->
    returnTc (HsDoOut stmts' then_id zero_id src_loc,
	      final_lie,
	      final_ty)
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
      = tcLookupGlobalValue field_label	`thenNF_Tc` \ sel_id ->
	tcInstId sel_id			`thenNF_Tc` \ (_, _, tau) ->

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
  = ppHang (ppStr "No constructor has all these fields:")
	 4 (interpp'SP sty fields)
  where
    fields = [field | (field, _, _) <- rbinds]

recordUpdCtxt sty = ppStr "In a record update construct"

badFieldsCon con rbinds sty
  = ppHang (ppBesides [ppStr "Inconsistent constructor:", ppr sty con])
	 4 (ppBesides [ppStr "and fields:", interpp'SP sty fields])
  where
    fields = [field | (field, _, _) <- rbinds]
\end{code}
