%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcExpr]{Typecheck an expression}

\begin{code}
module TcExpr ( tcApp, tcExpr, tcPolyExpr, tcId ) where

#include "HsVersions.h"

import HsSyn		( HsExpr(..), HsLit(..), ArithSeqInfo(..), 
			  HsBinds(..), Stmt(..), StmtCtxt(..)
			)
import RnHsSyn		( RenamedHsExpr, RenamedRecordBinds )
import TcHsSyn		( TcExpr, TcRecordBinds,
			  mkHsTyApp, mkHsLet, maybeBoxedPrimType
			)

import TcMonad
import BasicTypes	( RecFlag(..) )

import Inst		( Inst, InstOrigin(..), OverloadedLit(..),
			  LIE, emptyLIE, unitLIE, plusLIE, plusLIEs, newOverloadedLit,
			  newMethod, instOverloadedFun, newDicts, instToId )
import TcBinds		( tcBindsAndThen )
import TcEnv		( tcInstId,
			  tcLookupValue, tcLookupClassByKey,
			  tcLookupValueByKey,
			  tcExtendGlobalTyVars, tcLookupValueMaybe,
			  tcLookupTyCon, tcLookupDataCon
			)
import TcMatches	( tcMatchesCase, tcMatchLambda, tcStmts )
import TcMonoType	( tcHsType, checkSigTyVars, sigCtxt )
import TcPat		( badFieldCon )
import TcSimplify	( tcSimplifyAndCheck )
import TcType		( TcType, TcTauType,
			  tcInstTyVars,
			  tcInstTcType, tcSplitRhoTy,
			  newTyVarTy, newTyVarTy_OpenKind, zonkTcType )

import Class		( Class )
import FieldLabel	( FieldLabel, fieldLabelName, fieldLabelType
			)
import Id		( idType, recordSelectorFieldLabel,
			  isRecordSelector,
			  Id
			)
import DataCon		( dataConFieldLabels, dataConSig, dataConId,
			  dataConStrictMarks, StrictnessMark(..)
			)
import Name		( Name )
import Type		( mkFunTy, mkAppTy, mkTyVarTy, mkTyVarTys,
			  splitFunTy_maybe, splitFunTys, isNotUsgTy,
			  mkTyConApp,
			  splitForAllTys, splitRhoTy,
			  isTauTy, tyVarsOfType, tyVarsOfTypes, 
			  isForAllTy, splitAlgTyConApp, splitAlgTyConApp_maybe,
			  boxedTypeKind, mkArrowKind,
			  tidyOpenType
			)
import Subst		( mkTopTyVarSubst, substTheta )
import UsageSPUtils     ( unannotTy )
import VarSet		( elemVarSet, mkVarSet )
import TyCon		( tyConDataCons )
import TysPrim		( intPrimTy, charPrimTy, doublePrimTy,
			  floatPrimTy, addrPrimTy
			)
import TysWiredIn	( boolTy, charTy, stringTy )
import PrelInfo		( ioTyCon_NAME )
import TcUnify		( unifyTauTy, unifyFunTy, unifyListTy, unifyTupleTy,
			  unifyUnboxedTupleTy )
import Unique		( cCallableClassKey, cReturnableClassKey, 
			  enumFromClassOpKey, enumFromThenClassOpKey,
			  enumFromToClassOpKey, enumFromThenToClassOpKey,
			  thenMClassOpKey, failMClassOpKey, returnMClassOpKey
			)
import Outputable
import Maybes		( maybeToBool, mapMaybe )
import ListSetOps	( minusList )
import Util
import CmdLineOpts      ( opt_WarnMissingFields )

\end{code}

%************************************************************************
%*									*
\subsection{Main wrappers}
%*									*
%************************************************************************

\begin{code}
tcExpr :: RenamedHsExpr			-- Expession to type check
	-> TcType 			-- Expected type (could be a polytpye)
	-> TcM s (TcExpr, LIE)

tcExpr expr ty | isForAllTy ty = -- Polymorphic case
				 tcPolyExpr expr ty 	`thenTc` \ (expr', lie, _, _, _) ->
				 returnTc (expr', lie)

	       | otherwise     = -- Monomorphic case
				 tcMonoExpr expr ty
\end{code}


%************************************************************************
%*									*
\subsection{@tcPolyExpr@ typchecks an application}
%*									*
%************************************************************************

\begin{code}
-- tcPolyExpr is like tcMonoExpr, except that the expected type
-- can be a polymorphic one.
tcPolyExpr :: RenamedHsExpr
	   -> TcType				-- Expected type
	   -> TcM s (TcExpr, LIE,		-- Generalised expr with expected type, and LIE
		     TcExpr, TcTauType, LIE)	-- Same thing, but instantiated; tau-type returned

tcPolyExpr arg expected_arg_ty
  = 	-- Ha!  The argument type of the function is a for-all type,
	-- An example of rank-2 polymorphism.

	-- To ensure that the forall'd type variables don't get unified with each
	-- other or any other types, we make fresh copy of the alleged type
    tcInstTcType expected_arg_ty 	`thenNF_Tc` \ (sig_tyvars, sig_rho) ->
    let
	(sig_theta, sig_tau) = splitRhoTy sig_rho
    in
	-- Type-check the arg and unify with expected type
    tcMonoExpr arg sig_tau				`thenTc` \ (arg', lie_arg) ->

	-- Check that the sig_tyvars havn't been constrained
	-- The interesting bit here is that we must include the free variables
	-- of the expected arg ty.  Here's an example:
	--	 runST (newVar True)
	-- Here, if we don't make a check, we'll get a type (ST s (MutVar s Bool))
	-- for (newVar True), with s fresh.  Then we unify with the runST's arg type
	-- forall s'. ST s' a. That unifies s' with s, and a with MutVar s Bool.
	-- So now s' isn't unconstrained because it's linked to a.
	-- Conclusion: include the free vars of the expected arg type in the
	-- list of "free vars" for the signature check.

    tcExtendGlobalTyVars (tyVarsOfType expected_arg_ty)		$
    tcAddErrCtxtM (sigCtxt sig_msg expected_arg_ty)		$

    checkSigTyVars sig_tyvars			`thenTc` \ zonked_sig_tyvars ->

    newDicts SignatureOrigin sig_theta		`thenNF_Tc` \ (sig_dicts, dict_ids) ->
	-- ToDo: better origin
    tcSimplifyAndCheck 
	(text "the type signature of an expression")
	(mkVarSet zonked_sig_tyvars)
	sig_dicts lie_arg			`thenTc` \ (free_insts, inst_binds) ->

    let
	    -- This HsLet binds any Insts which came out of the simplification.
	    -- It's a bit out of place here, but using AbsBind involves inventing
	    -- a couple of new names which seems worse.
	generalised_arg = TyLam zonked_sig_tyvars $
			  DictLam dict_ids $
			  mkHsLet inst_binds $ 
			  arg' 
    in
    returnTc ( generalised_arg, free_insts,
	       arg', sig_tau, lie_arg )
  where
    sig_msg ty = sep [ptext SLIT("In an expression with expected type:"),
		      nest 4 (ppr ty)]
\end{code}

%************************************************************************
%*									*
\subsection{The TAUT rules for variables}
%*									*
%************************************************************************

\begin{code}
tcMonoExpr :: RenamedHsExpr		-- Expession to type check
	   -> TcTauType 			-- Expected type (could be a type variable)
	   -> TcM s (TcExpr, LIE)

tcMonoExpr (HsVar name) res_ty
  = tcId name			`thenNF_Tc` \ (expr', lie, id_ty) ->
    unifyTauTy res_ty id_ty 	`thenTc_`

    -- Check that the result type doesn't have any nested for-alls.
    -- For example, a "build" on its own is no good; it must be
    -- applied to something.
    checkTc (isTauTy id_ty)
	    (lurkingRank2Err name id_ty) `thenTc_`

    returnTc (expr', lie)
\end{code}

%************************************************************************
%*									*
\subsection{Literals}
%*									*
%************************************************************************

Overloaded literals.

\begin{code}
tcMonoExpr (HsLit (HsInt i)) res_ty
  = newOverloadedLit (LiteralOrigin (HsInt i))
		     (OverloadedIntegral i)
		     res_ty  `thenNF_Tc` \ stuff ->
    returnTc stuff

tcMonoExpr (HsLit (HsFrac f)) res_ty
  = newOverloadedLit (LiteralOrigin (HsFrac f))
		     (OverloadedFractional f)
		     res_ty  `thenNF_Tc` \ stuff ->
    returnTc stuff


tcMonoExpr (HsLit lit@(HsLitLit s)) res_ty
  = tcLookupClassByKey cCallableClassKey		`thenNF_Tc` \ cCallableClass ->
    newDicts (LitLitOrigin (_UNPK_ s))
	     [(cCallableClass, [res_ty])]		`thenNF_Tc` \ (dicts, _) ->
    returnTc (HsLitOut lit res_ty, dicts)
\end{code}

Primitive literals:

\begin{code}
tcMonoExpr (HsLit lit@(HsCharPrim c)) res_ty
  = unifyTauTy res_ty charPrimTy		`thenTc_`
    returnTc (HsLitOut lit charPrimTy, emptyLIE)

tcMonoExpr (HsLit lit@(HsStringPrim s)) res_ty
  = unifyTauTy res_ty addrPrimTy		`thenTc_`
    returnTc (HsLitOut lit addrPrimTy, emptyLIE)

tcMonoExpr (HsLit lit@(HsIntPrim i)) res_ty
  = unifyTauTy res_ty intPrimTy		`thenTc_`
    returnTc (HsLitOut lit intPrimTy, emptyLIE)

tcMonoExpr (HsLit lit@(HsFloatPrim f)) res_ty
  = unifyTauTy res_ty floatPrimTy		`thenTc_`
    returnTc (HsLitOut lit floatPrimTy, emptyLIE)

tcMonoExpr (HsLit lit@(HsDoublePrim d)) res_ty
  = unifyTauTy res_ty doublePrimTy		`thenTc_`
    returnTc (HsLitOut lit doublePrimTy, emptyLIE)
\end{code}

Unoverloaded literals:

\begin{code}
tcMonoExpr (HsLit lit@(HsChar c)) res_ty
  = unifyTauTy res_ty charTy		`thenTc_`
    returnTc (HsLitOut lit charTy, emptyLIE)

tcMonoExpr (HsLit lit@(HsString str)) res_ty
  = unifyTauTy res_ty stringTy 		`thenTc_`
    returnTc (HsLitOut lit stringTy, emptyLIE)
\end{code}

%************************************************************************
%*									*
\subsection{Other expression forms}
%*									*
%************************************************************************

\begin{code}
tcMonoExpr (HsPar expr) res_ty -- preserve parens so printing needn't guess where they go
  = tcMonoExpr expr res_ty

-- perform the negate *before* overloading the integer, since the case
-- of minBound on Ints fails otherwise.  Could be done elsewhere, but
-- convenient to do it here.

tcMonoExpr (NegApp (HsLit (HsInt i)) neg) res_ty
  = tcMonoExpr (HsLit (HsInt (-i))) res_ty

tcMonoExpr (NegApp expr neg) res_ty 
  = tcMonoExpr (HsApp neg expr) res_ty

tcMonoExpr (HsLam match) res_ty
  = tcMatchLambda match res_ty 		`thenTc` \ (match',lie) ->
    returnTc (HsLam match', lie)

tcMonoExpr (HsApp e1 e2) res_ty = accum e1 [e2]
  where
    accum (HsApp e1 e2) args = accum e1 (e2:args)
    accum fun args
      = tcApp fun args res_ty 	`thenTc` \ (fun', args', lie) ->
	returnTc (foldl HsApp fun' args', lie)

-- equivalent to (op e1) e2:
tcMonoExpr (OpApp arg1 op fix arg2) res_ty
  = tcApp op [arg1,arg2] res_ty	`thenTc` \ (op', [arg1', arg2'], lie) ->
    returnTc (OpApp arg1' op' fix arg2', lie)
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

tcMonoExpr in_expr@(SectionL arg op) res_ty
  = tcApp op [arg] res_ty 		`thenTc` \ (op', [arg'], lie) ->

	-- Check that res_ty is a function type
	-- Without this check we barf in the desugarer on
	-- 	f op = (3 `op`)
	-- because it tries to desugar to
	--	f op = \r -> 3 op r
	-- so (3 `op`) had better be a function!
    tcAddErrCtxt (sectionLAppCtxt in_expr) $
    unifyFunTy res_ty			`thenTc_`

    returnTc (SectionL arg' op', lie)

-- Right sections, equivalent to \ x -> x op expr, or
--	\ x -> op x expr

tcMonoExpr in_expr@(SectionR op expr) res_ty
  = tcExpr_id op		`thenTc`    \ (op', lie1, op_ty) ->
    tcAddErrCtxt (sectionRAppCtxt in_expr) $
    split_fun_ty op_ty 2 {- two args -}			`thenTc` \ ([arg1_ty, arg2_ty], op_res_ty) ->
    tcMonoExpr expr arg2_ty				`thenTc` \ (expr',lie2) ->
    unifyTauTy res_ty (mkFunTy arg1_ty op_res_ty)	`thenTc_`
    returnTc (SectionR op' expr', lie1 `plusLIE` lie2)
\end{code}

The interesting thing about @ccall@ is that it is just a template
which we instantiate by filling in details about the types of its
argument and result (ie minimal typechecking is performed).  So, the
basic story is that we allocate a load of type variables (to hold the
arg/result types); unify them with the args/result; and store them for
later use.

\begin{code}
tcMonoExpr (CCall lbl args may_gc is_asm ignored_fake_result_ty) res_ty
  = 	-- Get the callable and returnable classes.
    tcLookupClassByKey cCallableClassKey	`thenNF_Tc` \ cCallableClass ->
    tcLookupClassByKey cReturnableClassKey	`thenNF_Tc` \ cReturnableClass ->
    tcLookupTyCon ioTyCon_NAME			`thenNF_Tc` \ ioTyCon ->
    let
	new_arg_dict (arg, arg_ty)
	  = newDicts (CCallOrigin (_UNPK_ lbl) (Just arg))
		     [(cCallableClass, [arg_ty])]	`thenNF_Tc` \ (arg_dicts, _) ->
	    returnNF_Tc arg_dicts	-- Actually a singleton bag

	result_origin = CCallOrigin (_UNPK_ lbl) Nothing {- Not an arg -}
    in

	-- Arguments
    let n_args = length args
	tv_idxs | n_args == 0 = []
		| otherwise   = [1..n_args]
    in
    mapNF_Tc (\ _ -> newTyVarTy_OpenKind) tv_idxs	`thenNF_Tc` \ arg_tys ->
    tcMonoExprs args arg_tys		   		`thenTc`    \ (args', args_lie) ->

	-- The argument types can be unboxed or boxed; the result
	-- type must, however, be boxed since it's an argument to the IO
	-- type constructor.
    newTyVarTy boxedTypeKind  		`thenNF_Tc` \ result_ty ->
    let
	io_result_ty = mkTyConApp ioTyCon [result_ty]
	[ioDataCon]  = tyConDataCons ioTyCon
    in
    unifyTauTy res_ty io_result_ty		`thenTc_`

	-- Construct the extra insts, which encode the
	-- constraints on the argument and result types.
    mapNF_Tc new_arg_dict (zipEqual "tcMonoExpr:CCall" args arg_tys)	`thenNF_Tc` \ ccarg_dicts_s ->
    newDicts result_origin [(cReturnableClass, [result_ty])]		`thenNF_Tc` \ (ccres_dict, _) ->
    returnTc (HsApp (HsVar (dataConId ioDataCon) `TyApp` [result_ty])
		    (CCall lbl args' may_gc is_asm result_ty),
		      -- do the wrapping in the newtype constructor here
	      foldr plusLIE ccres_dict ccarg_dicts_s `plusLIE` args_lie)
\end{code}

\begin{code}
tcMonoExpr (HsSCC lbl expr) res_ty
  = tcMonoExpr expr res_ty		`thenTc` \ (expr', lie) ->
    returnTc (HsSCC lbl expr', lie)

tcMonoExpr (HsLet binds expr) res_ty
  = tcBindsAndThen
	combiner
	binds 			-- Bindings to check
	tc_expr		`thenTc` \ (expr', lie) ->
    returnTc (expr', lie)
  where
    tc_expr = tcMonoExpr expr res_ty `thenTc` \ (expr', lie) ->
	      returnTc (expr', lie)
    combiner is_rec bind expr = HsLet (MonoBind bind [] is_rec) expr

tcMonoExpr in_expr@(HsCase scrut matches src_loc) res_ty
  = tcAddSrcLoc src_loc			$
    tcAddErrCtxt (caseCtxt in_expr)	$

	-- Typecheck the case alternatives first.
	-- The case patterns tend to give good type info to use
	-- when typechecking the scrutinee.  For example
	--	case (map f) of
	--	  (x:xs) -> ...
	-- will report that map is applied to too few arguments
	--
	-- Not only that, but it's better to check the matches on their
	-- own, so that we get the expected results for scoped type variables.
	--	f x = case x of
	--		(p::a, q::b) -> (q,p)
	-- The above should work: the match (p,q) -> (q,p) is polymorphic as
	-- claimed by the pattern signatures.  But if we typechecked the
	-- match with x in scope and x's type as the expected type, we'd be hosed.

    tcMatchesCase matches res_ty	`thenTc`    \ (scrut_ty, matches', lie2) ->

    tcAddErrCtxt (caseScrutCtxt scrut)	(
      tcMonoExpr scrut scrut_ty
    )					`thenTc`    \ (scrut',lie1) ->

    returnTc (HsCase scrut' matches' src_loc, plusLIE lie1 lie2)

tcMonoExpr (HsIf pred b1 b2 src_loc) res_ty
  = tcAddSrcLoc src_loc	$
    tcAddErrCtxt (predCtxt pred) (
    tcMonoExpr pred boolTy	)	`thenTc`    \ (pred',lie1) ->

    tcMonoExpr b1 res_ty		`thenTc`    \ (b1',lie2) ->
    tcMonoExpr b2 res_ty		`thenTc`    \ (b2',lie3) ->
    returnTc (HsIf pred' b1' b2' src_loc, plusLIE lie1 (plusLIE lie2 lie3))
\end{code}

\begin{code}
tcMonoExpr expr@(HsDo do_or_lc stmts src_loc) res_ty
  = tcDoStmts do_or_lc stmts src_loc res_ty
\end{code}

\begin{code}
tcMonoExpr in_expr@(ExplicitList exprs) res_ty	-- Non-empty list
  = unifyListTy res_ty                        `thenTc` \ elt_ty ->  
    mapAndUnzipTc (tc_elt elt_ty) exprs	      `thenTc` \ (exprs', lies) ->
    returnTc (ExplicitListOut elt_ty exprs', plusLIEs lies)
  where
    tc_elt elt_ty expr
      = tcAddErrCtxt (listCtxt expr) $
	tcMonoExpr expr elt_ty

tcMonoExpr (ExplicitTuple exprs boxed) res_ty
  = (if boxed
	then unifyTupleTy (length exprs) res_ty
	else unifyUnboxedTupleTy (length exprs) res_ty
						) `thenTc` \ arg_tys ->
    mapAndUnzipTc (\ (expr, arg_ty) -> tcMonoExpr expr arg_ty)
               (exprs `zip` arg_tys) -- we know they're of equal length.
               					`thenTc` \ (exprs', lies) ->
    returnTc (ExplicitTuple exprs' boxed, plusLIEs lies)

tcMonoExpr (RecordCon con_name rbinds) res_ty
  = tcId con_name			`thenNF_Tc` \ (con_expr, con_lie, con_tau) ->
    let
	(_, record_ty) = splitFunTys con_tau
    in
	-- Con is syntactically constrained to be a data constructor
    ASSERT( maybeToBool (splitAlgTyConApp_maybe record_ty ) )
    unifyTauTy res_ty record_ty          `thenTc_`

	-- Check that the record bindings match the constructor
    tcLookupDataCon con_name	`thenTc` \ (data_con, _, _) ->
    let
	bad_fields = badFields rbinds data_con
    in
    if not (null bad_fields) then
	mapNF_Tc (addErrTc . badFieldCon con_name) bad_fields	`thenNF_Tc_`
	failTc	-- Fail now, because tcRecordBinds will crash on a bad field
    else

	-- Typecheck the record bindings
    tcRecordBinds record_ty rbinds		`thenTc` \ (rbinds', rbinds_lie) ->
    
    let
      missing_s_fields = missingStrictFields rbinds data_con
    in
    checkTcM (null missing_s_fields)
	(mapNF_Tc (addErrTc . missingStrictFieldCon con_name) missing_s_fields `thenNF_Tc_`
	 returnNF_Tc ())  `thenNF_Tc_`
    let
      missing_fields = missingFields rbinds data_con
    in
    checkTcM (not (opt_WarnMissingFields && not (null missing_fields)))
	(mapNF_Tc ((warnTc True) . missingFieldCon con_name) missing_fields `thenNF_Tc_`
	 returnNF_Tc ())  `thenNF_Tc_`

    returnTc (RecordConOut data_con con_expr rbinds', con_lie `plusLIE` rbinds_lie)

-- The main complication with RecordUpd is that we need to explicitly
-- handle the *non-updated* fields.  Consider:
--
--	data T a b = MkT1 { fa :: a, fb :: b }
--		   | MkT2 { fa :: a, fc :: Int -> Int }
--		   | MkT3 { fd :: a }
--	
--	upd :: T a b -> c -> T a c
--	upd t x = t { fb = x}
--
-- The type signature on upd is correct (i.e. the result should not be (T a b))
-- because upd should be equivalent to:
--
--	upd t x = case t of 
--			MkT1 p q -> MkT1 p x
--			MkT2 a b -> MkT2 p b
--			MkT3 d   -> error ...
--
-- So we need to give a completely fresh type to the result record,
-- and then constrain it by the fields that are *not* updated ("p" above).
--
-- Note that because MkT3 doesn't contain all the fields being updated,
-- its RHS is simply an error, so it doesn't impose any type constraints
--
-- All this is done in STEP 4 below.

tcMonoExpr (RecordUpd record_expr rbinds) res_ty
  = tcAddErrCtxt recordUpdCtxt			$

	-- STEP 0
	-- Check that the field names are really field names
    ASSERT( not (null rbinds) )
    let 
	field_names = [field_name | (field_name, _, _) <- rbinds]
    in
    mapNF_Tc tcLookupValueMaybe field_names		`thenNF_Tc` \ maybe_sel_ids ->
    let
	bad_guys = [field_name | (field_name, maybe_sel_id) <- field_names `zip` maybe_sel_ids,
				 case maybe_sel_id of
					Nothing -> True
					Just sel_id -> not (isRecordSelector sel_id)
		   ]
    in
    mapNF_Tc (addErrTc . notSelector) bad_guys	`thenTc_`
    if not (null bad_guys) then
	failTc
    else
    
	-- STEP 1
	-- Figure out the tycon and data cons from the first field name
    let
	(Just sel_id : _)	  = maybe_sel_ids
	(_, tau)	      	  = ASSERT( isNotUsgTy (idType sel_id) )
                                    splitForAllTys (idType sel_id)
	Just (data_ty, _)     	  = splitFunTy_maybe tau	-- Must succeed since sel_id is a selector
	(tycon, _, data_cons) 	  = splitAlgTyConApp data_ty
	(con_tyvars, theta, _, _, _, _) = dataConSig (head data_cons)
    in
    tcInstTyVars con_tyvars			`thenNF_Tc` \ (_, result_inst_tys, _) ->

	-- STEP 2
	-- Check that at least one constructor has all the named fields
	-- i.e. has an empty set of bad fields returned by badFields
    checkTc (any (null . badFields rbinds) data_cons)
	    (badFieldsUpd rbinds)		`thenTc_`

	-- STEP 3
	-- Typecheck the update bindings.
	-- (Do this after checking for bad fields in case there's a field that
	--  doesn't match the constructor.)
    let
	result_record_ty = mkTyConApp tycon result_inst_tys
    in
    unifyTauTy res_ty result_record_ty          `thenTc_`
    tcRecordBinds result_record_ty rbinds	`thenTc` \ (rbinds', rbinds_lie) ->

	-- STEP 4
	-- Use the un-updated fields to find a vector of booleans saying
	-- which type arguments must be the same in updatee and result.
	--
	-- WARNING: this code assumes that all data_cons in a common tycon
	-- have FieldLabels abstracted over the same tyvars.
    let
	upd_field_lbls      = [recordSelectorFieldLabel sel_id | (sel_id, _, _) <- rbinds']
	con_field_lbls_s    = map dataConFieldLabels data_cons

		-- A constructor is only relevant to this process if
		-- it contains all the fields that are being updated
	relevant_field_lbls_s      = filter is_relevant con_field_lbls_s
	is_relevant con_field_lbls = all (`elem` con_field_lbls) upd_field_lbls

	non_upd_field_lbls  = concat relevant_field_lbls_s `minusList` upd_field_lbls
	common_tyvars       = tyVarsOfTypes (map fieldLabelType non_upd_field_lbls)

	mk_inst_ty (tyvar, result_inst_ty) 
	  | tyvar `elemVarSet` common_tyvars = returnNF_Tc result_inst_ty	-- Same as result type
	  | otherwise			            = newTyVarTy boxedTypeKind	-- Fresh type
    in
    mapNF_Tc mk_inst_ty (zip con_tyvars result_inst_tys)	`thenNF_Tc` \ inst_tys ->

	-- STEP 5
	-- Typecheck the expression to be updated
    let
	record_ty = mkTyConApp tycon inst_tys
    in
    tcMonoExpr record_expr record_ty			`thenTc`    \ (record_expr', record_lie) ->

	-- STEP 6
	-- Figure out the LIE we need.  We have to generate some 
	-- dictionaries for the data type context, since we are going to
	-- do some construction.
	--
	-- What dictionaries do we need?  For the moment we assume that all
	-- data constructors have the same context, and grab it from the first
	-- constructor.  If they have varying contexts then we'd have to 
	-- union the ones that could participate in the update.
    let
	(tyvars, theta, _, _, _, _) = dataConSig (head data_cons)
	inst_env = mkTopTyVarSubst tyvars result_inst_tys
	theta'   = substTheta inst_env theta
    in
    newDicts RecordUpdOrigin theta'		`thenNF_Tc` \ (con_lie, dicts) ->

	-- Phew!
    returnTc (RecordUpdOut record_expr' result_record_ty dicts rbinds', 
	      con_lie `plusLIE` record_lie `plusLIE` rbinds_lie)

tcMonoExpr (ArithSeqIn seq@(From expr)) res_ty
  = unifyListTy res_ty 				`thenTc` \ elt_ty ->  
    tcMonoExpr expr elt_ty		 	`thenTc` \ (expr', lie1) ->

    tcLookupValueByKey enumFromClassOpKey	`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq)
	      sel_id [elt_ty]			`thenNF_Tc` \ (lie2, enum_from_id) ->

    returnTc (ArithSeqOut (HsVar enum_from_id) (From expr'),
	      lie1 `plusLIE` lie2)

tcMonoExpr in_expr@(ArithSeqIn seq@(FromThen expr1 expr2)) res_ty
  = tcAddErrCtxt (arithSeqCtxt in_expr) $ 
    unifyListTy  res_ty         `thenTc`    \ elt_ty ->  
    tcMonoExpr expr1 elt_ty	`thenTc`    \ (expr1',lie1) ->
    tcMonoExpr expr2 elt_ty	`thenTc`    \ (expr2',lie2) ->
    tcLookupValueByKey enumFromThenClassOpKey		`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq)
	      sel_id [elt_ty]				`thenNF_Tc` \ (lie3, enum_from_then_id) ->

    returnTc (ArithSeqOut (HsVar enum_from_then_id)
			   (FromThen expr1' expr2'),
	      lie1 `plusLIE` lie2 `plusLIE` lie3)

tcMonoExpr in_expr@(ArithSeqIn seq@(FromTo expr1 expr2)) res_ty
  = tcAddErrCtxt (arithSeqCtxt in_expr) $
    unifyListTy  res_ty         `thenTc`    \ elt_ty ->  
    tcMonoExpr expr1 elt_ty	`thenTc`    \ (expr1',lie1) ->
    tcMonoExpr expr2 elt_ty	`thenTc`    \ (expr2',lie2) ->
    tcLookupValueByKey enumFromToClassOpKey	`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq)
	      sel_id [elt_ty] 				`thenNF_Tc` \ (lie3, enum_from_to_id) ->

    returnTc (ArithSeqOut (HsVar enum_from_to_id)
			  (FromTo expr1' expr2'),
	      lie1 `plusLIE` lie2 `plusLIE` lie3)

tcMonoExpr in_expr@(ArithSeqIn seq@(FromThenTo expr1 expr2 expr3)) res_ty
  = tcAddErrCtxt  (arithSeqCtxt in_expr) $
    unifyListTy  res_ty         `thenTc`    \ elt_ty ->  
    tcMonoExpr expr1 elt_ty	`thenTc`    \ (expr1',lie1) ->
    tcMonoExpr expr2 elt_ty	`thenTc`    \ (expr2',lie2) ->
    tcMonoExpr expr3 elt_ty	`thenTc`    \ (expr3',lie3) ->
    tcLookupValueByKey enumFromThenToClassOpKey	`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq)
	      sel_id [elt_ty]				`thenNF_Tc` \ (lie4, eft_id) ->

    returnTc (ArithSeqOut (HsVar eft_id)
			   (FromThenTo expr1' expr2' expr3'),
	      lie1 `plusLIE` lie2 `plusLIE` lie3 `plusLIE` lie4)
\end{code}

%************************************************************************
%*									*
\subsection{Expressions type signatures}
%*									*
%************************************************************************

\begin{code}
tcMonoExpr in_expr@(ExprWithTySig expr poly_ty) res_ty
 = tcSetErrCtxt (exprSigCtxt in_expr)	$
   tcHsType  poly_ty		`thenTc` \ sig_tc_ty ->

   if not (isForAllTy sig_tc_ty) then
	-- Easy case
	unifyTauTy sig_tc_ty res_ty	`thenTc_`
	tcMonoExpr expr sig_tc_ty

   else	-- Signature is polymorphic
	tcPolyExpr expr sig_tc_ty		`thenTc` \ (_, _, expr, expr_ty, lie) ->

	    -- Now match the signature type with res_ty.
	    -- We must not do this earlier, because res_ty might well
	    -- mention variables free in the environment, and we'd get
	    -- bogus complaints about not being able to for-all the
	    -- sig_tyvars
	unifyTauTy res_ty expr_ty			`thenTc_`

	    -- If everything is ok, return the stuff unchanged, except for
	    -- the effect of any substutions etc.  We simply discard the
	    -- result of the tcSimplifyAndCheck (inside tcPolyExpr), except for any default
	    -- resolution it may have done, which is recorded in the
	    -- substitution.
	returnTc (expr, lie)
\end{code}

Typecheck expression which in most cases will be an Id.

\begin{code}
tcExpr_id :: RenamedHsExpr
           -> TcM s (TcExpr,
 	       	     LIE,
	             TcType)
tcExpr_id id_expr
 = case id_expr of
	HsVar name -> tcId name			`thenNF_Tc` \ stuff -> 
		      returnTc stuff
	other	   -> newTyVarTy_OpenKind	`thenNF_Tc` \ id_ty ->
		      tcMonoExpr id_expr id_ty	`thenTc`    \ (id_expr', lie_id) ->
		      returnTc (id_expr', lie_id, id_ty) 
\end{code}

%************************************************************************
%*									*
\subsection{@tcApp@ typchecks an application}
%*									*
%************************************************************************

\begin{code}

tcApp :: RenamedHsExpr -> [RenamedHsExpr]   	-- Function and args
      -> TcType			    		-- Expected result type of application
      -> TcM s (TcExpr, [TcExpr],	    	-- Translated fun and args
		LIE)

tcApp fun args res_ty
  = 	-- First type-check the function
    tcExpr_id fun  				`thenTc` \ (fun', lie_fun, fun_ty) ->

    tcAddErrCtxt (wrongArgsCtxt "too many" fun args) (
	split_fun_ty fun_ty (length args)
    )						`thenTc` \ (expected_arg_tys, actual_result_ty) ->

	-- Unify with expected result before type-checking the args
	-- This is when we might detect a too-few args situation
    tcAddErrCtxtM (checkArgsCtxt fun args res_ty actual_result_ty) (
       unifyTauTy res_ty actual_result_ty
    )							`thenTc_`

	-- Now typecheck the args
    mapAndUnzipTc (tcArg fun)
	  (zip3 args expected_arg_tys [1..])	`thenTc` \ (args', lie_args_s) ->

    -- Check that the result type doesn't have any nested for-alls.
    -- For example, a "build" on its own is no good; it must be applied to something.
    checkTc (isTauTy actual_result_ty)
	    (lurkingRank2Err fun fun_ty)	`thenTc_`

    returnTc (fun', args', lie_fun `plusLIE` plusLIEs lie_args_s)


-- If an error happens we try to figure out whether the
-- function has been given too many or too few arguments,
-- and say so
checkArgsCtxt fun args expected_res_ty actual_res_ty tidy_env
  = zonkTcType expected_res_ty	  `thenNF_Tc` \ exp_ty' ->
    zonkTcType actual_res_ty	  `thenNF_Tc` \ act_ty' ->
    let
      (env1, exp_ty'') = tidyOpenType tidy_env exp_ty'
      (env2, act_ty'') = tidyOpenType env1     act_ty'
      (exp_args, _) = splitFunTys exp_ty''
      (act_args, _) = splitFunTys act_ty''

      message | length exp_args < length act_args = wrongArgsCtxt "too few" fun args
              | length exp_args > length act_args = wrongArgsCtxt "too many" fun args
	      | otherwise			  = appCtxt fun args
    in
    returnNF_Tc (env2, message)


split_fun_ty :: TcType		-- The type of the function
	     -> Int			-- Number of arguments
	     -> TcM s ([TcType],	-- Function argument types
		       TcType)	-- Function result types

split_fun_ty fun_ty 0 
  = returnTc ([], fun_ty)

split_fun_ty fun_ty n
  = 	-- Expect the function to have type A->B
    unifyFunTy fun_ty		`thenTc` \ (arg_ty, res_ty) ->
    split_fun_ty res_ty (n-1)	`thenTc` \ (arg_tys, final_res_ty) ->
    returnTc (arg_ty:arg_tys, final_res_ty)
\end{code}

\begin{code}
tcArg :: RenamedHsExpr			-- The function (for error messages)
      -> (RenamedHsExpr, TcType, Int)	-- Actual argument and expected arg type
      -> TcM s (TcExpr, LIE)	-- Resulting argument and LIE

tcArg the_fun (arg, expected_arg_ty, arg_no)
  = tcAddErrCtxt (funAppCtxt the_fun arg arg_no) $
    tcExpr arg expected_arg_ty
\end{code}


%************************************************************************
%*									*
\subsection{@tcId@ typchecks an identifier occurrence}
%*									*
%************************************************************************

Between the renamer and the first invocation of the UsageSP inference,
identifiers read from interface files will have usage information in
their types, whereas other identifiers will not.  The unannotTy here
in @tcId@ prevents this information from pointlessly propagating
further prior to the first usage inference.

\begin{code}
tcId :: Name -> NF_TcM s (TcExpr, LIE, TcType)

tcId name
  = 	-- Look up the Id and instantiate its type
    tcLookupValueMaybe name	`thenNF_Tc` \ maybe_local ->

    case maybe_local of
      Just tc_id -> instantiate_it (OccurrenceOf tc_id) (HsVar tc_id) (unannotTy (idType tc_id))

      Nothing ->    tcLookupValue name		`thenNF_Tc` \ id ->
		    tcInstId id			`thenNF_Tc` \ (tyvars, theta, tau) ->
		    instantiate_it2 (OccurrenceOf id) (HsVar id) tyvars theta tau

  where
	-- The instantiate_it loop runs round instantiating the Id.
	-- It has to be a loop because we are now prepared to entertain
	-- types like
	--		f:: forall a. Eq a => forall b. Baz b => tau
	-- We want to instantiate this to
	--		f2::tau		{f2 = f1 b (Baz b), f1 = f a (Eq a)}
    instantiate_it orig fun ty
      = tcInstTcType ty		`thenNF_Tc` \ (tyvars, rho) ->
	tcSplitRhoTy rho	`thenNF_Tc` \ (theta, tau) ->
	instantiate_it2 orig fun tyvars theta tau

    instantiate_it2 orig fun tyvars theta tau
      = if null theta then 	-- Is it overloaded?
		returnNF_Tc (mkHsTyApp fun arg_tys, emptyLIE, tau)
	else
		-- Yes, it's overloaded
	instOverloadedFun orig fun arg_tys theta tau	`thenNF_Tc` \ (fun', lie1) ->
	instantiate_it orig fun' tau			`thenNF_Tc` \ (expr, lie2, final_tau) ->
	returnNF_Tc (expr, lie1 `plusLIE` lie2, final_tau)

      where
	arg_tys	= mkTyVarTys tyvars
\end{code}

%************************************************************************
%*									*
\subsection{@tcDoStmts@ typechecks a {\em list} of do statements}
%*									*
%************************************************************************

\begin{code}
tcDoStmts do_or_lc stmts src_loc res_ty
  =	-- get the Monad and MonadZero classes
	-- create type consisting of a fresh monad tyvar
    ASSERT( not (null stmts) )
    tcAddSrcLoc src_loc	$

    newTyVarTy (mkArrowKind boxedTypeKind boxedTypeKind)	`thenNF_Tc` \ m ->
    newTyVarTy boxedTypeKind 					`thenNF_Tc` \ elt_ty ->
    unifyTauTy res_ty (mkAppTy m elt_ty)			`thenTc_`

	-- If it's a comprehension we're dealing with, 
	-- force it to be a list comprehension.
	-- (as of Haskell 98, monad comprehensions are no more.)
    (case do_or_lc of
       ListComp -> unifyListTy res_ty `thenTc_` returnTc ()
       _	-> returnTc ())					`thenTc_`

    tcStmts do_or_lc (mkAppTy m) stmts elt_ty	`thenTc`   \ (stmts', stmts_lie) ->

	-- Build the then and zero methods in case we need them
	-- It's important that "then" and "return" appear just once in the final LIE,
	-- not only for typechecker efficiency, but also because otherwise during
	-- simplification we end up with silly stuff like
	--	then = case d of (t,r) -> t
	--	then = then
	-- where the second "then" sees that it already exists in the "available" stuff.
	--
    tcLookupValueByKey returnMClassOpKey	`thenNF_Tc` \ return_sel_id ->
    tcLookupValueByKey thenMClassOpKey		`thenNF_Tc` \ then_sel_id ->
    tcLookupValueByKey failMClassOpKey		`thenNF_Tc` \ fail_sel_id ->
    newMethod DoOrigin return_sel_id [m]	`thenNF_Tc` \ (return_lie, return_id) ->
    newMethod DoOrigin then_sel_id [m]		`thenNF_Tc` \ (then_lie, then_id) ->
    newMethod DoOrigin fail_sel_id [m]		`thenNF_Tc` \ (fail_lie, fail_id) ->
    let
      monad_lie = then_lie `plusLIE` return_lie `plusLIE` fail_lie
    in
    returnTc (HsDoOut do_or_lc stmts' return_id then_id fail_id res_ty src_loc,
	      stmts_lie `plusLIE` monad_lie)
\end{code}


%************************************************************************
%*									*
\subsection{Record bindings}
%*									*
%************************************************************************

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
	:: TcType		-- Expected type of whole record
	-> RenamedRecordBinds
	-> TcM s (TcRecordBinds, LIE)

tcRecordBinds expected_record_ty rbinds
  = mapAndUnzipTc do_bind rbinds	`thenTc` \ (rbinds', lies) ->
    returnTc (rbinds', plusLIEs lies)
  where
    do_bind (field_label, rhs, pun_flag)
      = tcLookupValue field_label	`thenNF_Tc` \ sel_id ->
	ASSERT( isRecordSelector sel_id )
		-- This lookup and assertion will surely succeed, because
		-- we check that the fields are indeed record selectors
		-- before calling tcRecordBinds

	tcInstId sel_id			`thenNF_Tc` \ (_, _, tau) ->

		-- Record selectors all have type
		-- 	forall a1..an.  T a1 .. an -> tau
	ASSERT( maybeToBool (splitFunTy_maybe tau) )
	let
		-- Selector must have type RecordType -> FieldType
	  Just (record_ty, field_ty) = splitFunTy_maybe tau
	in
	unifyTauTy expected_record_ty record_ty		`thenTc_`
	tcPolyExpr rhs field_ty				`thenTc` \ (rhs', lie, _, _, _) ->
	returnTc ((sel_id, rhs', pun_flag), lie)

badFields rbinds data_con
  = [field_name | (field_name, _, _) <- rbinds,
		  not (field_name `elem` field_names)
    ]
  where
    field_names = map fieldLabelName (dataConFieldLabels data_con)

missingStrictFields rbinds data_con
  = [ fn | fn <- strict_field_names,
  		 not (fn `elem` field_names_used)
    ]
  where
    field_names_used = [ field_name | (field_name, _, _) <- rbinds ]
    strict_field_names = mapMaybe isStrict field_info

    isStrict (fl, MarkedStrict) = Just (fieldLabelName fl)
    isStrict _			= Nothing

    field_info = zip (dataConFieldLabels data_con)
    		     (dataConStrictMarks data_con)

missingFields rbinds data_con
  = [ fn | fn <- non_strict_field_names, not (fn `elem` field_names_used) ]
  where
    field_names_used = [ field_name | (field_name, _, _) <- rbinds ]

     -- missing strict fields have already been flagged as 
     -- being so, so leave them out here.
    non_strict_field_names = mapMaybe isn'tStrict field_info

    isn'tStrict (fl, MarkedStrict) = Nothing
    isn'tStrict (fl, _)            = Just (fieldLabelName fl)

    field_info = zip (dataConFieldLabels data_con)
    		     (dataConStrictMarks data_con)

\end{code}

%************************************************************************
%*									*
\subsection{@tcMonoExprs@ typechecks a {\em list} of expressions}
%*									*
%************************************************************************

\begin{code}
tcMonoExprs :: [RenamedHsExpr] -> [TcType] -> TcM s ([TcExpr], LIE)

tcMonoExprs [] [] = returnTc ([], emptyLIE)
tcMonoExprs (expr:exprs) (ty:tys)
 = tcMonoExpr  expr  ty		`thenTc` \ (expr',  lie1) ->
   tcMonoExprs exprs tys		`thenTc` \ (exprs', lie2) ->
   returnTc (expr':exprs', lie1 `plusLIE` lie2)
\end{code}


% =================================================

Errors and contexts
~~~~~~~~~~~~~~~~~~~

Mini-utils:
\begin{code}
pp_nest_hang :: String -> SDoc -> SDoc
pp_nest_hang lbl stuff = nest 2 (hang (text lbl) 4 stuff)
\end{code}

Boring and alphabetical:
\begin{code}
arithSeqCtxt expr
  = hang (ptext SLIT("In an arithmetic sequence:")) 4 (ppr expr)

caseCtxt expr
  = hang (ptext SLIT("In the case expression:")) 4 (ppr expr)

caseScrutCtxt expr
  = hang (ptext SLIT("In the scrutinee of a case expression:")) 4 (ppr expr)

exprSigCtxt expr
  = hang (ptext SLIT("In an expression with a type signature:"))
	 4 (ppr expr)

listCtxt expr
  = hang (ptext SLIT("In the list element:")) 4 (ppr expr)

predCtxt expr
  = hang (ptext SLIT("In the predicate expression:")) 4 (ppr expr)

sectionRAppCtxt expr
  = hang (ptext SLIT("In the right section:")) 4 (ppr expr)

sectionLAppCtxt expr
  = hang (ptext SLIT("In the left section:")) 4 (ppr expr)

funAppCtxt fun arg arg_no
  = hang (hsep [ ptext SLIT("In the"), speakNth arg_no, ptext SLIT("argument of"), 
		    quotes (ppr fun) <> text ", namely"])
	 4 (quotes (ppr arg))

wrongArgsCtxt too_many_or_few fun args
  = hang (ptext SLIT("Probable cause:") <+> quotes (ppr fun)
		    <+> ptext SLIT("is applied to") <+> text too_many_or_few 
		    <+> ptext SLIT("arguments in the call"))
	 4 (parens (ppr the_app))
  where
    the_app = foldl HsApp fun args	-- Used in error messages

appCtxt fun args
  = ptext SLIT("In the application") <+> quotes (ppr the_app)
  where
    the_app = foldl HsApp fun args	-- Used in error messages

lurkingRank2Err fun fun_ty
  = hang (hsep [ptext SLIT("Illegal use of"), quotes (ppr fun)])
	 4 (vcat [ptext SLIT("It is applied to too few arguments"),  
		  ptext SLIT("so that the result type has for-alls in it")])

rank2ArgCtxt arg expected_arg_ty
  = ptext SLIT("In a polymorphic function argument:") <+> ppr arg

badFieldsUpd rbinds
  = hang (ptext SLIT("No constructor has all these fields:"))
	 4 (pprQuotedList fields)
  where
    fields = [field | (field, _, _) <- rbinds]

recordUpdCtxt = ptext SLIT("In a record update construct")

notSelector field
  = hsep [quotes (ppr field), ptext SLIT("is not a record selector")]

illegalCcallTyErr isArg ty
  = hang (hsep [ptext SLIT("Unacceptable"), arg_or_res, ptext SLIT("type in _ccall_ or _casm_:")])
	 4 (hsep [ppr ty])
  where
   arg_or_res
    | isArg     = ptext SLIT("argument")
    | otherwise = ptext SLIT("result")


missingStrictFieldCon :: Name -> Name -> SDoc
missingStrictFieldCon con field
  = hsep [ptext SLIT("Constructor") <+> quotes (ppr con),
	  ptext SLIT("does not have the required strict field"), quotes (ppr field)]

missingFieldCon :: Name -> Name -> SDoc
missingFieldCon con field
  = hsep [ptext SLIT("Constructor") <+> quotes (ppr con),
	  ptext SLIT("does not have the field"), quotes (ppr field)]

\end{code}
