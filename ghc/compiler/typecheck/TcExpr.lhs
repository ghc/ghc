%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcExpr]{Typecheck an expression}

\begin{code}
module TcExpr ( tcApp, tcExpr, tcMonoExpr, tcPolyExpr, tcId ) where

#include "HsVersions.h"

import HsSyn		( HsExpr(..), HsLit(..), ArithSeqInfo(..), 
			  HsMatchContext(..), HsDoContext(..), mkMonoBind
			)
import RnHsSyn		( RenamedHsExpr, RenamedRecordBinds )
import TcHsSyn		( TcExpr, TcRecordBinds, mkHsLet )

import TcMonad
import BasicTypes	( RecFlag(..) )

import Inst		( InstOrigin(..), 
			  LIE, mkLIE, emptyLIE, unitLIE, plusLIE, plusLIEs,
			  newOverloadedLit, newMethod, newIPDict,
			  newDicts, 
			  instToId, tcInstId
			)
import TcBinds		( tcBindsAndThen )
import TcEnv		( tcLookupClass, tcLookupGlobalId, tcLookupGlobal_maybe,
			  tcLookupTyCon, tcLookupDataCon, tcLookupId,
			  tcExtendGlobalTyVars, tcLookupSyntaxName
			)
import TcMatches	( tcMatchesCase, tcMatchLambda, tcStmts )
import TcMonoType	( tcHsSigType, checkSigTyVars, sigCtxt )
import TcPat		( badFieldCon, simpleHsLitTy )
import TcSimplify	( tcSimplifyCheck, tcSimplifyIPs )
import TcMType		( tcInstTyVars, tcInstType, 
			  newTyVarTy, newTyVarTys, zonkTcType,
			  unifyTauTy, unifyFunTy, unifyListTy, unifyTupleTy
			)
import TcType		( tcSplitFunTys, tcSplitTyConApp,
			  isQualifiedTy, 
			  mkFunTy, mkAppTy, mkTyConTy,
			  mkTyConApp, mkClassPred, tcFunArgTy,
			  isTauTy, tyVarsOfType, tyVarsOfTypes, 
			  liftedTypeKind, openTypeKind, mkArrowKind,
			  tcSplitSigmaTy, tcTyConAppTyCon,
			  tidyOpenType
			)
import FieldLabel	( FieldLabel, fieldLabelName, fieldLabelType, fieldLabelTyCon )
import Id		( idType, recordSelectorFieldLabel, isRecordSelector )
import DataCon		( dataConFieldLabels, dataConSig, 
			  dataConStrictMarks
			)
import Demand		( isMarkedStrict )
import Name		( Name )
import TyCon		( TyCon, tyConTyVars, isAlgTyCon, tyConDataCons )
import Subst		( mkTopTyVarSubst, substTheta, substTy )
import VarSet		( elemVarSet )
import TysWiredIn	( boolTy, mkListTy, listTyCon )
import PrelNames	( cCallableClassName, 
			  cReturnableClassName, 
			  enumFromName, enumFromThenName, negateName,
			  enumFromToName, enumFromThenToName,
			  thenMName, failMName, returnMName, ioTyConName
			)
import Outputable
import Maybes		( maybeToBool )
import ListSetOps	( minusList )
import Util
import CmdLineOpts
import HscTypes		( TyThing(..) )

\end{code}

%************************************************************************
%*									*
\subsection{Main wrappers}
%*									*
%************************************************************************

\begin{code}
tcExpr :: RenamedHsExpr			-- Expession to type check
	-> TcType 			-- Expected type (could be a polytpye)
	-> TcM (TcExpr, LIE)

tcExpr expr ty | isQualifiedTy ty = -- Polymorphic case
				    tcPolyExpr expr ty 	`thenTc` \ (expr', lie, _, _, _) ->
				    returnTc (expr', lie)

	       | otherwise        = -- Monomorphic case
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
	   -> TcM (TcExpr, LIE,		-- Generalised expr with expected type, and LIE
		     TcExpr, TcTauType, LIE)	-- Same thing, but instantiated; tau-type returned

tcPolyExpr arg expected_arg_ty
  = 	-- Ha!  The argument type of the function is a for-all type,
	-- An example of rank-2 polymorphism.

	-- To ensure that the forall'd type variables don't get unified with each
	-- other or any other types, we make fresh copy of the alleged type
    tcInstType expected_arg_ty 		`thenNF_Tc` \ (sig_tyvars, sig_theta, sig_tau) ->
    let
	free_tvs = tyVarsOfType expected_arg_ty
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

    tcExtendGlobalTyVars free_tvs				  $
    tcAddErrCtxtM (sigCtxt sig_msg sig_tyvars sig_theta sig_tau)  $

    newDicts SignatureOrigin sig_theta		`thenNF_Tc` \ sig_dicts ->
    tcSimplifyCheck 
	(text "the type signature of an expression")
	sig_tyvars
	sig_dicts lie_arg			`thenTc` \ (free_insts, inst_binds) ->

    checkSigTyVars sig_tyvars free_tvs		`thenTc` \ zonked_sig_tyvars ->

    let
	    -- This HsLet binds any Insts which came out of the simplification.
	    -- It's a bit out of place here, but using AbsBind involves inventing
	    -- a couple of new names which seems worse.
	generalised_arg = TyLam zonked_sig_tyvars $
			  DictLam (map instToId sig_dicts) $
			  mkHsLet inst_binds $ 
			  arg' 
    in
    returnTc ( generalised_arg, free_insts,
	       arg', sig_tau, lie_arg )
  where
    sig_msg = ptext SLIT("When checking an expression type signature")
\end{code}

%************************************************************************
%*									*
\subsection{The TAUT rules for variables}
%*									*
%************************************************************************

\begin{code}
tcMonoExpr :: RenamedHsExpr		-- Expession to type check
	   -> TcTauType 		-- Expected type (could be a type variable)
	   -> TcM (TcExpr, LIE)

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

\begin{code}
tcMonoExpr (HsIPVar name) res_ty
  = newIPDict (IPOcc name) name res_ty		`thenNF_Tc` \ ip ->
    returnNF_Tc (HsIPVar (instToId ip), unitLIE ip)
\end{code}

%************************************************************************
%*									*
\subsection{Other expression forms}
%*									*
%************************************************************************

\begin{code}
tcMonoExpr (HsLit lit)     res_ty = tcLit lit res_ty
tcMonoExpr (HsOverLit lit) res_ty = newOverloadedLit (LiteralOrigin lit) lit res_ty
tcMonoExpr (HsPar expr)    res_ty = tcMonoExpr expr res_ty

tcMonoExpr (NegApp expr) res_ty
  = tcLookupSyntaxName negateName	`thenNF_Tc` \ neg ->
    tcMonoExpr (HsApp (HsVar neg) expr) res_ty

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
tcMonoExpr (HsCCall lbl args may_gc is_asm ignored_fake_result_ty) res_ty
  = 	-- Get the callable and returnable classes.
    tcLookupClass cCallableClassName	`thenNF_Tc` \ cCallableClass ->
    tcLookupClass cReturnableClassName	`thenNF_Tc` \ cReturnableClass ->
    tcLookupTyCon ioTyConName		`thenNF_Tc` \ ioTyCon ->
    let
	new_arg_dict (arg, arg_ty)
	  = newDicts (CCallOrigin (_UNPK_ lbl) (Just arg))
		     [mkClassPred cCallableClass [arg_ty]]	`thenNF_Tc` \ arg_dicts ->
	    returnNF_Tc arg_dicts	-- Actually a singleton bag

	result_origin = CCallOrigin (_UNPK_ lbl) Nothing {- Not an arg -}
    in

	-- Arguments
    let n_args = length args
	tv_idxs | n_args == 0 = []
		| otherwise   = [1..n_args]
    in
    newTyVarTys (length tv_idxs) openTypeKind		`thenNF_Tc` \ arg_tys ->
    tcMonoExprs args arg_tys		   		`thenTc`    \ (args', args_lie) ->

	-- The argument types can be unlifted or lifted; the result
	-- type must, however, be lifted since it's an argument to the IO
	-- type constructor.
    newTyVarTy liftedTypeKind  		`thenNF_Tc` \ result_ty ->
    let
	io_result_ty = mkTyConApp ioTyCon [result_ty]
    in
    unifyTauTy res_ty io_result_ty		`thenTc_`

	-- Construct the extra insts, which encode the
	-- constraints on the argument and result types.
    mapNF_Tc new_arg_dict (zipEqual "tcMonoExpr:CCall" args arg_tys)	`thenNF_Tc` \ ccarg_dicts_s ->
    newDicts result_origin [mkClassPred cReturnableClass [result_ty]]	`thenNF_Tc` \ ccres_dict ->
    returnTc (HsCCall lbl args' may_gc is_asm io_result_ty,
	      mkLIE (ccres_dict ++ concat ccarg_dicts_s) `plusLIE` args_lie)
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
    combiner is_rec bind expr = HsLet (mkMonoBind bind [] is_rec) expr

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

tcMonoExpr (ExplicitTuple exprs boxity) res_ty
  = unifyTupleTy boxity (length exprs) res_ty	`thenTc` \ arg_tys ->
    mapAndUnzipTc (\ (expr, arg_ty) -> tcMonoExpr expr arg_ty)
               (exprs `zip` arg_tys) -- we know they're of equal length.
               					`thenTc` \ (exprs', lies) ->
    returnTc (ExplicitTuple exprs' boxity, plusLIEs lies)

tcMonoExpr expr@(RecordCon con_name rbinds) res_ty
  = tcAddErrCtxt (recordConCtxt expr)		$
    tcId con_name			`thenNF_Tc` \ (con_expr, con_lie, con_tau) ->
    let
	(_, record_ty)   = tcSplitFunTys con_tau
	(tycon, ty_args) = tcSplitTyConApp record_ty
    in
    ASSERT( isAlgTyCon tycon )
    unifyTauTy res_ty record_ty          `thenTc_`

	-- Check that the record bindings match the constructor
	-- con_name is syntactically constrained to be a data constructor
    tcLookupDataCon con_name	`thenTc` \ data_con ->
    let
	bad_fields = badFields rbinds data_con
    in
    if not (null bad_fields) then
	mapNF_Tc (addErrTc . badFieldCon con_name) bad_fields	`thenNF_Tc_`
	failTc	-- Fail now, because tcRecordBinds will crash on a bad field
    else

	-- Typecheck the record bindings
    tcRecordBinds tycon ty_args rbinds		`thenTc` \ (rbinds', rbinds_lie) ->
    
    let
      (missing_s_fields, missing_fields) = missingFields rbinds data_con
    in
    checkTcM (null missing_s_fields)
	(mapNF_Tc (addErrTc . missingStrictFieldCon con_name) missing_s_fields `thenNF_Tc_`
	 returnNF_Tc ())  `thenNF_Tc_`
    doptsTc Opt_WarnMissingFields `thenNF_Tc` \ warn ->
    checkTcM (not (warn && not (null missing_fields)))
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

tcMonoExpr expr@(RecordUpd record_expr rbinds) res_ty
  = tcAddErrCtxt (recordUpdCtxt	expr)		$

	-- STEP 0
	-- Check that the field names are really field names
    ASSERT( not (null rbinds) )
    let 
	field_names = [field_name | (field_name, _, _) <- rbinds]
    in
    mapNF_Tc tcLookupGlobal_maybe field_names		`thenNF_Tc` \ maybe_sel_ids ->
    let
	bad_guys = [ addErrTc (notSelector field_name) 
		   | (field_name, maybe_sel_id) <- field_names `zip` maybe_sel_ids,
		      case maybe_sel_id of
			Just (AnId sel_id) -> not (isRecordSelector sel_id)
			other		   -> True
		   ]
    in
    checkTcM (null bad_guys) (listNF_Tc bad_guys `thenNF_Tc_` failTc)	`thenTc_`
    
	-- STEP 1
	-- Figure out the tycon and data cons from the first field name
    let
		-- It's OK to use the non-tc splitters here (for a selector)
	(Just (AnId sel_id) : _)    = maybe_sel_ids
	(_, _, tau)	      	    = tcSplitSigmaTy (idType sel_id)	-- Selectors can be overloaded
									-- when the data type has a context
	data_ty		     	    = tcFunArgTy tau			-- Must succeed since sel_id is a selector
	tycon		 	    = tcTyConAppTyCon data_ty
	data_cons		    = tyConDataCons tycon
	(con_tyvars, _, _, _, _, _) = dataConSig (head data_cons)
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
    tcRecordBinds tycon result_inst_tys rbinds	`thenTc` \ (rbinds', rbinds_lie) ->

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
	  | otherwise			            = newTyVarTy liftedTypeKind	-- Fresh type
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
    newDicts RecordUpdOrigin theta'	`thenNF_Tc` \ dicts ->

	-- Phew!
    returnTc (RecordUpdOut record_expr' result_record_ty (map instToId dicts) rbinds', 
	      mkLIE dicts `plusLIE` record_lie `plusLIE` rbinds_lie)

tcMonoExpr (ArithSeqIn seq@(From expr)) res_ty
  = unifyListTy res_ty 				`thenTc` \ elt_ty ->  
    tcMonoExpr expr elt_ty		 	`thenTc` \ (expr', lie1) ->

    tcLookupGlobalId enumFromName		`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq)
	      sel_id [elt_ty]			`thenNF_Tc` \ enum_from ->

    returnTc (ArithSeqOut (HsVar (instToId enum_from)) (From expr'),
	      lie1 `plusLIE` unitLIE enum_from)

tcMonoExpr in_expr@(ArithSeqIn seq@(FromThen expr1 expr2)) res_ty
  = tcAddErrCtxt (arithSeqCtxt in_expr) $ 
    unifyListTy  res_ty         			`thenTc`    \ elt_ty ->  
    tcMonoExpr expr1 elt_ty				`thenTc`    \ (expr1',lie1) ->
    tcMonoExpr expr2 elt_ty				`thenTc`    \ (expr2',lie2) ->
    tcLookupGlobalId enumFromThenName			`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq) sel_id [elt_ty]	`thenNF_Tc` \ enum_from_then ->

    returnTc (ArithSeqOut (HsVar (instToId enum_from_then))
			  (FromThen expr1' expr2'),
	      lie1 `plusLIE` lie2 `plusLIE` unitLIE enum_from_then)

tcMonoExpr in_expr@(ArithSeqIn seq@(FromTo expr1 expr2)) res_ty
  = tcAddErrCtxt (arithSeqCtxt in_expr) $
    unifyListTy  res_ty         			`thenTc`    \ elt_ty ->  
    tcMonoExpr expr1 elt_ty				`thenTc`    \ (expr1',lie1) ->
    tcMonoExpr expr2 elt_ty				`thenTc`    \ (expr2',lie2) ->
    tcLookupGlobalId enumFromToName			`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq) sel_id [elt_ty]	`thenNF_Tc` \ enum_from_to ->

    returnTc (ArithSeqOut (HsVar (instToId enum_from_to))
			  (FromTo expr1' expr2'),
	      lie1 `plusLIE` lie2 `plusLIE` unitLIE enum_from_to)

tcMonoExpr in_expr@(ArithSeqIn seq@(FromThenTo expr1 expr2 expr3)) res_ty
  = tcAddErrCtxt  (arithSeqCtxt in_expr) $
    unifyListTy  res_ty         			`thenTc`    \ elt_ty ->  
    tcMonoExpr expr1 elt_ty				`thenTc`    \ (expr1',lie1) ->
    tcMonoExpr expr2 elt_ty				`thenTc`    \ (expr2',lie2) ->
    tcMonoExpr expr3 elt_ty				`thenTc`    \ (expr3',lie3) ->
    tcLookupGlobalId enumFromThenToName			`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq) sel_id [elt_ty]	`thenNF_Tc` \ eft ->

    returnTc (ArithSeqOut (HsVar (instToId eft))
			  (FromThenTo expr1' expr2' expr3'),
	      lie1 `plusLIE` lie2 `plusLIE` lie3 `plusLIE` unitLIE eft)
\end{code}

%************************************************************************
%*									*
\subsection{Expressions type signatures}
%*									*
%************************************************************************

\begin{code}
tcMonoExpr in_expr@(ExprWithTySig expr poly_ty) res_ty
 = tcAddErrCtxt (exprSigCtxt in_expr)	$
   tcHsSigType  poly_ty		`thenTc` \ sig_tc_ty ->

   if not (isQualifiedTy sig_tc_ty) then
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
	    -- result of the tcSimplifyCheck (inside tcPolyExpr), except for any default
	    -- resolution it may have done, which is recorded in the
	    -- substitution.
	returnTc (expr, lie)
\end{code}

Implicit Parameter bindings.

\begin{code}
tcMonoExpr (HsWith expr binds) res_ty
  = tcMonoExpr expr res_ty			`thenTc` \ (expr', expr_lie) ->
    mapAndUnzipTc tcIPBind binds		`thenTc` \ (pairs, bind_lies) ->

	-- If the binding binds ?x = E, we  must now 
	-- discharge any ?x constraints in expr_lie
    tcSimplifyIPs (map fst pairs) expr_lie	`thenTc` \ (expr_lie', dict_binds) ->
    let
	binds' = [(instToId ip, rhs) | (ip,rhs) <- pairs]
	expr'' = HsLet (mkMonoBind dict_binds [] Recursive) expr'
    in
    returnTc (HsWith expr'' binds', expr_lie' `plusLIE` plusLIEs bind_lies)

tcIPBind (name, expr)
  = newTyVarTy openTypeKind		`thenTc` \ ty ->
    tcGetSrcLoc				`thenTc` \ loc ->
    newIPDict (IPBind name) name ty	`thenNF_Tc` \ ip ->
    tcMonoExpr expr ty			`thenTc` \ (expr', lie) ->
    returnTc ((ip, expr'), lie)
\end{code}

%************************************************************************
%*									*
\subsection{@tcApp@ typchecks an application}
%*									*
%************************************************************************

\begin{code}

tcApp :: RenamedHsExpr -> [RenamedHsExpr]   	-- Function and args
      -> TcType			    		-- Expected result type of application
      -> TcM (TcExpr, [TcExpr],	    	-- Translated fun and args
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
	    (lurkingRank2Err fun actual_result_ty)	`thenTc_`

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
      (exp_args, _)    = tcSplitFunTys exp_ty''
      (act_args, _)    = tcSplitFunTys act_ty''

      message | length exp_args < length act_args = wrongArgsCtxt "too few" fun args
              | length exp_args > length act_args = wrongArgsCtxt "too many" fun args
	      | otherwise			  = appCtxt fun args
    in
    returnNF_Tc (env2, message)


split_fun_ty :: TcType		-- The type of the function
	     -> Int			-- Number of arguments
	     -> TcM ([TcType],	-- Function argument types
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
      -> TcM (TcExpr, LIE)	-- Resulting argument and LIE

tcArg the_fun (arg, expected_arg_ty, arg_no)
  = tcAddErrCtxt (funAppCtxt the_fun arg arg_no) $
    tcExpr arg expected_arg_ty
\end{code}


%************************************************************************
%*									*
\subsection{@tcId@ typchecks an identifier occurrence}
%*									*
%************************************************************************

\begin{code}
tcId :: Name -> NF_TcM (TcExpr, LIE, TcType)
tcId name	-- Look up the Id and instantiate its type
  = tcLookupId name			`thenNF_Tc` \ id ->
    tcInstId id
\end{code}

Typecheck expression which in most cases will be an Id.

\begin{code}
tcExpr_id :: RenamedHsExpr -> TcM (TcExpr, LIE, TcType)
tcExpr_id (HsVar name) = tcId name
tcExpr_id expr         = newTyVarTy openTypeKind	`thenNF_Tc` \ id_ty ->
			 tcMonoExpr expr id_ty	`thenTc`    \ (expr', lie_id) ->
		         returnTc (expr', lie_id, id_ty) 
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

	-- If it's a comprehension we're dealing with, 
	-- force it to be a list comprehension.
	-- (as of Haskell 98, monad comprehensions are no more.)
    (case do_or_lc of
       ListComp -> unifyListTy res_ty			`thenTc` \ elt_ty ->
		   returnNF_Tc (mkTyConTy listTyCon, (mkListTy, elt_ty))

       _	-> newTyVarTy (mkArrowKind liftedTypeKind liftedTypeKind)	`thenNF_Tc` \ m_ty ->
		   newTyVarTy liftedTypeKind 					`thenNF_Tc` \ elt_ty ->
		   unifyTauTy res_ty (mkAppTy m_ty elt_ty)				`thenTc_`
		   returnNF_Tc (m_ty, (mkAppTy m_ty, elt_ty))
    )							`thenNF_Tc` \ (tc_ty, m_ty) ->

    tcStmts (DoCtxt do_or_lc) m_ty stmts		`thenTc`   \ (stmts', stmts_lie) ->

	-- Build the then and zero methods in case we need them
	-- It's important that "then" and "return" appear just once in the final LIE,
	-- not only for typechecker efficiency, but also because otherwise during
	-- simplification we end up with silly stuff like
	--	then = case d of (t,r) -> t
	--	then = then
	-- where the second "then" sees that it already exists in the "available" stuff.
	--
    tcLookupGlobalId returnMName		`thenNF_Tc` \ return_sel_id ->
    tcLookupGlobalId thenMName			`thenNF_Tc` \ then_sel_id ->
    tcLookupGlobalId failMName			`thenNF_Tc` \ fail_sel_id ->
    newMethod DoOrigin return_sel_id [tc_ty]	`thenNF_Tc` \ return_inst ->
    newMethod DoOrigin then_sel_id   [tc_ty]	`thenNF_Tc` \ then_inst ->
    newMethod DoOrigin fail_sel_id   [tc_ty]	`thenNF_Tc` \ fail_inst ->
    let
	monad_lie = mkLIE [return_inst, then_inst, fail_inst]
    in
    returnTc (HsDoOut do_or_lc stmts'
		      (instToId return_inst) (instToId then_inst) (instToId fail_inst)
		      res_ty src_loc,
	      stmts_lie `plusLIE` monad_lie)
\end{code}


%************************************************************************
%*									*
\subsection{Record bindings}
%*									*
%************************************************************************

Game plan for record bindings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. Find the TyCon for the bindings, from the first field label.

2. Instantiate its tyvars and unify (T a1 .. an) with expected_ty.

For each binding field = value

3. Instantiate the field type (from the field label) using the type
   envt from step 2.

4  Type check the value using tcArg, passing the field type as 
   the expected argument type.

This extends OK when the field types are universally quantified.

	
\begin{code}
tcRecordBinds
	:: TyCon		-- Type constructor for the record
	-> [TcType]		-- Args of this type constructor
	-> RenamedRecordBinds
	-> TcM (TcRecordBinds, LIE)

tcRecordBinds tycon ty_args rbinds
  = mapAndUnzipTc do_bind rbinds	`thenTc` \ (rbinds', lies) ->
    returnTc (rbinds', plusLIEs lies)
  where
    tenv = mkTopTyVarSubst (tyConTyVars tycon) ty_args

    do_bind (field_lbl_name, rhs, pun_flag)
      = tcLookupGlobalId field_lbl_name		`thenNF_Tc` \ sel_id ->
	let
	    field_lbl = recordSelectorFieldLabel sel_id
	    field_ty  = substTy tenv (fieldLabelType field_lbl)
	in
	ASSERT( isRecordSelector sel_id )
		-- This lookup and assertion will surely succeed, because
		-- we check that the fields are indeed record selectors
		-- before calling tcRecordBinds
	ASSERT2( fieldLabelTyCon field_lbl == tycon, ppr field_lbl )
		-- The caller of tcRecordBinds has already checked
		-- that all the fields come from the same type

	tcPolyExpr rhs field_ty 	`thenTc` \ (rhs', lie, _, _, _) ->

	returnTc ((sel_id, rhs', pun_flag), lie)

badFields rbinds data_con
  = [field_name | (field_name, _, _) <- rbinds,
		  not (field_name `elem` field_names)
    ]
  where
    field_names = map fieldLabelName (dataConFieldLabels data_con)

missingFields rbinds data_con
  | null field_labels = ([], [])	-- Not declared as a record;
					-- But C{} is still valid
  | otherwise	
  = (missing_strict_fields, other_missing_fields)
  where
    missing_strict_fields
	= [ fl | (fl, str) <- field_info,
	  	 isMarkedStrict str,
	  	 not (fieldLabelName fl `elem` field_names_used)
	  ]
    other_missing_fields
	= [ fl | (fl, str) <- field_info,
	  	 not (isMarkedStrict str),
	  	 not (fieldLabelName fl `elem` field_names_used)
	  ]

    field_names_used = [ field_name | (field_name, _, _) <- rbinds ]
    field_labels     = dataConFieldLabels data_con

    field_info = zipEqual "missingFields"
			  field_labels
	  		  (drop (length ex_theta) (dataConStrictMarks data_con))
	-- The 'drop' is because dataConStrictMarks
	-- includes the existential dictionaries
    (_, _, _, ex_theta, _, _) = dataConSig data_con
\end{code}

%************************************************************************
%*									*
\subsection{@tcMonoExprs@ typechecks a {\em list} of expressions}
%*									*
%************************************************************************

\begin{code}
tcMonoExprs :: [RenamedHsExpr] -> [TcType] -> TcM ([TcExpr], LIE)

tcMonoExprs [] [] = returnTc ([], emptyLIE)
tcMonoExprs (expr:exprs) (ty:tys)
 = tcMonoExpr  expr  ty		`thenTc` \ (expr',  lie1) ->
   tcMonoExprs exprs tys		`thenTc` \ (exprs', lie2) ->
   returnTc (expr':exprs', lie1 `plusLIE` lie2)
\end{code}


%************************************************************************
%*									*
\subsection{Literals}
%*									*
%************************************************************************

Overloaded literals.

\begin{code}
tcLit :: HsLit -> TcType -> TcM (TcExpr, LIE)
tcLit (HsLitLit s _) res_ty
  = tcLookupClass cCallableClassName			`thenNF_Tc` \ cCallableClass ->
    newDicts (LitLitOrigin (_UNPK_ s))
	     [mkClassPred cCallableClass [res_ty]]	`thenNF_Tc` \ dicts ->
    returnTc (HsLit (HsLitLit s res_ty), mkLIE dicts)

tcLit lit res_ty 
  = unifyTauTy res_ty (simpleHsLitTy lit)		`thenTc_`
    returnTc (HsLit lit, emptyLIE)
\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

Mini-utils:

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
		  ptext SLIT("so that the result type has for-alls in it:") <+> ppr fun_ty])

badFieldsUpd rbinds
  = hang (ptext SLIT("No constructor has all these fields:"))
	 4 (pprQuotedList fields)
  where
    fields = [field | (field, _, _) <- rbinds]

recordUpdCtxt expr = ptext SLIT("In the record update:") <+> ppr expr
recordConCtxt expr = ptext SLIT("In the record construction:") <+> ppr expr

notSelector field
  = hsep [quotes (ppr field), ptext SLIT("is not a record selector")]

missingStrictFieldCon :: Name -> FieldLabel -> SDoc
missingStrictFieldCon con field
  = hsep [ptext SLIT("Constructor") <+> quotes (ppr con),
	  ptext SLIT("does not have the required strict field"), quotes (ppr field)]

missingFieldCon :: Name -> FieldLabel -> SDoc
missingFieldCon con field
  = hsep [ptext SLIT("Field") <+> quotes (ppr field),
	  ptext SLIT("is not initialised")]
\end{code}
