%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcExpr]{Typecheck an expression}

\begin{code}
module TcExpr ( tcExpr, tcMonoExpr, tcId ) where

#include "HsVersions.h"

import HsSyn		( HsExpr(..), HsLit(..), ArithSeqInfo(..), 
			  HsMatchContext(..), HsDoContext(..), mkMonoBind
			)
import RnHsSyn		( RenamedHsExpr, RenamedRecordBinds )
import TcHsSyn		( TcExpr, TcRecordBinds, simpleHsLitTy  )

import TcMonad
import TcUnify		( tcSub, tcGen, (<$>),
			  unifyTauTy, unifyFunTy, unifyListTy, unifyPArrTy,
			  unifyTupleTy )
import BasicTypes	( RecFlag(..),  isMarkedStrict )
import Inst		( InstOrigin(..), 
			  LIE, mkLIE, emptyLIE, unitLIE, plusLIE, plusLIEs,
			  newOverloadedLit, newMethod, newIPDict,
			  newDicts, newMethodWithGivenTy,
			  instToId, tcInstCall
			)
import TcBinds		( tcBindsAndThen )
import TcEnv		( tcLookupClass, tcLookupGlobalId, tcLookupGlobal_maybe,
			  tcLookupTyCon, tcLookupDataCon, tcLookupId
			)
import TcMatches	( tcMatchesCase, tcMatchLambda, tcStmts )
import TcMonoType	( tcHsSigType, UserTypeCtxt(..) )
import TcPat		( badFieldCon )
import TcSimplify	( tcSimplifyIPs )
import TcMType		( tcInstTyVars, tcInstType, newHoleTyVarTy,
			  newTyVarTy, newTyVarTys, zonkTcType )
import TcType		( TcType, TcSigmaType, TcPhiType,
			  tcSplitFunTys, tcSplitTyConApp, mkTyVarTys,
			  isSigmaTy, mkFunTy, mkAppTy, mkTyConTy,
			  mkTyConApp, mkClassPred, tcFunArgTy,
			  tyVarsOfTypes, isLinearPred,
			  liftedTypeKind, openTypeKind, mkArrowKind,
			  tcSplitSigmaTy, tcTyConAppTyCon,
			  tidyOpenType
			)
import FieldLabel	( FieldLabel, fieldLabelName, fieldLabelType, fieldLabelTyCon )
import Id		( idType, recordSelectorFieldLabel, isRecordSelector )
import DataCon		( dataConFieldLabels, dataConSig, 
			  dataConStrictMarks
			)
import Name		( Name )
import TyCon		( TyCon, tyConTyVars, isAlgTyCon, tyConDataCons )
import Subst		( mkTopTyVarSubst, substTheta, substTy )
import VarSet		( emptyVarSet, elemVarSet )
import TysWiredIn	( boolTy, mkListTy, mkPArrTy, listTyCon, parrTyCon )
import PrelNames	( cCallableClassName, 
			  cReturnableClassName, 
			  enumFromName, enumFromThenName, 
			  enumFromToName, enumFromThenToName,
			  enumFromToPName, enumFromThenToPName,
			  thenMName, failMName, returnMName, ioTyConName
			)
import Outputable
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
tcExpr :: RenamedHsExpr		-- Expession to type check
	-> TcSigmaType 		-- Expected type (could be a polytpye)
	-> TcM (TcExpr, LIE)	-- Generalised expr with expected type, and LIE

tcExpr expr expected_ty 
  | not (isSigmaTy expected_ty)  -- Monomorphic case
  = tcMonoExpr expr expected_ty

  | otherwise
  = tcGen expected_ty emptyVarSet (
	tcMonoExpr expr
    )					`thenTc` \ (gen_fn, expr', lie) ->
    returnTc (gen_fn <$> expr', lie)
\end{code}


%************************************************************************
%*									*
\subsection{The TAUT rules for variables}
%*									*
%************************************************************************

\begin{code}
tcMonoExpr :: RenamedHsExpr		-- Expession to type check
	   -> TcPhiType 		-- Expected type (could be a type variable)
					-- Definitely no foralls at the top
					-- Can be a 'hole'.
	   -> TcM (TcExpr, LIE)

tcMonoExpr (HsVar name) res_ty
  = tcId name			`thenNF_Tc` \ (expr', lie1, id_ty) ->
    tcSub res_ty id_ty 		`thenTc` \ (co_fn, lie2) ->
    returnTc (co_fn <$> expr', lie1 `plusLIE` lie2)

tcMonoExpr (HsIPVar ip) res_ty
  = 	-- Implicit parameters must have a *tau-type* not a 
	-- type scheme.  We enforce this by creating a fresh
	-- type variable as its type.  (Because res_ty may not
	-- be a tau-type.)
    newTyVarTy openTypeKind		`thenNF_Tc` \ ip_ty ->
    newIPDict (IPOcc ip) ip ip_ty 	`thenNF_Tc` \ (ip', inst) ->
    tcSub res_ty ip_ty			`thenTc` \ (co_fn, lie) ->
    returnNF_Tc (co_fn <$> HsIPVar ip', lie `plusLIE` unitLIE inst)
\end{code}


%************************************************************************
%*									*
\subsection{Expressions type signatures}
%*									*
%************************************************************************

\begin{code}
tcMonoExpr in_expr@(ExprWithTySig expr poly_ty) res_ty
 = tcHsSigType ExprSigCtxt poly_ty	`thenTc` \ sig_tc_ty ->
   tcExpr expr sig_tc_ty		`thenTc` \ (expr', lie1) ->

	-- Must instantiate the outer for-alls of sig_tc_ty
	-- else we risk instantiating a ? res_ty to a forall-type
	-- which breaks the invariant that tcMonoExpr only returns phi-types
   tcAddErrCtxt (exprSigCtxt in_expr)	$
   tcInstCall SignatureOrigin sig_tc_ty	`thenNF_Tc` \ (inst_fn, lie2, inst_sig_ty) ->
   tcSub res_ty inst_sig_ty		`thenTc` \ (co_fn, lie3) ->

   returnTc (co_fn <$> inst_fn expr', lie1 `plusLIE` lie2 `plusLIE` lie3)
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

tcMonoExpr (NegApp expr neg_name) res_ty
  = tcMonoExpr (HsApp (HsVar neg_name) expr) res_ty

tcMonoExpr (HsLam match) res_ty
  = tcMatchLambda match res_ty 		`thenTc` \ (match',lie) ->
    returnTc (HsLam match', lie)

tcMonoExpr (HsApp e1 e2) res_ty 
  = tcApp e1 [e2] res_ty
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

tcMonoExpr in_expr@(SectionL arg1 op) res_ty
  = tcExpr_id op				`thenTc` \ (op', lie1, op_ty) ->
    split_fun_ty op_ty 2 {- two args -}		`thenTc` \ ([arg1_ty, arg2_ty], op_res_ty) ->
    tcArg op (arg1, arg1_ty, 1)			`thenTc` \ (arg1',lie2) ->
    tcAddErrCtxt (exprCtxt in_expr)		$
    tcSub res_ty (mkFunTy arg2_ty op_res_ty)	`thenTc` \ (co_fn, lie3) ->
    returnTc (co_fn <$> SectionL arg1' op', lie1 `plusLIE` lie2 `plusLIE` lie3)

-- Right sections, equivalent to \ x -> x op expr, or
--	\ x -> op x expr

tcMonoExpr in_expr@(SectionR op arg2) res_ty
  = tcExpr_id op				`thenTc` \ (op', lie1, op_ty) ->
    split_fun_ty op_ty 2 {- two args -}		`thenTc` \ ([arg1_ty, arg2_ty], op_res_ty) ->
    tcArg op (arg2, arg2_ty, 2)			`thenTc` \ (arg2',lie2) ->
    tcAddErrCtxt (exprCtxt in_expr)		$
    tcSub res_ty (mkFunTy arg1_ty op_res_ty)	`thenTc` \ (co_fn, lie3) ->
    returnTc (co_fn <$> SectionR op' arg2', lie1 `plusLIE` lie2 `plusLIE` lie3)

-- equivalent to (op e1) e2:

tcMonoExpr in_expr@(OpApp arg1 op fix arg2) res_ty
  = tcExpr_id op				`thenTc` \ (op', lie1, op_ty) ->
    split_fun_ty op_ty 2 {- two args -}		`thenTc` \ ([arg1_ty, arg2_ty], op_res_ty) ->
    tcArg op (arg1, arg1_ty, 1)			`thenTc` \ (arg1',lie2a) ->
    tcArg op (arg2, arg2_ty, 2)			`thenTc` \ (arg2',lie2b) ->
    tcAddErrCtxt (exprCtxt in_expr)		$
    tcSub res_ty op_res_ty			`thenTc` \ (co_fn, lie3) ->
    returnTc (OpApp arg1' op' fix arg2', 
	      lie1 `plusLIE` lie2a `plusLIE` lie2b `plusLIE` lie3)
\end{code}

The interesting thing about @ccall@ is that it is just a template
which we instantiate by filling in details about the types of its
argument and result (ie minimal typechecking is performed).  So, the
basic story is that we allocate a load of type variables (to hold the
arg/result types); unify them with the args/result; and store them for
later use.

\begin{code}
tcMonoExpr e0@(HsCCall lbl args may_gc is_casm ignored_fake_result_ty) res_ty

  = getDOptsTc				`thenNF_Tc` \ dflags ->

    checkTc (not (is_casm && dopt_HscLang dflags /= HscC)) 
        (vcat [text "_casm_ is only supported when compiling via C (-fvia-C).",
               text "Either compile with -fvia-C, or, better, rewrite your code",
               text "to use the foreign function interface.  _casm_s are deprecated",
               text "and support for them may one day disappear."])
					`thenTc_`

    -- Get the callable and returnable classes.
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
    let tv_idxs | null args  = []
		| otherwise  = [1..length args]
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
    returnTc (HsCCall lbl args' may_gc is_casm io_result_ty,
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
tcMonoExpr in_expr@(ExplicitList _ exprs) res_ty	-- Non-empty list
  = unifyListTy res_ty                        `thenTc` \ elt_ty ->  
    mapAndUnzipTc (tc_elt elt_ty) exprs	      `thenTc` \ (exprs', lies) ->
    returnTc (ExplicitList elt_ty exprs', plusLIEs lies)
  where
    tc_elt elt_ty expr
      = tcAddErrCtxt (listCtxt expr) $
	tcMonoExpr expr elt_ty

tcMonoExpr in_expr@(ExplicitPArr _ exprs) res_ty	-- maybe empty
  = unifyPArrTy res_ty                        `thenTc` \ elt_ty ->  
    mapAndUnzipTc (tc_elt elt_ty) exprs	      `thenTc` \ (exprs', lies) ->
    returnTc (ExplicitPArr elt_ty exprs', plusLIEs lies)
  where
    tc_elt elt_ty expr
      = tcAddErrCtxt (parrCtxt expr) $
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
    returnTc (RecordUpdOut record_expr' record_ty result_record_ty (map instToId dicts) rbinds', 
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

tcMonoExpr in_expr@(PArrSeqIn seq@(FromTo expr1 expr2)) res_ty
  = tcAddErrCtxt (parrSeqCtxt in_expr) $
    unifyPArrTy  res_ty         			`thenTc`    \ elt_ty ->  
    tcMonoExpr expr1 elt_ty				`thenTc`    \ (expr1',lie1) ->
    tcMonoExpr expr2 elt_ty				`thenTc`    \ (expr2',lie2) ->
    tcLookupGlobalId enumFromToPName			`thenNF_Tc` \ sel_id ->
    newMethod (PArrSeqOrigin seq) sel_id [elt_ty]	`thenNF_Tc` \ enum_from_to ->

    returnTc (PArrSeqOut (HsVar (instToId enum_from_to))
			 (FromTo expr1' expr2'),
	      lie1 `plusLIE` lie2 `plusLIE` unitLIE enum_from_to)

tcMonoExpr in_expr@(PArrSeqIn seq@(FromThenTo expr1 expr2 expr3)) res_ty
  = tcAddErrCtxt  (parrSeqCtxt in_expr) $
    unifyPArrTy  res_ty         			`thenTc`    \ elt_ty ->  
    tcMonoExpr expr1 elt_ty				`thenTc`    \ (expr1',lie1) ->
    tcMonoExpr expr2 elt_ty				`thenTc`    \ (expr2',lie2) ->
    tcMonoExpr expr3 elt_ty				`thenTc`    \ (expr3',lie3) ->
    tcLookupGlobalId enumFromThenToPName		`thenNF_Tc` \ sel_id ->
    newMethod (PArrSeqOrigin seq) sel_id [elt_ty]	`thenNF_Tc` \ eft ->

    returnTc (PArrSeqOut (HsVar (instToId eft))
			 (FromThenTo expr1' expr2' expr3'),
	      lie1 `plusLIE` lie2 `plusLIE` lie3 `plusLIE` unitLIE eft)

tcMonoExpr (PArrSeqIn _) _ 
  = panic "TcExpr.tcMonoExpr: Infinite parallel array!"
    -- the parser shouldn't have generated it and the renamer shouldn't have
    -- let it through
\end{code}

%************************************************************************
%*									*
\subsection{Implicit Parameter bindings}
%*									*
%************************************************************************

\begin{code}
tcMonoExpr (HsWith expr binds) res_ty
  = tcMonoExpr expr res_ty			`thenTc` \ (expr', expr_lie) ->
    mapAndUnzip3Tc tcIPBind binds		`thenTc` \ (avail_ips, binds', bind_lies) ->

	-- If the binding binds ?x = E, we  must now 
	-- discharge any ?x constraints in expr_lie
    tcSimplifyIPs avail_ips expr_lie		`thenTc` \ (expr_lie', dict_binds) ->
    let
	expr'' = HsLet (mkMonoBind dict_binds [] Recursive) expr'
    in
    returnTc (HsWith expr'' binds', expr_lie' `plusLIE` plusLIEs bind_lies)

tcIPBind (ip, expr)
  = newTyVarTy openTypeKind		`thenTc` \ ty ->
    tcGetSrcLoc				`thenTc` \ loc ->
    newIPDict (IPBind ip) ip ty		`thenNF_Tc` \ (ip', ip_inst) ->
    tcMonoExpr expr ty			`thenTc` \ (expr', lie) ->
    returnTc (ip_inst, (ip', expr'), lie)
\end{code}

%************************************************************************
%*									*
\subsection{@tcApp@ typchecks an application}
%*									*
%************************************************************************

\begin{code}

tcApp :: RenamedHsExpr -> [RenamedHsExpr]   	-- Function and args
      -> TcType			    		-- Expected result type of application
      -> TcM (TcExpr, LIE)		    	-- Translated fun and args

tcApp (HsApp e1 e2) args res_ty 
  = tcApp e1 (e2:args) res_ty		-- Accumulate the arguments

tcApp fun args res_ty
  = 	-- First type-check the function
    tcExpr_id fun  				`thenTc` \ (fun', lie_fun, fun_ty) ->

    tcAddErrCtxt (wrongArgsCtxt "too many" fun args) (
	split_fun_ty fun_ty (length args)
    )						`thenTc` \ (expected_arg_tys, actual_result_ty) ->

	-- Now typecheck the args
    mapAndUnzipTc (tcArg fun)
	  (zip3 args expected_arg_tys [1..])	`thenTc` \ (args', lie_args_s) ->

	-- Unify with expected result after type-checking the args
	-- so that the info from args percolates to actual_result_ty.
	-- This is when we might detect a too-few args situation.
	-- (One can think of cases when the opposite order would give
	-- a better error message.)
    tcAddErrCtxtM (checkArgsCtxt fun args res_ty actual_result_ty)
		  (tcSub res_ty actual_result_ty)	`thenTc` \ (co_fn, lie_res) ->

    returnTc (co_fn <$> foldl HsApp fun' args', 
	      lie_res `plusLIE` lie_fun `plusLIE` plusLIEs lie_args_s)


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

      len_act_args     = length act_args
      len_exp_args     = length exp_args

      message | len_exp_args < len_act_args = wrongArgsCtxt "too few" fun args
              | len_exp_args > len_act_args = wrongArgsCtxt "too many" fun args
	      | otherwise		    = appCtxt fun args
    in
    returnNF_Tc (env2, message)


split_fun_ty :: TcType		-- The type of the function
	     -> Int		-- Number of arguments
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
tcArg :: RenamedHsExpr				-- The function (for error messages)
      -> (RenamedHsExpr, TcSigmaType, Int)	-- Actual argument and expected arg type
      -> TcM (TcExpr, LIE)			-- Resulting argument and LIE

tcArg the_fun (arg, expected_arg_ty, arg_no)
  = tcAddErrCtxt (funAppCtxt the_fun arg arg_no) $
    tcExpr arg expected_arg_ty
\end{code}


%************************************************************************
%*									*
\subsection{@tcId@ typchecks an identifier occurrence}
%*									*
%************************************************************************

tcId instantiates an occurrence of an Id.
The instantiate_it loop runs round instantiating the Id.
It has to be a loop because we are now prepared to entertain
types like
	f:: forall a. Eq a => forall b. Baz b => tau
We want to instantiate this to
	f2::tau		{f2 = f1 b (Baz b), f1 = f a (Eq a)}

The -fno-method-sharing flag controls what happens so far as the LIE
is concerned.  The default case is that for an overloaded function we 
generate a "method" Id, and add the Method Inst to the LIE.  So you get
something like
	f :: Num a => a -> a
	f = /\a (d:Num a) -> let m = (+) a d in \ (x:a) -> m x x
If you specify -fno-method-sharing, the dictionary application 
isn't shared, so we get
	f :: Num a => a -> a
	f = /\a (d:Num a) (x:a) -> (+) a d x x
This gets a bit less sharing, but
	a) it's better for RULEs involving overloaded functions
	b) perhaps fewer separated lambdas

\begin{code}
tcId :: Name -> NF_TcM (TcExpr, LIE, TcType)
tcId name	-- Look up the Id and instantiate its type
  = tcLookupId name			`thenNF_Tc` \ id ->
    loop (OccurrenceOf id) (HsVar id) emptyLIE (idType id)
  where
    loop orig (HsVar fun_id) lie fun_ty
	| want_method_inst fun_ty
	= tcInstType fun_ty			`thenNF_Tc` \ (tyvars, theta, tau) ->
	  newMethodWithGivenTy orig fun_id 
		(mkTyVarTys tyvars) theta tau	`thenNF_Tc` \ meth ->
	  loop orig (HsVar (instToId meth)) 
	       (unitLIE meth `plusLIE` lie) tau

    loop orig fun lie fun_ty 
	| isSigmaTy fun_ty
	= tcInstCall orig fun_ty	`thenNF_Tc` \ (inst_fn, inst_lie, tau) ->
	  loop orig (inst_fn fun) (inst_lie `plusLIE` lie) tau

	| otherwise
	= returnNF_Tc (fun, lie, fun_ty)

    want_method_inst fun_ty 
	| opt_NoMethodSharing = False	
	| otherwise	      = case tcSplitSigmaTy fun_ty of
				  (_,[],_)    -> False 	-- Not overloaded
				  (_,theta,_) -> not (any isLinearPred theta)
	-- This is a slight hack.
	-- If 	f :: (%x :: T) => Int -> Int
	-- Then if we have two separate calls, (f 3, f 4), we cannot
	-- make a method constraint that then gets shared, thus:
	--	let m = f %x in (m 3, m 4)
	-- because that loses the linearity of the constraint.
	-- The simplest thing to do is never to construct a method constraint
	-- in the first place that has a linear implicit parameter in it.
\end{code}

Typecheck expression which in most cases will be an Id.
The expression can return a higher-ranked type, such as
	(forall a. a->a) -> Int
so we must create a HoleTyVarTy to pass in as the expected tyvar.

\begin{code}
tcExpr_id :: RenamedHsExpr -> TcM (TcExpr, LIE, TcType)
tcExpr_id (HsVar name) = tcId name
tcExpr_id expr         = newHoleTyVarTy			`thenNF_Tc` \ id_ty ->
			 tcMonoExpr expr id_ty		`thenTc`    \ (expr', lie_id) ->
		         returnTc (expr', lie_id, id_ty) 
\end{code}


%************************************************************************
%*									*
\subsection{@tcDoStmts@ typechecks a {\em list} of do statements}
%*									*
%************************************************************************

\begin{code}
-- I don't like this lumping together of do expression and list/array
-- comprehensions; creating the monad instances is entirely pointless in the
-- latter case; I'll leave the list case as it is for the moment, but handle
-- arrays extra (would be better to handle arrays and lists together, though)
-- -=chak
--
tcDoStmts PArrComp stmts src_loc res_ty
  =
    ASSERT( not (null stmts) )
    tcAddSrcLoc src_loc	$

    unifyPArrTy res_ty			      `thenTc` \elt_ty              ->
    let tc_ty = mkTyConTy parrTyCon
	m_ty  = (mkPArrTy, elt_ty)
    in
    tcStmts (DoCtxt PArrComp) m_ty stmts      `thenTc` \(stmts', stmts_lie) ->
    returnTc (HsDoOut PArrComp stmts'
		      undefined undefined undefined  -- don't touch!
		      res_ty src_loc,
	      stmts_lie)

tcDoStmts do_or_lc stmts src_loc res_ty
  =	-- get the Monad and MonadZero classes
	-- create type consisting of a fresh monad tyvar
    ASSERT( not (null stmts) )
    tcAddSrcLoc src_loc	$

	-- If it's a comprehension we're dealing with, 
	-- force it to be a list comprehension.
	-- (as of Haskell 98, monad comprehensions are no more.)
	-- Similarily, array comprehensions must involve parallel arrays types
	--   -=chak
    (case do_or_lc of
       ListComp -> unifyListTy res_ty			`thenTc` \ elt_ty ->
		   returnNF_Tc (mkTyConTy listTyCon, (mkListTy, elt_ty))

       PArrComp -> panic "TcExpr.tcDoStmts: How did we get here?!?"

       _	-> newTyVarTy (mkArrowKind liftedTypeKind liftedTypeKind)	`thenNF_Tc` \ m_ty ->
		   newTyVarTy liftedTypeKind 					`thenNF_Tc` \ elt_ty ->
		   unifyTauTy res_ty (mkAppTy m_ty elt_ty)			`thenTc_`
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

	tcExpr rhs field_ty 			`thenTc` \ (rhs', lie) ->

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
	  		  (dropList ex_theta (dataConStrictMarks data_con))
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

parrSeqCtxt expr
  = hang (ptext SLIT("In a parallel array sequence:")) 4 (ppr expr)

caseCtxt expr
  = hang (ptext SLIT("In the case expression:")) 4 (ppr expr)

caseScrutCtxt expr
  = hang (ptext SLIT("In the scrutinee of a case expression:")) 4 (ppr expr)

exprSigCtxt expr
  = hang (ptext SLIT("When checking the type signature of the expression:"))
	 4 (ppr expr)

listCtxt expr
  = hang (ptext SLIT("In the list element:")) 4 (ppr expr)

parrCtxt expr
  = hang (ptext SLIT("In the parallel array element:")) 4 (ppr expr)

predCtxt expr
  = hang (ptext SLIT("In the predicate expression:")) 4 (ppr expr)

exprCtxt expr
  = hang (ptext SLIT("In the expression:")) 4 (ppr expr)

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
