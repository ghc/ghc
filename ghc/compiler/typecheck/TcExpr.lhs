%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcExpr]{Typecheck an expression}

\begin{code}
module TcExpr ( tcExpr, tcExpr_id, tcMonoExpr ) where

#include "HsVersions.h"

#ifdef GHCI 	/* Only if bootstrapped */
import {-# SOURCE #-}	TcSplice( tcSpliceExpr, tcBracket )
import HsSyn		( HsReify(..), ReifyFlavour(..) )
import TcType		( isTauTy )
import TcEnv		( bracketOK, tcMetaTy, tcLookupGlobal,
			  wellStaged, metaLevel )
import TcSimplify	( tcSimplifyBracket )
import Name		( isExternalName )
import qualified DsMeta
#endif

import HsSyn		( HsExpr(..), HsLit(..), ArithSeqInfo(..),
			  mkMonoBind, recBindFields
			)
import RnHsSyn		( RenamedHsExpr, RenamedRecordBinds )
import TcHsSyn		( TcExpr, TcRecordBinds, hsLitType, mkHsDictApp, mkHsTyApp, mkHsLet )
import TcRnMonad
import TcUnify		( tcSubExp, tcGen, (<$>),
			  unifyTauTy, unifyFunTy, unifyListTy, unifyPArrTy,
			  unifyTupleTy )
import BasicTypes	( RecFlag(..),  isMarkedStrict )
import Inst		( InstOrigin(..), 
			  newOverloadedLit, newMethodFromName, newIPDict,
			  newDicts, newMethodWithGivenTy, 
			  instToId, tcInstCall, tcInstDataCon
			)
import TcBinds		( tcBindsAndThen )
import TcEnv		( tcLookupClass, tcLookupGlobal_maybe, tcLookupIdLvl,
			  tcLookupTyCon, tcLookupDataCon, tcLookupId
			)
import TcMatches	( tcMatchesCase, tcMatchLambda, tcDoStmts )
import TcMonoType	( tcHsSigType, UserTypeCtxt(..) )
import TcPat		( badFieldCon )
import TcSimplify	( tcSimplifyIPs )
import TcMType		( tcInstTyVars, tcInstType, newHoleTyVarTy, zapToType,
			  newTyVarTy, newTyVarTys, zonkTcType, readHoleResult )
import TcType		( TcType, TcSigmaType, TcRhoType, TyVarDetails(VanillaTv),
			  tcSplitFunTys, tcSplitTyConApp, mkTyVarTys,
			  isSigmaTy, mkFunTy, mkFunTys,
			  mkTyConApp, mkClassPred, tcFunArgTy,
			  tyVarsOfTypes, isLinearPred,
			  liftedTypeKind, openTypeKind, 
			  tcSplitSigmaTy, tcTyConAppTyCon,
			  tidyOpenType
			)
import FieldLabel	( FieldLabel, fieldLabelName, fieldLabelType, fieldLabelTyCon )
import Id		( Id, idType, recordSelectorFieldLabel, isRecordSelector, isDataConWrapId_maybe )
import DataCon		( DataCon, dataConFieldLabels, dataConSig, dataConStrictMarks )
import Name		( Name )
import TyCon		( TyCon, tyConTyVars, tyConTheta, isAlgTyCon, tyConDataCons )
import Subst		( mkTopTyVarSubst, substTheta, substTy )
import VarSet		( emptyVarSet, elemVarSet )
import TysWiredIn	( boolTy )
import PrelNames	( cCallableClassName, cReturnableClassName, 
			  enumFromName, enumFromThenName, 
			  enumFromToName, enumFromThenToName,
			  enumFromToPName, enumFromThenToPName,
			  ioTyConName
			)
import ListSetOps	( minusList )
import CmdLineOpts
import HscTypes		( TyThing(..) )

import Util
import Outputable
import FastString
\end{code}

%************************************************************************
%*									*
\subsection{Main wrappers}
%*									*
%************************************************************************

\begin{code}
tcExpr :: RenamedHsExpr		-- Expession to type check
	-> TcSigmaType 		-- Expected type (could be a polytpye)
	-> TcM TcExpr		-- Generalised expr with expected type

tcExpr expr expected_ty 
  = traceTc (text "tcExpr" <+> (ppr expected_ty $$ ppr expr)) `thenM_`
    tc_expr' expr expected_ty

tc_expr' expr expected_ty
  | not (isSigmaTy expected_ty)  -- Monomorphic case
  = tcMonoExpr expr expected_ty

  | otherwise
  = tcGen expected_ty emptyVarSet (
	tcMonoExpr expr
    )				`thenM` \ (gen_fn, expr') ->
    returnM (gen_fn <$> expr')
\end{code}


%************************************************************************
%*									*
\subsection{The TAUT rules for variables}
%*									*
%************************************************************************

\begin{code}
tcMonoExpr :: RenamedHsExpr		-- Expession to type check
	   -> TcRhoType 		-- Expected type (could be a type variable)
					-- Definitely no foralls at the top
					-- Can be a 'hole'.
	   -> TcM TcExpr

tcMonoExpr (HsVar name) res_ty
  = tcId name			`thenM` \ (expr', id_ty) ->
    tcSubExp res_ty id_ty 	`thenM` \ co_fn ->
    returnM (co_fn <$> expr')

tcMonoExpr (HsIPVar ip) res_ty
  = 	-- Implicit parameters must have a *tau-type* not a 
	-- type scheme.  We enforce this by creating a fresh
	-- type variable as its type.  (Because res_ty may not
	-- be a tau-type.)
    newTyVarTy openTypeKind		`thenM` \ ip_ty ->
    newIPDict (IPOcc ip) ip ip_ty 	`thenM` \ (ip', inst) ->
    extendLIE inst			`thenM_`
    tcSubExp res_ty ip_ty		`thenM` \ co_fn ->
    returnM (co_fn <$> HsIPVar ip')
\end{code}


%************************************************************************
%*									*
\subsection{Expressions type signatures}
%*									*
%************************************************************************

\begin{code}
tcMonoExpr in_expr@(ExprWithTySig expr poly_ty) res_ty
 = addErrCtxt (exprSigCtxt in_expr)	$
   tcHsSigType ExprSigCtxt poly_ty	`thenM` \ sig_tc_ty ->
   tcExpr expr sig_tc_ty		`thenM` \ expr' ->

	-- Must instantiate the outer for-alls of sig_tc_ty
	-- else we risk instantiating a ? res_ty to a forall-type
	-- which breaks the invariant that tcMonoExpr only returns phi-types
   tcInstCall SignatureOrigin sig_tc_ty	`thenM` \ (inst_fn, inst_sig_ty) ->
   tcSubExp res_ty inst_sig_ty		`thenM` \ co_fn ->

   returnM (co_fn <$> inst_fn expr')

tcMonoExpr (HsType ty) res_ty
  = failWithTc (text "Can't handle type argument:" <+> ppr ty)
	-- This is the syntax for type applications that I was planning
	-- but there are difficulties (e.g. what order for type args)
	-- so it's not enabled yet.
	-- Can't eliminate it altogether from the parser, because the
	-- same parser parses *patterns*.
\end{code}


%************************************************************************
%*									*
\subsection{Other expression forms}
%*									*
%************************************************************************

\begin{code}
tcMonoExpr (HsLit lit)     res_ty  = tcLit lit res_ty
tcMonoExpr (HsOverLit lit) res_ty  = newOverloadedLit (LiteralOrigin lit) lit res_ty
tcMonoExpr (HsPar expr)    res_ty  = tcMonoExpr expr res_ty	`thenM` \ expr' -> 
				     returnM (HsPar expr')
tcMonoExpr (HsSCC lbl expr) res_ty = tcMonoExpr expr res_ty	`thenM` \ expr' ->
				     returnM (HsSCC lbl expr')


tcMonoExpr (NegApp expr neg_name) res_ty
  = tcMonoExpr (HsApp (HsVar neg_name) expr) res_ty
	-- ToDo: use tcSyntaxName

tcMonoExpr (HsLam match) res_ty
  = tcMatchLambda match res_ty 		`thenM` \ match' ->
    returnM (HsLam match')

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
  = tcExpr_id op				`thenM` \ (op', op_ty) ->
    split_fun_ty op_ty 2 {- two args -}		`thenM` \ ([arg1_ty, arg2_ty], op_res_ty) ->
    tcArg op (arg1, arg1_ty, 1)			`thenM` \ arg1' ->
    addErrCtxt (exprCtxt in_expr)		$
    tcSubExp res_ty (mkFunTy arg2_ty op_res_ty)	`thenM` \ co_fn ->
    returnM (co_fn <$> SectionL arg1' op')

-- Right sections, equivalent to \ x -> x op expr, or
--	\ x -> op x expr

tcMonoExpr in_expr@(SectionR op arg2) res_ty
  = tcExpr_id op				`thenM` \ (op', op_ty) ->
    split_fun_ty op_ty 2 {- two args -}		`thenM` \ ([arg1_ty, arg2_ty], op_res_ty) ->
    tcArg op (arg2, arg2_ty, 2)			`thenM` \ arg2' ->
    addErrCtxt (exprCtxt in_expr)		$
    tcSubExp res_ty (mkFunTy arg1_ty op_res_ty)	`thenM` \ co_fn ->
    returnM (co_fn <$> SectionR op' arg2')

-- equivalent to (op e1) e2:

tcMonoExpr in_expr@(OpApp arg1 op fix arg2) res_ty
  = tcExpr_id op				`thenM` \ (op', op_ty) ->
    split_fun_ty op_ty 2 {- two args -}		`thenM` \ ([arg1_ty, arg2_ty], op_res_ty) ->
    tcArg op (arg1, arg1_ty, 1)			`thenM` \ arg1' ->
    tcArg op (arg2, arg2_ty, 2)			`thenM` \ arg2' ->
    addErrCtxt (exprCtxt in_expr)		$
    tcSubExp res_ty op_res_ty			`thenM` \ co_fn ->
    returnM (OpApp arg1' op' fix arg2')
\end{code}

\begin{code}
tcMonoExpr (HsLet binds expr) res_ty
  = tcBindsAndThen
	combiner
	binds 			-- Bindings to check
	(tcMonoExpr expr res_ty)
  where
    combiner is_rec bind expr = HsLet (mkMonoBind bind [] is_rec) expr

tcMonoExpr in_expr@(HsCase scrut matches src_loc) res_ty
  = addSrcLoc src_loc			$
    addErrCtxt (caseCtxt in_expr)	$

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

    tcMatchesCase matches res_ty	`thenM`    \ (scrut_ty, matches') ->

    addErrCtxt (caseScrutCtxt scrut)	(
      tcMonoExpr scrut scrut_ty
    )					`thenM`    \ scrut' ->

    returnM (HsCase scrut' matches' src_loc)

tcMonoExpr (HsIf pred b1 b2 src_loc) res_ty
  = addSrcLoc src_loc	$
    addErrCtxt (predCtxt pred) (
    tcMonoExpr pred boolTy	)	`thenM`    \ pred' ->

    zapToType res_ty			`thenM`    \ res_ty' ->
	-- C.f. the call to zapToType in TcMatches.tcMatches

    tcMonoExpr b1 res_ty'		`thenM`    \ b1' ->
    tcMonoExpr b2 res_ty'		`thenM`    \ b2' ->
    returnM (HsIf pred' b1' b2' src_loc)

tcMonoExpr (HsDo do_or_lc stmts method_names _ src_loc) res_ty
  = addSrcLoc src_loc		$
    tcDoStmts do_or_lc stmts method_names res_ty	`thenM` \ (binds, stmts', methods') ->
    returnM (mkHsLet binds (HsDo do_or_lc stmts' methods' res_ty src_loc))

tcMonoExpr in_expr@(ExplicitList _ exprs) res_ty	-- Non-empty list
  = unifyListTy res_ty                `thenM` \ elt_ty ->  
    mappM (tc_elt elt_ty) exprs	      `thenM` \ exprs' ->
    returnM (ExplicitList elt_ty exprs')
  where
    tc_elt elt_ty expr
      = addErrCtxt (listCtxt expr) $
	tcMonoExpr expr elt_ty

tcMonoExpr in_expr@(ExplicitPArr _ exprs) res_ty	-- maybe empty
  = unifyPArrTy res_ty                `thenM` \ elt_ty ->  
    mappM (tc_elt elt_ty) exprs	      `thenM` \ exprs' ->
    returnM (ExplicitPArr elt_ty exprs')
  where
    tc_elt elt_ty expr
      = addErrCtxt (parrCtxt expr) $
	tcMonoExpr expr elt_ty

tcMonoExpr (ExplicitTuple exprs boxity) res_ty
  = unifyTupleTy boxity (length exprs) res_ty	`thenM` \ arg_tys ->
    tcMonoExprs exprs arg_tys 			`thenM` \ exprs' ->
    returnM (ExplicitTuple exprs' boxity)
\end{code}


%************************************************************************
%*									*
		Foreign calls
%*									*
%************************************************************************

The interesting thing about @ccall@ is that it is just a template
which we instantiate by filling in details about the types of its
argument and result (ie minimal typechecking is performed).  So, the
basic story is that we allocate a load of type variables (to hold the
arg/result types); unify them with the args/result; and store them for
later use.

\begin{code}
tcMonoExpr e0@(HsCCall lbl args may_gc is_casm ignored_fake_result_ty) res_ty

  = getDOpts				`thenM` \ dflags ->

    checkTc (not (is_casm && dopt_HscLang dflags /= HscC)) 
        (vcat [text "_casm_ is only supported when compiling via C (-fvia-C).",
               text "Either compile with -fvia-C, or, better, rewrite your code",
               text "to use the foreign function interface.  _casm_s are deprecated",
               text "and support for them may one day disappear."])
					`thenM_`

    -- Get the callable and returnable classes.
    tcLookupClass cCallableClassName	`thenM` \ cCallableClass ->
    tcLookupClass cReturnableClassName	`thenM` \ cReturnableClass ->
    tcLookupTyCon ioTyConName		`thenM` \ ioTyCon ->
    let
	new_arg_dict (arg, arg_ty)
	  = newDicts (CCallOrigin (unpackFS lbl) (Just arg))
		     [mkClassPred cCallableClass [arg_ty]]	`thenM` \ arg_dicts ->
	    returnM arg_dicts	-- Actually a singleton bag

	result_origin = CCallOrigin (unpackFS lbl) Nothing {- Not an arg -}
    in

	-- Arguments
    let tv_idxs | null args  = []
		| otherwise  = [1..length args]
    in
    newTyVarTys (length tv_idxs) openTypeKind		`thenM` \ arg_tys ->
    tcMonoExprs args arg_tys		   		`thenM` \ args' ->

	-- The argument types can be unlifted or lifted; the result
	-- type must, however, be lifted since it's an argument to the IO
	-- type constructor.
    newTyVarTy liftedTypeKind  		`thenM` \ result_ty ->
    let
	io_result_ty = mkTyConApp ioTyCon [result_ty]
    in
    unifyTauTy res_ty io_result_ty		`thenM_`

	-- Construct the extra insts, which encode the
	-- constraints on the argument and result types.
    mappM new_arg_dict (zipEqual "tcMonoExpr:CCall" args arg_tys)	`thenM` \ ccarg_dicts_s ->
    newDicts result_origin [mkClassPred cReturnableClass [result_ty]]	`thenM` \ ccres_dict ->
    extendLIEs (ccres_dict ++ concat ccarg_dicts_s)			`thenM_`
    returnM (HsCCall lbl args' may_gc is_casm io_result_ty)
\end{code}


%************************************************************************
%*									*
		Record construction and update
%*									*
%************************************************************************

\begin{code}
tcMonoExpr expr@(RecordCon con_name rbinds) res_ty
  = addErrCtxt (recordConCtxt expr)		$
    tcId con_name			`thenM` \ (con_expr, con_tau) ->
    let
	(_, record_ty)   = tcSplitFunTys con_tau
	(tycon, ty_args) = tcSplitTyConApp record_ty
    in
    ASSERT( isAlgTyCon tycon )
    unifyTauTy res_ty record_ty          `thenM_`

	-- Check that the record bindings match the constructor
	-- con_name is syntactically constrained to be a data constructor
    tcLookupDataCon con_name	`thenM` \ data_con ->
    let
	bad_fields = badFields rbinds data_con
    in
    if notNull bad_fields then
	mappM (addErrTc . badFieldCon data_con) bad_fields	`thenM_`
	failM	-- Fail now, because tcRecordBinds will crash on a bad field
    else

	-- Typecheck the record bindings
    tcRecordBinds tycon ty_args rbinds		`thenM` \ rbinds' ->
    
 	-- Check for missing fields
    checkMissingFields data_con rbinds		`thenM_` 

    returnM (RecordConOut data_con con_expr rbinds')

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
  = addErrCtxt (recordUpdCtxt	expr)		$

	-- STEP 0
	-- Check that the field names are really field names
    ASSERT( notNull rbinds )
    let 
	field_names = recBindFields rbinds
    in
    mappM tcLookupGlobal_maybe field_names		`thenM` \ maybe_sel_ids ->
    let
	bad_guys = [ addErrTc (notSelector field_name) 
		   | (field_name, maybe_sel_id) <- field_names `zip` maybe_sel_ids,
		      case maybe_sel_id of
			Just (AnId sel_id) -> not (isRecordSelector sel_id)
			other		   -> True
		   ]
    in
    checkM (null bad_guys) (sequenceM bad_guys `thenM_` failM)	`thenM_`
    
	-- STEP 1
	-- Figure out the tycon and data cons from the first field name
    let
		-- It's OK to use the non-tc splitters here (for a selector)
	(Just (AnId sel_id) : _) = maybe_sel_ids

	(_, _, tau)  = tcSplitSigmaTy (idType sel_id)	-- Selectors can be overloaded
		     					-- when the data type has a context
	data_ty	     = tcFunArgTy tau			-- Must succeed since sel_id is a selector
	tycon	     = tcTyConAppTyCon data_ty
	data_cons    = tyConDataCons tycon
	tycon_tyvars = tyConTyVars tycon		-- The data cons use the same type vars
    in
    tcInstTyVars VanillaTv tycon_tyvars		`thenM` \ (_, result_inst_tys, inst_env) ->

	-- STEP 2
	-- Check that at least one constructor has all the named fields
	-- i.e. has an empty set of bad fields returned by badFields
    checkTc (any (null . badFields rbinds) data_cons)
	    (badFieldsUpd rbinds)		`thenM_`

	-- STEP 3
	-- Typecheck the update bindings.
	-- (Do this after checking for bad fields in case there's a field that
	--  doesn't match the constructor.)
    let
	result_record_ty = mkTyConApp tycon result_inst_tys
    in
    unifyTauTy res_ty result_record_ty          `thenM_`
    tcRecordBinds tycon result_inst_tys rbinds	`thenM` \ rbinds' ->

	-- STEP 4
	-- Use the un-updated fields to find a vector of booleans saying
	-- which type arguments must be the same in updatee and result.
	--
	-- WARNING: this code assumes that all data_cons in a common tycon
	-- have FieldLabels abstracted over the same tyvars.
    let
	upd_field_lbls      = map recordSelectorFieldLabel (recBindFields rbinds')
	con_field_lbls_s    = map dataConFieldLabels data_cons

		-- A constructor is only relevant to this process if
		-- it contains all the fields that are being updated
	relevant_field_lbls_s      = filter is_relevant con_field_lbls_s
	is_relevant con_field_lbls = all (`elem` con_field_lbls) upd_field_lbls

	non_upd_field_lbls  = concat relevant_field_lbls_s `minusList` upd_field_lbls
	common_tyvars       = tyVarsOfTypes (map fieldLabelType non_upd_field_lbls)

	mk_inst_ty (tyvar, result_inst_ty) 
	  | tyvar `elemVarSet` common_tyvars = returnM result_inst_ty	-- Same as result type
	  | otherwise			     = newTyVarTy liftedTypeKind	-- Fresh type
    in
    mappM mk_inst_ty (zip tycon_tyvars result_inst_tys)	`thenM` \ inst_tys ->

	-- STEP 5
	-- Typecheck the expression to be updated
    let
	record_ty = mkTyConApp tycon inst_tys
    in
    tcMonoExpr record_expr record_ty		`thenM` \ record_expr' ->

	-- STEP 6
	-- Figure out the LIE we need.  We have to generate some 
	-- dictionaries for the data type context, since we are going to
	-- do pattern matching over the data cons.
	--
	-- What dictionaries do we need?  
	-- We just take the context of the type constructor
    let
	theta' = substTheta inst_env (tyConTheta tycon)
    in
    newDicts RecordUpdOrigin theta'	`thenM` \ dicts ->
    extendLIEs dicts			`thenM_`

	-- Phew!
    returnM (RecordUpdOut record_expr' record_ty result_record_ty rbinds') 
\end{code}


%************************************************************************
%*									*
	Arithmetic sequences			e.g. [a,b..]
	and their parallel-array counterparts	e.g. [: a,b.. :]
		
%*									*
%************************************************************************

\begin{code}
tcMonoExpr (ArithSeqIn seq@(From expr)) res_ty
  = unifyListTy res_ty 				`thenM` \ elt_ty ->  
    tcMonoExpr expr elt_ty		 	`thenM` \ expr' ->

    newMethodFromName (ArithSeqOrigin seq) 
		      elt_ty enumFromName	`thenM` \ enum_from ->

    returnM (ArithSeqOut (HsVar enum_from) (From expr'))

tcMonoExpr in_expr@(ArithSeqIn seq@(FromThen expr1 expr2)) res_ty
  = addErrCtxt (arithSeqCtxt in_expr) $ 
    unifyListTy  res_ty         			`thenM`    \ elt_ty ->  
    tcMonoExpr expr1 elt_ty				`thenM`    \ expr1' ->
    tcMonoExpr expr2 elt_ty				`thenM`    \ expr2' ->
    newMethodFromName (ArithSeqOrigin seq) 
		      elt_ty enumFromThenName		`thenM` \ enum_from_then ->

    returnM (ArithSeqOut (HsVar enum_from_then) (FromThen expr1' expr2'))


tcMonoExpr in_expr@(ArithSeqIn seq@(FromTo expr1 expr2)) res_ty
  = addErrCtxt (arithSeqCtxt in_expr) $
    unifyListTy  res_ty         			`thenM`    \ elt_ty ->  
    tcMonoExpr expr1 elt_ty				`thenM`    \ expr1' ->
    tcMonoExpr expr2 elt_ty				`thenM`    \ expr2' ->
    newMethodFromName (ArithSeqOrigin seq) 
	  	      elt_ty enumFromToName		`thenM` \ enum_from_to ->

    returnM (ArithSeqOut (HsVar enum_from_to) (FromTo expr1' expr2'))

tcMonoExpr in_expr@(ArithSeqIn seq@(FromThenTo expr1 expr2 expr3)) res_ty
  = addErrCtxt  (arithSeqCtxt in_expr) $
    unifyListTy  res_ty         			`thenM`    \ elt_ty ->  
    tcMonoExpr expr1 elt_ty				`thenM`    \ expr1' ->
    tcMonoExpr expr2 elt_ty				`thenM`    \ expr2' ->
    tcMonoExpr expr3 elt_ty				`thenM`    \ expr3' ->
    newMethodFromName (ArithSeqOrigin seq) 
		      elt_ty enumFromThenToName		`thenM` \ eft ->

    returnM (ArithSeqOut (HsVar eft) (FromThenTo expr1' expr2' expr3'))

tcMonoExpr in_expr@(PArrSeqIn seq@(FromTo expr1 expr2)) res_ty
  = addErrCtxt (parrSeqCtxt in_expr) $
    unifyPArrTy  res_ty         			`thenM`    \ elt_ty ->  
    tcMonoExpr expr1 elt_ty				`thenM`    \ expr1' ->
    tcMonoExpr expr2 elt_ty				`thenM`    \ expr2' ->
    newMethodFromName (PArrSeqOrigin seq) 
		      elt_ty enumFromToPName 		`thenM` \ enum_from_to ->

    returnM (PArrSeqOut (HsVar enum_from_to) (FromTo expr1' expr2'))

tcMonoExpr in_expr@(PArrSeqIn seq@(FromThenTo expr1 expr2 expr3)) res_ty
  = addErrCtxt  (parrSeqCtxt in_expr) $
    unifyPArrTy  res_ty         			`thenM`    \ elt_ty ->  
    tcMonoExpr expr1 elt_ty				`thenM`    \ expr1' ->
    tcMonoExpr expr2 elt_ty				`thenM`    \ expr2' ->
    tcMonoExpr expr3 elt_ty				`thenM`    \ expr3' ->
    newMethodFromName (PArrSeqOrigin seq)
		      elt_ty enumFromThenToPName	`thenM` \ eft ->

    returnM (PArrSeqOut (HsVar eft) (FromThenTo expr1' expr2' expr3'))

tcMonoExpr (PArrSeqIn _) _ 
  = panic "TcExpr.tcMonoExpr: Infinite parallel array!"
    -- the parser shouldn't have generated it and the renamer shouldn't have
    -- let it through
\end{code}


%************************************************************************
%*									*
		Template Haskell
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI	/* Only if bootstrapped */
	-- Rename excludes these cases otherwise

tcMonoExpr (HsSplice n expr loc) res_ty = addSrcLoc loc (tcSpliceExpr n expr res_ty)
  
tcMonoExpr (HsBracket brack loc) res_ty
  = addSrcLoc loc			$
    getStage 				`thenM` \ level ->
    case bracketOK level of {
	Nothing         -> failWithTc (illegalBracket level) ;
	Just next_level ->

   	-- Typecheck expr to make sure it is valid,
	-- but throw away the results.  We'll type check
	-- it again when we actually use it.
    newMutVar []	 		`thenM` \ pending_splices ->
    getLIEVar				`thenM` \ lie_var ->

    setStage (Brack next_level pending_splices lie_var) (
	getLIE (tcBracket brack)
    )					`thenM` \ (meta_ty, lie) ->
    tcSimplifyBracket lie 		`thenM_`  

    unifyTauTy res_ty meta_ty		`thenM_`

	-- Return the original expression, not the type-decorated one
    readMutVar pending_splices		`thenM` \ pendings ->
    returnM (HsBracketOut brack pendings)
    }

tcMonoExpr (HsReify (Reify flavour name)) res_ty
  = addErrCtxt (ptext SLIT("At the reification of") <+> ppr name)	$
    tcMetaTy  tycon_name	`thenM` \ reify_ty ->
    unifyTauTy res_ty reify_ty	`thenM_`
    returnM (HsReify (ReifyOut flavour name))
  where
    tycon_name = case flavour of
		   ReifyDecl -> DsMeta.decTyConName
		   ReifyType -> DsMeta.typTyConName
		   ReifyFixity -> pprPanic "tcMonoExpr: cant do reifyFixity yet" (ppr name)
#endif GHCI
\end{code}

%************************************************************************
%*									*
\subsection{Implicit Parameter bindings}
%*									*
%************************************************************************

\begin{code}
tcMonoExpr (HsWith expr binds is_with) res_ty
  = getLIE (tcMonoExpr expr res_ty)	`thenM` \ (expr', expr_lie) ->
    mapAndUnzipM tc_ip_bind binds	`thenM` \ (avail_ips, binds') ->

	-- If the binding binds ?x = E, we  must now 
	-- discharge any ?x constraints in expr_lie
    tcSimplifyIPs avail_ips expr_lie	`thenM` \ dict_binds ->
    let
	expr'' = HsLet (mkMonoBind dict_binds [] Recursive) expr'
    in
    returnM (HsWith expr'' binds' is_with)
  where
    tc_ip_bind (ip, expr)
      = newTyVarTy openTypeKind		`thenM` \ ty ->
  	getSrcLocM			`thenM` \ loc ->
  	newIPDict (IPBind ip) ip ty	`thenM` \ (ip', ip_inst) ->
  	tcMonoExpr expr ty		`thenM` \ expr' ->
  	returnM (ip_inst, (ip', expr'))
\end{code}


%************************************************************************
%*									*
		Catch-all
%*									*
%************************************************************************

\begin{code}
tcMonoExpr other _ = pprPanic "tcMonoExpr" (ppr other)
\end{code}


%************************************************************************
%*									*
\subsection{@tcApp@ typchecks an application}
%*									*
%************************************************************************

\begin{code}

tcApp :: RenamedHsExpr -> [RenamedHsExpr]   	-- Function and args
      -> TcType			    		-- Expected result type of application
      -> TcM TcExpr			    	-- Translated fun and args

tcApp (HsApp e1 e2) args res_ty 
  = tcApp e1 (e2:args) res_ty		-- Accumulate the arguments

tcApp fun args res_ty
  = 	-- First type-check the function
    tcExpr_id fun  				`thenM` \ (fun', fun_ty) ->

    addErrCtxt (wrongArgsCtxt "too many" fun args) (
	traceTc (text "tcApp" <+> (ppr fun $$ ppr fun_ty)) 	`thenM_`
	split_fun_ty fun_ty (length args)
    )						`thenM` \ (expected_arg_tys, actual_result_ty) ->

	-- Now typecheck the args
    mappM (tcArg fun)
	  (zip3 args expected_arg_tys [1..])	`thenM` \ args' ->

	-- Unify with expected result after type-checking the args
	-- so that the info from args percolates to actual_result_ty.
	-- This is when we might detect a too-few args situation.
	-- (One can think of cases when the opposite order would give
	-- a better error message.)
    addErrCtxtM (checkArgsCtxt fun args res_ty actual_result_ty)
		  (tcSubExp res_ty actual_result_ty)	`thenM` \ co_fn ->

    returnM (co_fn <$> foldl HsApp fun' args') 


-- If an error happens we try to figure out whether the
-- function has been given too many or too few arguments,
-- and say so
checkArgsCtxt fun args expected_res_ty actual_res_ty tidy_env
  = zonkTcType expected_res_ty	  `thenM` \ exp_ty' ->
    zonkTcType actual_res_ty	  `thenM` \ act_ty' ->
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
    returnM (env2, message)


split_fun_ty :: TcType		-- The type of the function
	     -> Int		-- Number of arguments
	     -> TcM ([TcType],	-- Function argument types
		     TcType)	-- Function result types

split_fun_ty fun_ty 0 
  = returnM ([], fun_ty)

split_fun_ty fun_ty n
  = 	-- Expect the function to have type A->B
    unifyFunTy fun_ty		`thenM` \ (arg_ty, res_ty) ->
    split_fun_ty res_ty (n-1)	`thenM` \ (arg_tys, final_res_ty) ->
    returnM (arg_ty:arg_tys, final_res_ty)
\end{code}

\begin{code}
tcArg :: RenamedHsExpr				-- The function (for error messages)
      -> (RenamedHsExpr, TcSigmaType, Int)	-- Actual argument and expected arg type
      -> TcM TcExpr				-- Resulting argument and LIE

tcArg the_fun (arg, expected_arg_ty, arg_no)
  = addErrCtxt (funAppCtxt the_fun arg arg_no) $
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
tcId :: Name -> TcM (TcExpr, TcType)
tcId name	-- Look up the Id and instantiate its type
  = tcLookupIdLvl name			`thenM` \ (id, bind_lvl) ->

	-- Check for cross-stage lifting
#ifdef GHCI
    getStage				`thenM` \ use_stage -> 
    case use_stage of
      Brack use_lvl ps_var lie_var
	| use_lvl > bind_lvl && not (isExternalName name)
	-> 	-- E.g. \x -> [| h x |]
			-- We must behave as if the reference to x was
			--	h $(lift x)	
			-- We use 'x' itself as the splice proxy, used by 
			-- the desugarer to stitch it all back together
			-- NB: isExernalName is true of top level things, 
			-- and false of nested bindings
	
	let
	    id_ty = idType id
	in
	checkTc (isTauTy id_ty)	(polySpliceErr id)	`thenM_` 
		    -- If x is polymorphic, its occurrence sites might
		    -- have different instantiations, so we can't use plain
		    -- 'x' as the splice proxy name.  I don't know how to 
		    -- solve this, and it's probably unimportant, so I'm
		    -- just going to flag an error for now

	setLIEVar lie_var	(
	newMethodFromName orig id_ty DsMeta.liftName	`thenM` \ lift ->
		-- Put the 'lift' constraint into the right LIE
	
	-- Update the pending splices
        readMutVar ps_var			`thenM` \ ps ->
        writeMutVar ps_var ((name, HsApp (HsVar lift) (HsVar id)) : ps)	`thenM_`

	returnM (HsVar id, id_ty))

      other -> 
	let
	   use_lvl = metaLevel use_stage
	in
	checkTc (wellStaged bind_lvl use_lvl)
		(badStageErr id bind_lvl use_lvl)	`thenM_`
#endif
	-- This is the bit that handles the no-Template-Haskell case
	case isDataConWrapId_maybe id of
		Nothing       -> loop (HsVar id) (idType id)
		Just data_con -> inst_data_con id data_con

  where
    orig = OccurrenceOf name

    loop (HsVar fun_id) fun_ty
	| want_method_inst fun_ty
	= tcInstType VanillaTv fun_ty		`thenM` \ (tyvars, theta, tau) ->
	  newMethodWithGivenTy orig fun_id 
		(mkTyVarTys tyvars) theta tau	`thenM` \ meth_id ->
	  loop (HsVar meth_id) tau

    loop fun fun_ty 
	| isSigmaTy fun_ty
	= tcInstCall orig fun_ty	`thenM` \ (inst_fn, tau) ->
	  loop (inst_fn fun) tau

	| otherwise
	= returnM (fun, fun_ty)

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

	-- We treat data constructors differently, because we have to generate
	-- constraints for their silly theta, which no longer appears in
	-- the type of dataConWrapId.  It's dual to TcPat.tcConstructor
    inst_data_con id data_con
      = tcInstDataCon orig data_con	`thenM` \ (ty_args, ex_dicts, arg_tys, result_ty, _) ->
	extendLIEs ex_dicts		`thenM_`
	returnM (mkHsDictApp (mkHsTyApp (HsVar id) ty_args) (map instToId ex_dicts), 
		 mkFunTys arg_tys result_ty)
\end{code}

Typecheck expression which in most cases will be an Id.
The expression can return a higher-ranked type, such as
	(forall a. a->a) -> Int
so we must create a HoleTyVarTy to pass in as the expected tyvar.

\begin{code}
tcExpr_id :: RenamedHsExpr -> TcM (TcExpr, TcType)
tcExpr_id (HsVar name) = tcId name
tcExpr_id expr         = newHoleTyVarTy			`thenM` \ id_ty ->
			 tcMonoExpr expr id_ty		`thenM` \ expr' ->
			 readHoleResult id_ty		`thenM` \ id_ty' ->
		         returnM (expr', id_ty') 
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
	-> TcM TcRecordBinds

tcRecordBinds tycon ty_args rbinds
  = mappM do_bind rbinds
  where
    tenv = mkTopTyVarSubst (tyConTyVars tycon) ty_args

    do_bind (field_lbl_name, rhs)
      = addErrCtxt (fieldCtxt field_lbl_name)	$
           tcLookupId field_lbl_name		`thenM` \ sel_id ->
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

	tcExpr rhs field_ty 			`thenM` \ rhs' ->

	returnM (sel_id, rhs')

badFields rbinds data_con
  = filter (not . (`elem` field_names)) (recBindFields rbinds)
  where
    field_names = map fieldLabelName (dataConFieldLabels data_con)

checkMissingFields :: DataCon -> RenamedRecordBinds -> TcM ()
checkMissingFields data_con rbinds
  | null field_labels 	-- Not declared as a record;
			-- But C{} is still valid if no strict fields
  = if any isMarkedStrict field_strs then
	-- Illegal if any arg is strict
	addErrTc (missingStrictFields data_con [])
    else
	returnM ()
			
  | otherwise		-- A record
  = checkM (null missing_s_fields)
	   (addErrTc (missingStrictFields data_con missing_s_fields))	`thenM_`

    doptM Opt_WarnMissingFields		`thenM` \ warn ->
    checkM (not (warn && notNull missing_ns_fields))
	   (warnTc True (missingFields data_con missing_ns_fields))

  where
    missing_s_fields
	= [ fl | (fl, str) <- field_info,
	  	 isMarkedStrict str,
	  	 not (fieldLabelName fl `elem` field_names_used)
	  ]
    missing_ns_fields
	= [ fl | (fl, str) <- field_info,
	  	 not (isMarkedStrict str),
	  	 not (fieldLabelName fl `elem` field_names_used)
	  ]

    field_names_used = recBindFields rbinds
    field_labels     = dataConFieldLabels data_con

    field_info = zipEqual "missingFields"
			  field_labels
	  		  field_strs

    field_strs = dropList ex_theta (dataConStrictMarks data_con)
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
tcMonoExprs :: [RenamedHsExpr] -> [TcType] -> TcM [TcExpr]

tcMonoExprs [] [] = returnM []
tcMonoExprs (expr:exprs) (ty:tys)
 = tcMonoExpr  expr  ty		`thenM` \ expr' ->
   tcMonoExprs exprs tys	`thenM` \ exprs' ->
   returnM (expr':exprs')
\end{code}


%************************************************************************
%*									*
\subsection{Literals}
%*									*
%************************************************************************

Overloaded literals.

\begin{code}
tcLit :: HsLit -> TcType -> TcM TcExpr
tcLit (HsLitLit s _) res_ty
  = tcLookupClass cCallableClassName			`thenM` \ cCallableClass ->
    newDicts (LitLitOrigin (unpackFS s))
	     [mkClassPred cCallableClass [res_ty]]	`thenM` \ dicts ->
    extendLIEs dicts					`thenM_`
    returnM (HsLit (HsLitLit s res_ty))

tcLit lit res_ty 
  = unifyTauTy res_ty (hsLitType lit)		`thenM_`
    returnM (HsLit lit)
\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

Boring and alphabetical:
\begin{code}
arithSeqCtxt expr
  = hang (ptext SLIT("In an arithmetic sequence:")) 4 (ppr expr)


badStageErr id bind_lvl use_lvl
  = ptext SLIT("Stage error:") <+> quotes (ppr id) <+> 
	hsep   [ptext SLIT("is bound at stage") <+> ppr bind_lvl,
		ptext SLIT("but used at stage") <+> ppr use_lvl]

parrSeqCtxt expr
  = hang (ptext SLIT("In a parallel array sequence:")) 4 (ppr expr)

caseCtxt expr
  = hang (ptext SLIT("In the case expression:")) 4 (ppr expr)

caseScrutCtxt expr
  = hang (ptext SLIT("In the scrutinee of a case expression:")) 4 (ppr expr)

exprSigCtxt expr
  = hang (ptext SLIT("When checking the type signature of the expression:"))
	 4 (ppr expr)

exprCtxt expr
  = hang (ptext SLIT("In the expression:")) 4 (ppr expr)

fieldCtxt field_name
  = ptext SLIT("In the") <+> quotes (ppr field_name) <+> ptext SLIT("field of a record")

funAppCtxt fun arg arg_no
  = hang (hsep [ ptext SLIT("In the"), speakNth arg_no, ptext SLIT("argument of"), 
		    quotes (ppr fun) <> text ", namely"])
	 4 (quotes (ppr arg))

listCtxt expr
  = hang (ptext SLIT("In the list element:")) 4 (ppr expr)

parrCtxt expr
  = hang (ptext SLIT("In the parallel array element:")) 4 (ppr expr)

predCtxt expr
  = hang (ptext SLIT("In the predicate expression:")) 4 (ppr expr)

illegalBracket level
  = ptext SLIT("Illegal bracket at level") <+> ppr level

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
	 4 (pprQuotedList (recBindFields rbinds))

recordUpdCtxt expr = ptext SLIT("In the record update:") <+> ppr expr
recordConCtxt expr = ptext SLIT("In the record construction:") <+> ppr expr

notSelector field
  = hsep [quotes (ppr field), ptext SLIT("is not a record selector")]

missingStrictFields :: DataCon -> [FieldLabel] -> SDoc
missingStrictFields con fields
  = header <> rest
  where
    rest | null fields = empty	-- Happens for non-record constructors 
				-- with strict fields
	 | otherwise   = colon <+> pprWithCommas ppr fields

    header = ptext SLIT("Constructor") <+> quotes (ppr con) <+> 
	     ptext SLIT("does not have the required strict field(s)") 
	  

missingFields :: DataCon -> [FieldLabel] -> SDoc
missingFields con fields
  = ptext SLIT("Fields of") <+> quotes (ppr con) <+> ptext SLIT("not initialised:") 
	<+> pprWithCommas ppr fields

polySpliceErr :: Id -> SDoc
polySpliceErr id
  = ptext SLIT("Can't splice the polymorphic local variable") <+> quotes (ppr id)

wrongArgsCtxt too_many_or_few fun args
  = hang (ptext SLIT("Probable cause:") <+> quotes (ppr fun)
		    <+> ptext SLIT("is applied to") <+> text too_many_or_few 
		    <+> ptext SLIT("arguments in the call"))
	 4 (parens (ppr the_app))
  where
    the_app = foldl HsApp fun args	-- Used in error messages
\end{code}
