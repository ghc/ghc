%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcExpr]{Typecheck an expression}

\begin{code}
module TcExpr ( tcExpr, tcStmt, tcId ) where

#include "HsVersions.h"

import HsSyn		( HsExpr(..), HsLit(..), ArithSeqInfo(..), 
			  HsBinds(..), Stmt(..), DoOrListComp(..),
			  failureFreePat, collectPatBinders
			)
import RnHsSyn		( RenamedHsExpr, 
			  RenamedStmt, RenamedRecordBinds
			)
import TcHsSyn		( TcExpr, TcStmt,
			  TcRecordBinds,
			  mkHsTyApp
			)

import TcMonad
import BasicTypes	( RecFlag(..) )

import Inst		( Inst, InstOrigin(..), OverloadedLit(..),
			  LIE, emptyLIE, plusLIE, plusLIEs, newOverloadedLit,
			  newMethod, newMethodWithGivenTy, newDicts )
import TcBinds		( tcBindsAndThen, checkSigTyVars, sigThetaCtxt )
import TcEnv		( TcIdOcc(..), tcInstId,
			  tcLookupLocalValue, tcLookupGlobalValue, tcLookupClassByKey,
			  tcLookupGlobalValueByKey, newMonoIds,
			  tcExtendGlobalTyVars, tcLookupGlobalValueMaybe,
			  tcLookupTyCon
			)
import TcMatches	( tcMatchesCase, tcMatchExpected )
import TcMonoType	( tcHsType )
import TcPat		( tcPat )
import TcSimplify	( tcSimplifyAndCheck )
import TcType		( TcType, TcMaybe(..),
			  tcInstType, tcInstSigTcType, tcInstTyVars,
			  tcInstSigType, tcInstTcType, tcInstTheta, tcSplitRhoTy,
			  newTyVarTy, newTyVarTys, zonkTcType )
import TcKind		( TcKind )

import Class		( Class )
import FieldLabel	( FieldLabel, fieldLabelName, fieldLabelType )
import Id		( idType, dataConFieldLabels, dataConSig, recordSelectorFieldLabel,
			  isRecordSelector,
			  Id, GenId
			)
import Kind		( Kind, mkBoxedTypeKind, mkTypeKind, mkArrowKind )
import Name		( Name{-instance Eq-} )
import Type		( mkFunTy, mkAppTy, mkTyVarTy, mkTyVarTys,
			  splitFunTy_maybe, splitFunTys,
			  mkTyConApp,
			  splitForAllTys, splitRhoTy, splitSigmaTy, 
			  isTauTy, tyVarsOfType, tyVarsOfTypes, 
			  splitForAllTy_maybe, splitAlgTyConApp, splitAlgTyConApp_maybe
			)
import TyVar		( emptyTyVarEnv, zipTyVarEnv,
			  elementOfTyVarSet, mkTyVarSet, tyVarSetToList
			)
import TyCon		( tyConDataCons )
import TysPrim		( intPrimTy, charPrimTy, doublePrimTy,
			  floatPrimTy, addrPrimTy
			)
import TysWiredIn	( boolTy, charTy, stringTy )
import PrelInfo		( ioTyCon_NAME )
import Unify		( unifyTauTy, unifyFunTy, unifyListTy, unifyTupleTy )
import Unique		( Unique, cCallableClassKey, cReturnableClassKey, 
			  enumFromClassOpKey, enumFromThenClassOpKey,
			  enumFromToClassOpKey, enumFromThenToClassOpKey,
			  thenMClassOpKey, zeroClassOpKey, returnMClassOpKey
			)
import Outputable
import PprType		( GenType, GenTyVar )	-- Instances
import Maybes		( maybeToBool )
import ListSetOps	( minusList )
import Util
\end{code}

\begin{code}
tcExpr :: RenamedHsExpr			-- Expession to type check
       -> TcType s 			-- Expected type (could be a type variable)
       -> TcM s (TcExpr s, LIE s)
\end{code}

%************************************************************************
%*									*
\subsection{The TAUT rules for variables}
%*									*
%************************************************************************

\begin{code}
tcExpr (HsVar name) res_ty
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
tcExpr (HsLit (HsInt i)) res_ty
  = newOverloadedLit (LiteralOrigin (HsInt i))
		     (OverloadedIntegral i)
		     res_ty  `thenNF_Tc` \ stuff ->
    returnTc stuff

tcExpr (HsLit (HsFrac f)) res_ty
  = newOverloadedLit (LiteralOrigin (HsFrac f))
		     (OverloadedFractional f)
		     res_ty  `thenNF_Tc` \ stuff ->
    returnTc stuff


tcExpr (HsLit lit@(HsLitLit s)) res_ty
  = tcLookupClassByKey cCallableClassKey		`thenNF_Tc` \ cCallableClass ->
    newDicts (LitLitOrigin (_UNPK_ s))
	     [(cCallableClass, [res_ty])]		`thenNF_Tc` \ (dicts, _) ->
    returnTc (HsLitOut lit res_ty, dicts)
\end{code}

Primitive literals:

\begin{code}
tcExpr (HsLit lit@(HsCharPrim c)) res_ty
  = unifyTauTy charPrimTy res_ty		`thenTc_`
    returnTc (HsLitOut lit charPrimTy, emptyLIE)

tcExpr (HsLit lit@(HsStringPrim s)) res_ty
  = unifyTauTy addrPrimTy res_ty		`thenTc_`
    returnTc (HsLitOut lit addrPrimTy, emptyLIE)

tcExpr (HsLit lit@(HsIntPrim i)) res_ty
  = unifyTauTy intPrimTy res_ty		`thenTc_`
    returnTc (HsLitOut lit intPrimTy, emptyLIE)

tcExpr (HsLit lit@(HsFloatPrim f)) res_ty
  = unifyTauTy floatPrimTy res_ty		`thenTc_`
    returnTc (HsLitOut lit floatPrimTy, emptyLIE)

tcExpr (HsLit lit@(HsDoublePrim d)) res_ty
  = unifyTauTy doublePrimTy res_ty		`thenTc_`
    returnTc (HsLitOut lit doublePrimTy, emptyLIE)
\end{code}

Unoverloaded literals:

\begin{code}
tcExpr (HsLit lit@(HsChar c)) res_ty
  = unifyTauTy charTy res_ty		`thenTc_`
    returnTc (HsLitOut lit charTy, emptyLIE)

tcExpr (HsLit lit@(HsString str)) res_ty
  = unifyTauTy stringTy res_ty		`thenTc_`
    returnTc (HsLitOut lit stringTy, emptyLIE)
\end{code}

%************************************************************************
%*									*
\subsection{Other expression forms}
%*									*
%************************************************************************

\begin{code}
tcExpr (HsPar expr) res_ty -- preserve parens so printing needn't guess where they go
  = tcExpr expr res_ty

-- perform the negate *before* overloading the integer, since the case
-- of minBound on Ints fails otherwise.  Could be done elsewhere, but
-- convenient to do it here.

tcExpr (NegApp (HsLit (HsInt i)) neg) res_ty
  = tcExpr (HsLit (HsInt (-i))) res_ty

tcExpr (NegApp expr neg) res_ty 
  = tcExpr (HsApp neg expr) res_ty

tcExpr (HsLam match) res_ty
  = tcMatchExpected [] res_ty match	`thenTc` \ (match',lie) ->
    returnTc (HsLam match', lie)

tcExpr (HsApp e1 e2) res_ty = accum e1 [e2]
  where
    accum (HsApp e1 e2) args = accum e1 (e2:args)
    accum fun args
      = tcApp fun args res_ty 	`thenTc` \ (fun', args', lie) ->
	returnTc (foldl HsApp fun' args', lie)

-- equivalent to (op e1) e2:
tcExpr (OpApp arg1 op fix arg2) res_ty
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

tcExpr in_expr@(SectionL arg op) res_ty
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

tcExpr in_expr@(SectionR op expr) res_ty
  = tcExpr_id op		`thenTc`    \ (op', lie1, op_ty) ->
    tcAddErrCtxt (sectionRAppCtxt in_expr) $
    split_fun_ty op_ty 2 {- two args -}			`thenTc` \ ([arg1_ty, arg2_ty], op_res_ty) ->
    tcExpr expr	arg2_ty					`thenTc` \ (expr',lie2) ->
    unifyTauTy (mkFunTy arg1_ty op_res_ty) res_ty	`thenTc_`
    returnTc (SectionR op' expr', lie1 `plusLIE` lie2)
\end{code}

The interesting thing about @ccall@ is that it is just a template
which we instantiate by filling in details about the types of its
argument and result (ie minimal typechecking is performed).  So, the
basic story is that we allocate a load of type variables (to hold the
arg/result types); unify them with the args/result; and store them for
later use.

\begin{code}
tcExpr (CCall lbl args may_gc is_asm ignored_fake_result_ty) res_ty
  = 	-- Get the callable and returnable classes.
    tcLookupClassByKey cCallableClassKey	`thenNF_Tc` \ cCallableClass ->
    tcLookupClassByKey cReturnableClassKey	`thenNF_Tc` \ cReturnableClass ->
    tcLookupTyCon ioTyCon_NAME			`thenTc` \ (_,_,ioTyCon) ->

    let
	new_arg_dict (arg, arg_ty)
	  = newDicts (CCallOrigin (_UNPK_ lbl) (Just arg))
		     [(cCallableClass, [arg_ty])]	`thenNF_Tc` \ (arg_dicts, _) ->
	    returnNF_Tc arg_dicts	-- Actually a singleton bag

	result_origin = CCallOrigin (_UNPK_ lbl) Nothing {- Not an arg -}
    in

	-- Arguments
    mapNF_Tc (\ _ -> newTyVarTy mkTypeKind) [1..(length args)] `thenNF_Tc` \ ty_vars ->
    tcExprs args ty_vars		   		       `thenTc`    \ (args', args_lie) ->

	-- The argument types can be unboxed or boxed; the result
	-- type must, however, be boxed since it's an argument to the IO
	-- type constructor.
    newTyVarTy mkBoxedTypeKind  		`thenNF_Tc` \ result_ty ->
    let
	io_result_ty = mkTyConApp ioTyCon [result_ty]
    in
    case tyConDataCons ioTyCon of { [ioDataCon] ->
    unifyTauTy io_result_ty res_ty   `thenTc_`

	-- Construct the extra insts, which encode the
	-- constraints on the argument and result types.
    mapNF_Tc new_arg_dict (zipEqual "tcExpr:CCall" args ty_vars)    `thenNF_Tc` \ ccarg_dicts_s ->
    newDicts result_origin [(cReturnableClass, [result_ty])]	    `thenNF_Tc` \ (ccres_dict, _) ->

    returnTc (HsApp (HsVar (RealId ioDataCon) `TyApp` [result_ty])
		    (CCall lbl args' may_gc is_asm io_result_ty),
		      -- do the wrapping in the newtype constructor here
	      foldr plusLIE ccres_dict ccarg_dicts_s `plusLIE` args_lie)
    }
\end{code}

\begin{code}
tcExpr (HsSCC label expr) res_ty
  = tcExpr expr res_ty		`thenTc` \ (expr', lie) ->
    returnTc (HsSCC label expr', lie)

tcExpr (HsLet binds expr) res_ty
  = tcBindsAndThen
	combiner
	binds 			-- Bindings to check
	(tc_expr)	`thenTc` \ (expr', lie) ->
    returnTc (expr', lie)
  where
    tc_expr = tcExpr expr res_ty `thenTc` \ (expr', lie) ->
	      returnTc (expr', lie)
    combiner is_rec bind expr = HsLet (MonoBind bind [] is_rec) expr

tcExpr in_expr@(HsCase scrut matches src_loc) res_ty
  = tcAddSrcLoc src_loc			$
    tcAddErrCtxt (caseCtxt in_expr)	$

	-- Typecheck the case alternatives first.
	-- The case patterns tend to give good type info to use
	-- when typechecking the scrutinee.  For example
	--	case (map f) of
	--	  (x:xs) -> ...
	-- will report that map is applied to too few arguments

    tcMatchesCase res_ty matches	`thenTc`    \ (scrut_ty, matches', lie2) ->

    tcAddErrCtxt (caseScrutCtxt scrut)	(
      tcExpr scrut scrut_ty
    )					`thenTc`    \ (scrut',lie1) ->

    returnTc (HsCase scrut' matches' src_loc, plusLIE lie1 lie2)

tcExpr (HsIf pred b1 b2 src_loc) res_ty
  = tcAddSrcLoc src_loc	$
    tcAddErrCtxt (predCtxt pred) (
    tcExpr pred boolTy	)	`thenTc`    \ (pred',lie1) ->

    tcExpr b1 res_ty		`thenTc`    \ (b1',lie2) ->
    tcExpr b2 res_ty		`thenTc`    \ (b2',lie3) ->
    returnTc (HsIf pred' b1' b2' src_loc, plusLIE lie1 (plusLIE lie2 lie3))
\end{code}

\begin{code}
tcExpr expr@(HsDo do_or_lc stmts src_loc) res_ty
  = tcDoStmts do_or_lc stmts src_loc res_ty
\end{code}

\begin{code}
tcExpr in_expr@(ExplicitList exprs) res_ty	-- Non-empty list
  = unifyListTy res_ty                        `thenTc` \ elt_ty ->  
    mapAndUnzipTc (tc_elt elt_ty) exprs	      `thenTc` \ (exprs', lies) ->
    returnTc (ExplicitListOut elt_ty exprs', plusLIEs lies)
  where
    tc_elt elt_ty expr
      = tcAddErrCtxt (listCtxt expr) $
	tcExpr expr elt_ty

tcExpr (ExplicitTuple exprs) res_ty
  = unifyTupleTy (length exprs) res_ty		`thenTc` \ arg_tys ->
    mapAndUnzipTc (\ (expr, arg_ty) -> tcExpr expr arg_ty)
               (exprs `zip` arg_tys) -- we know they're of equal length.
               							 `thenTc` \ (exprs', lies) ->
    returnTc (ExplicitTuple exprs', plusLIEs lies)

tcExpr (RecordCon con_name _ rbinds) res_ty
  = tcLookupGlobalValue con_name	`thenNF_Tc` \ con_id ->
    tcId con_name			`thenNF_Tc` \ (con_expr, con_lie, con_tau) ->
    let
	(_, record_ty) = splitFunTys con_tau
    in
	-- Con is syntactically constrained to be a data constructor
    ASSERT( maybeToBool (splitAlgTyConApp_maybe record_ty ) )
    unifyTauTy res_ty record_ty          `thenTc_`

	-- Check that the record bindings match the constructor
    let
	bad_fields = badFields rbinds con_id
    in
    checkTc (null bad_fields) (badFieldsCon con_id bad_fields)	`thenTc_`

	-- Typecheck the record bindings
	-- (Do this after checkRecordFields in case there's a field that
	--  doesn't match the constructor.)
    tcRecordBinds record_ty rbinds		`thenTc` \ (rbinds', rbinds_lie) ->

    returnTc (RecordCon (RealId con_id) con_expr rbinds', con_lie `plusLIE` rbinds_lie)


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

tcExpr (RecordUpd record_expr rbinds) res_ty
  = tcAddErrCtxt recordUpdCtxt			$

	-- STEP 1
	-- Figure out the tycon and data cons from the first field name
    ASSERT( not (null rbinds) )
    let 
	((first_field_name, _, _) : rest) = rbinds
    in
    tcLookupGlobalValueMaybe first_field_name	`thenNF_Tc` \ maybe_sel_id ->
    (case maybe_sel_id of
	Just sel_id | isRecordSelector sel_id -> returnTc sel_id
	other				      -> failWithTc (notSelector first_field_name)
    )						`thenTc` \ sel_id ->
    let
	(_, tau)	      	  = splitForAllTys (idType sel_id)
	Just (data_ty, _)     	  = splitFunTy_maybe tau	-- Must succeed since sel_id is a selector
	(tycon, _, data_cons) 	  = splitAlgTyConApp data_ty
	(con_tyvars, theta, _, _, _, _) = dataConSig (head data_cons)
    in
    tcInstTyVars con_tyvars			`thenNF_Tc` \ (_, result_inst_tys, _) ->

	-- STEP 2
	-- Check for bad fields
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
	upd_field_lbls      = [recordSelectorFieldLabel sel_id | (RealId sel_id, _, _) <- rbinds']
	con_field_lbls_s    = map dataConFieldLabels data_cons

		-- A constructor is only relevant to this process if
		-- it contains all the fields that are being updated
	relevant_field_lbls_s      = filter is_relevant con_field_lbls_s
	is_relevant con_field_lbls = all (`elem` con_field_lbls) upd_field_lbls

	non_upd_field_lbls  = concat relevant_field_lbls_s `minusList` upd_field_lbls
	common_tyvars       = tyVarsOfTypes (map fieldLabelType non_upd_field_lbls)

	mk_inst_ty (tyvar, result_inst_ty) 
	  | tyvar `elementOfTyVarSet` common_tyvars = returnNF_Tc result_inst_ty	-- Same as result type
	  | otherwise			            = newTyVarTy mkBoxedTypeKind	-- Fresh type
    in
    mapNF_Tc mk_inst_ty (zip con_tyvars result_inst_tys)	`thenNF_Tc` \ inst_tys ->

	-- STEP 5
	-- Typecheck the expression to be updated
    let
	record_ty = mkTyConApp tycon inst_tys
    in
    tcExpr record_expr record_ty			`thenTc`    \ (record_expr', record_lie) ->

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
	inst_env = zipTyVarEnv tyvars result_inst_tys
    in
    tcInstTheta inst_env theta			`thenNF_Tc` \ theta' ->
    newDicts RecordUpdOrigin theta'		`thenNF_Tc` \ (con_lie, dicts) ->

	-- Phew!
    returnTc (RecordUpdOut record_expr' result_record_ty dicts rbinds', 
	      con_lie `plusLIE` record_lie `plusLIE` rbinds_lie)

tcExpr (ArithSeqIn seq@(From expr)) res_ty
  = unifyListTy res_ty                        `thenTc` \ elt_ty ->  
    tcExpr expr elt_ty			      `thenTc` \ (expr', lie1) ->

    tcLookupGlobalValueByKey enumFromClassOpKey	`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq)
	      (RealId sel_id) [elt_ty]		`thenNF_Tc` \ (lie2, enum_from_id) ->

    returnTc (ArithSeqOut (HsVar enum_from_id) (From expr'),
	      lie1 `plusLIE` lie2)

tcExpr in_expr@(ArithSeqIn seq@(FromThen expr1 expr2)) res_ty
  = tcAddErrCtxt (arithSeqCtxt in_expr) $ 
    unifyListTy  res_ty         `thenTc`    \ elt_ty ->  
    tcExpr expr1 elt_ty		`thenTc`    \ (expr1',lie1) ->
    tcExpr expr2 elt_ty		`thenTc`    \ (expr2',lie2) ->
    tcLookupGlobalValueByKey enumFromThenClassOpKey	`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq)
	      (RealId sel_id) [elt_ty]			`thenNF_Tc` \ (lie3, enum_from_then_id) ->

    returnTc (ArithSeqOut (HsVar enum_from_then_id)
			   (FromThen expr1' expr2'),
	      lie1 `plusLIE` lie2 `plusLIE` lie3)

tcExpr in_expr@(ArithSeqIn seq@(FromTo expr1 expr2)) res_ty
  = tcAddErrCtxt (arithSeqCtxt in_expr) $
    unifyListTy  res_ty         `thenTc`    \ elt_ty ->  
    tcExpr expr1 elt_ty		`thenTc`    \ (expr1',lie1) ->
    tcExpr expr2 elt_ty		`thenTc`    \ (expr2',lie2) ->
    tcLookupGlobalValueByKey enumFromToClassOpKey	`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq)
	      (RealId sel_id) [elt_ty] 		`thenNF_Tc` \ (lie3, enum_from_to_id) ->

    returnTc (ArithSeqOut (HsVar enum_from_to_id)
			  (FromTo expr1' expr2'),
	      lie1 `plusLIE` lie2 `plusLIE` lie3)

tcExpr in_expr@(ArithSeqIn seq@(FromThenTo expr1 expr2 expr3)) res_ty
  = tcAddErrCtxt  (arithSeqCtxt in_expr) $
    unifyListTy  res_ty         `thenTc`    \ elt_ty ->  
    tcExpr expr1 elt_ty		`thenTc`    \ (expr1',lie1) ->
    tcExpr expr2 elt_ty		`thenTc`    \ (expr2',lie2) ->
    tcExpr expr3 elt_ty		`thenTc`    \ (expr3',lie3) ->
    tcLookupGlobalValueByKey enumFromThenToClassOpKey	`thenNF_Tc` \ sel_id ->
    newMethod (ArithSeqOrigin seq)
	      (RealId sel_id) [elt_ty]			`thenNF_Tc` \ (lie4, eft_id) ->

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
tcExpr in_expr@(ExprWithTySig expr poly_ty) res_ty
 = tcSetErrCtxt (exprSigCtxt in_expr)	$
   tcHsType  poly_ty		`thenTc` \ sigma_sig ->

	-- Check the tau-type part
   tcInstSigType sigma_sig		`thenNF_Tc` \ sigma_sig' ->
   let
	(sig_tyvars', sig_theta', sig_tau') = splitSigmaTy sigma_sig'
   in

	-- Type check the expression, expecting the signature type
   tcExtendGlobalTyVars sig_tyvars' (
	   tcExpr expr sig_tau'
   )						`thenTc` \ (texpr, lie) ->

	-- Check the type variables of the signature, 
	-- *after* typechecking the expression
   checkSigTyVars sig_tyvars' sig_tau'		`thenTc` \ zonked_sig_tyvars ->

	-- Check overloading constraints
   newDicts SignatureOrigin sig_theta'		`thenNF_Tc` \ (sig_dicts, _) ->
   tcAddErrCtxtM (sigThetaCtxt sig_dicts)	(
     tcSimplifyAndCheck
        (text "expr ty sig")
	(mkTyVarSet zonked_sig_tyvars)
	sig_dicts lie				
   )						`thenTc_`

	-- Now match the signature type with res_ty.
	-- We must not do this earlier, because res_ty might well
	-- mention variables free in the environment, and we'd get
	-- bogus complaints about not being able to for-all the
	-- sig_tyvars
   unifyTauTy sig_tau' res_ty		`thenTc_`

	-- If everything is ok, return the stuff unchanged, except for
	-- the effect of any substutions etc.  We simply discard the
	-- result of the tcSimplifyAndCheck, except for any default
	-- resolution it may have done, which is recorded in the
	-- substitution.
   returnTc (texpr, lie)

\end{code}

Typecheck expression which in most cases will be an Id.

\begin{code}
tcExpr_id :: RenamedHsExpr
           -> TcM s (TcExpr s,
 	       	     LIE s,
	             TcType s)
tcExpr_id id_expr
 = case id_expr of
	HsVar name -> tcId name			  `thenNF_Tc` \ stuff -> 
		      returnTc stuff
	other	   -> newTyVarTy mkTypeKind       `thenNF_Tc` \ id_ty ->
		      tcExpr id_expr id_ty	  `thenTc`    \ (id_expr', lie_id) ->
		      returnTc (id_expr', lie_id, id_ty) 
\end{code}

%************************************************************************
%*									*
\subsection{@tcApp@ typchecks an application}
%*									*
%************************************************************************

\begin{code}

tcApp :: RenamedHsExpr -> [RenamedHsExpr]   -- Function and args
      -> TcType s			    -- Expected result type of application
      -> TcM s (TcExpr s, [TcExpr s],	    -- Translated fun and args
		LIE s)

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
checkArgsCtxt fun args expected_res_ty actual_res_ty
  = zonkTcType expected_res_ty	  `thenNF_Tc` \ exp_ty' ->
    zonkTcType actual_res_ty	  `thenNF_Tc` \ act_ty' ->
    let
      (exp_args, _) = splitFunTys exp_ty'
      (act_args, _) = splitFunTys act_ty'
      message | length exp_args < length act_args = wrongArgsCtxt "too few" fun args
              | length exp_args > length act_args = wrongArgsCtxt "too many" fun args
	      | otherwise			  = appCtxt fun args
    in
    returnNF_Tc message


split_fun_ty :: TcType s		-- The type of the function
	     -> Int			-- Number of arguments
	     -> TcM s ([TcType s],	-- Function argument types
		       TcType s)	-- Function result types

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
      -> (RenamedHsExpr, TcType s, Int)	-- Actual argument and expected arg type
      -> TcM s (TcExpr s, LIE s)	-- Resulting argument and LIE

tcArg the_fun (arg, expected_arg_ty, arg_no)
  = tcAddErrCtxt (funAppCtxt the_fun arg arg_no) $
    tcPolyExpr arg expected_arg_ty


-- tcPolyExpr is like tcExpr, except that the expected type
-- can be a polymorphic one.
tcPolyExpr arg expected_arg_ty
  | not (maybeToBool (splitForAllTy_maybe expected_arg_ty))
  = 	-- The ordinary, non-rank-2 polymorphic case
    tcExpr arg expected_arg_ty

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
	-- Type-check the arg and unify with expected type
    tcExpr arg sig_tau				`thenTc` \ (arg', lie_arg) ->

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
    tcExtendGlobalTyVars (tyVarSetToList (tyVarsOfType expected_arg_ty)) $

    checkSigTyVars sig_tyvars sig_tau		`thenTc` \ zonked_sig_tyvars ->
    newDicts Rank2Origin sig_theta		`thenNF_Tc` \ (sig_dicts, dict_ids) ->
	-- ToDo: better origin

    tcAddErrCtxtM (sigThetaCtxt sig_dicts) $
    tcSimplifyAndCheck (text "rank2")
		(mkTyVarSet zonked_sig_tyvars)
		sig_dicts lie_arg		`thenTc` \ (free_insts, inst_binds) ->

	    -- This HsLet binds any Insts which came out of the simplification.
	    -- It's a bit out of place here, but using AbsBind involves inventing
	    -- a couple of new names which seems worse.
    returnTc ( TyLam zonked_sig_tyvars $
		   DictLam dict_ids $
		   HsLet (MonoBind inst_binds [] Recursive) 
		   arg' 
		 , free_insts
		 )
\end{code}

%************************************************************************
%*									*
\subsection{@tcId@ typchecks an identifier occurrence}
%*									*
%************************************************************************

\begin{code}
tcId :: Name -> NF_TcM s (TcExpr s, LIE s, TcType s)

tcId name
  = 	-- Look up the Id and instantiate its type
    tcLookupLocalValue name	`thenNF_Tc` \ maybe_local ->

    case maybe_local of
      Just tc_id -> instantiate_it (TcId tc_id) (idType tc_id)

      Nothing ->    tcLookupGlobalValue name		 `thenNF_Tc` \ id ->
		    tcInstType emptyTyVarEnv (idType id) `thenNF_Tc` \ inst_ty ->
		    let
			(tyvars, rho) = splitForAllTys inst_ty 
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
      = tcSplitRhoTy rho				`thenNF_Tc` \ (theta, tau) ->
	if null theta then 	-- Is it overloaded?
		returnNF_Tc (mkHsTyApp (HsVar tc_id_occ) arg_tys, emptyLIE, tau)
	else
		-- Yes, it's overloaded
	newMethodWithGivenTy (OccurrenceOf tc_id_occ)
			     tc_id_occ arg_tys theta tau `thenNF_Tc` \ (lie1, meth_id) ->
	instantiate_it meth_id tau			 `thenNF_Tc` \ (expr, lie2, final_tau) ->
	returnNF_Tc (expr, lie1 `plusLIE` lie2, final_tau)

      where
	arg_tys	      = mkTyVarTys tyvars
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
    newTyVarTy (mkArrowKind mkBoxedTypeKind mkBoxedTypeKind)	`thenNF_Tc` \ m ->

    let
      tc_stmts []	    = returnTc (([], error "tc_stmts"), emptyLIE)
      tc_stmts (stmt:stmts) = tcStmt tcExpr do_or_lc (mkAppTy m) combine_stmts stmt $
			      tc_stmts stmts

      combine_stmts stmt@(ReturnStmt _) (Just ty) ([], _) = ([stmt], ty)
      combine_stmts stmt@(ExprStmt e _) (Just ty) ([], _) = ([stmt], ty)
      combine_stmts stmt 		_ 	  ([], _) = panic "Bad last stmt tcDoStmts"
      combine_stmts stmt		_     (stmts, ty) = (stmt:stmts, ty)
    in
    tc_stmts stmts			`thenTc`   \ ((stmts', result_ty), final_lie) ->
    unifyTauTy result_ty res_ty		`thenTc_`

	-- Build the then and zero methods in case we need them
	-- It's important that "then" and "return" appear just once in the final LIE,
	-- not only for typechecker efficiency, but also because otherwise during
	-- simplification we end up with silly stuff like
	--	then = case d of (t,r) -> t
	--	then = then
	-- where the second "then" sees that it already exists in the "available" stuff.
	--
    tcLookupGlobalValueByKey returnMClassOpKey	`thenNF_Tc` \ return_sel_id ->
    tcLookupGlobalValueByKey thenMClassOpKey	`thenNF_Tc` \ then_sel_id ->
    tcLookupGlobalValueByKey zeroClassOpKey	`thenNF_Tc` \ zero_sel_id ->
    newMethod DoOrigin
	      (RealId return_sel_id) [m]	`thenNF_Tc` \ (return_lie, return_id) ->
    newMethod DoOrigin
	      (RealId then_sel_id) [m]		`thenNF_Tc` \ (then_lie, then_id) ->
    newMethod DoOrigin
	      (RealId zero_sel_id) [m]		`thenNF_Tc` \ (zero_lie, zero_id) ->
    let
      monad_lie = then_lie `plusLIE` return_lie `plusLIE` perhaps_zero_lie
      perhaps_zero_lie | all failure_free stmts' = emptyLIE
		       | otherwise		 = zero_lie

      failure_free (BindStmt pat _ _) = failureFreePat pat
      failure_free (GuardStmt _ _)    = False
      failure_free other_stmt	      = True
    in
    returnTc (HsDoOut do_or_lc stmts' return_id then_id zero_id res_ty src_loc,
	      final_lie `plusLIE` monad_lie)

\end{code}

\begin{code}
tcStmt :: (RenamedHsExpr -> TcType s -> TcM s (TcExpr s, LIE s))	-- This is tcExpr
				-- The sole, disgusting, reason for this parameter
				-- is to get the effect of polymorphic recursion
				-- ToDo: rm when booting with Haskell 1.3
       -> DoOrListComp
       -> (TcType s -> TcType s)		-- Relationship type of pat and rhs in pat <- rhs
       -> (TcStmt s -> Maybe (TcType s) -> thing -> thing)
       -> RenamedStmt
       -> TcM s (thing, LIE s)
       -> TcM s (thing, LIE s)

tcStmt tc_expr do_or_lc m combine stmt@(ReturnStmt exp) do_next
  = ASSERT( case do_or_lc of { DoStmt -> False; ListComp -> True; Guard -> True } )
    tcSetErrCtxt (stmtCtxt do_or_lc stmt) (
        newTyVarTy mkTypeKind                `thenNF_Tc` \ exp_ty ->
	tc_expr exp exp_ty		     `thenTc`    \ (exp', exp_lie) ->
	returnTc (ReturnStmt exp', exp_lie, m exp_ty)
    )					`thenTc` \ (stmt', stmt_lie, stmt_ty) ->
    do_next				`thenTc` \ (thing', thing_lie) ->
    returnTc (combine stmt' (Just stmt_ty) thing',
  	      stmt_lie `plusLIE` thing_lie)

tcStmt tc_expr do_or_lc m combine stmt@(GuardStmt exp src_loc) do_next
  = ASSERT( case do_or_lc of { DoStmt -> False; ListComp -> True; Guard -> True } )
    newTyVarTy mkTypeKind                    `thenNF_Tc` \ exp_ty ->
    tcAddSrcLoc src_loc 		(
    tcSetErrCtxt (stmtCtxt do_or_lc stmt) (
  	tc_expr exp boolTy 		`thenTc`    \ (exp', exp_lie) ->
  	returnTc (GuardStmt exp' src_loc, exp_lie)
    ))					`thenTc` \ (stmt', stmt_lie) ->
    do_next				`thenTc` \ (thing', thing_lie) ->
    returnTc (combine stmt' Nothing thing',
  	      stmt_lie `plusLIE` thing_lie)

tcStmt tc_expr do_or_lc m combine stmt@(ExprStmt exp src_loc) do_next
  = ASSERT( case do_or_lc of { DoStmt -> True; ListComp -> False; Guard -> False } )
    newTyVarTy mkTypeKind                    `thenNF_Tc` \ exp_ty ->
    tcAddSrcLoc src_loc 		(
    tcSetErrCtxt (stmtCtxt do_or_lc stmt)	(
  	newTyVarTy mkTypeKind		`thenNF_Tc` \ tau ->
	let
	    -- exp has type (m tau) for some tau (doesn't matter what)
	    exp_ty = m tau
	in
  	tc_expr exp exp_ty		`thenTc`    \ (exp', exp_lie) ->
  	returnTc (ExprStmt exp' src_loc, exp_lie, exp_ty)
    ))					`thenTc` \ (stmt',  stmt_lie, stmt_ty) ->
    do_next				`thenTc` \ (thing', thing_lie) ->
    returnTc (combine stmt' (Just stmt_ty) thing',
  	      stmt_lie `plusLIE` thing_lie)

tcStmt tc_expr do_or_lc m combine stmt@(BindStmt pat exp src_loc) do_next
  = newMonoIds (collectPatBinders pat) mkBoxedTypeKind $ \ _ ->
    tcAddSrcLoc src_loc		(
    tcSetErrCtxt (stmtCtxt do_or_lc stmt)	(
  	tcPat pat			`thenTc`    \ (pat', pat_lie, pat_ty) ->  
      	tc_expr exp (m pat_ty)		`thenTc`    \ (exp', exp_lie) ->

  	-- NB: the environment has been extended with the new binders
  	-- which the rhs can't "see", but the renamer should have made
  	-- sure that everything is distinct by now, so there's no problem.
  	-- Putting the tcExpr before the newMonoIds messes up the nesting
  	-- of error contexts, so I didn't  bother

  	returnTc (BindStmt pat' exp' src_loc, pat_lie `plusLIE` exp_lie)
    ))					`thenTc` \ (stmt', stmt_lie) ->
    do_next				`thenTc` \ (thing', thing_lie) ->
    returnTc (combine stmt' Nothing thing',
  	      stmt_lie `plusLIE` thing_lie)

tcStmt tc_expr do_or_lc m combine (LetStmt binds) do_next
     = tcBindsAndThen		-- No error context, but a binding group is
  	combine'		-- rather a large thing for an error context anyway
  	binds
  	do_next
     where
      	combine' is_rec binds' thing' = combine (LetStmt (MonoBind binds' [] is_rec)) Nothing thing'
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
	:: TcType s		-- Expected type of whole record
	-> RenamedRecordBinds
	-> TcM s (TcRecordBinds s, LIE s)

tcRecordBinds expected_record_ty rbinds
  = mapAndUnzipTc do_bind rbinds	`thenTc` \ (rbinds', lies) ->
    returnTc (rbinds', plusLIEs lies)
  where
    do_bind (field_label, rhs, pun_flag)
      = tcLookupGlobalValue field_label	`thenNF_Tc` \ sel_id ->
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
	tcPolyExpr rhs field_ty				`thenTc` \ (rhs', lie) ->
	returnTc ((RealId sel_id, rhs', pun_flag), lie)

badFields rbinds data_con
  = [field_name | (field_name, _, _) <- rbinds,
		  not (field_name `elem` field_names)
    ]
  where
    field_names = map fieldLabelName (dataConFieldLabels data_con)
\end{code}

%************************************************************************
%*									*
\subsection{@tcExprs@ typechecks a {\em list} of expressions}
%*									*
%************************************************************************

\begin{code}
tcExprs :: [RenamedHsExpr] -> [TcType s] -> TcM s ([TcExpr s], LIE s)

tcExprs [] [] = returnTc ([], emptyLIE)
tcExprs (expr:exprs) (ty:tys)
 = tcExpr  expr	 ty		`thenTc` \ (expr',  lie1) ->
   tcExprs exprs tys		`thenTc` \ (exprs', lie2) ->
   returnTc (expr':exprs', lie1 `plusLIE` lie2)
\end{code}


% =================================================

Errors and contexts
~~~~~~~~~~~~~~~~~~~

Mini-utils:
\begin{code}
pp_nest_hang :: String -> SDoc -> SDoc
pp_nest_hang label stuff = nest 2 (hang (text label) 4 stuff)
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

stmtCtxt do_or_lc stmt
  = hang (ptext SLIT("In a") <+> whatever <> colon)
         4 (ppr stmt)
  where
    whatever = case do_or_lc of
		 ListComp -> ptext SLIT("list-comprehension qualifier")
		 DoStmt   -> ptext SLIT("do statement")
		 Guard	  -> ptext SLIT("guard")

wrongArgsCtxt too_many_or_few fun args
  = hang (ptext SLIT("Probable cause:") <+> ppr fun
		    <+> ptext SLIT("is applied to") <+> text too_many_or_few 
		    <+> ptext SLIT("arguments in the call"))
	 4 (ppr the_app)
  where
    the_app = foldl HsApp fun args	-- Used in error messages

appCtxt fun args
  = ptext SLIT("In the application") <+> (ppr the_app)
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

badFieldsCon con fields
  = hsep [ptext SLIT("Constructor"), 		ppr con,
	   ptext SLIT("does not have field(s):"), pprQuotedList fields]

notSelector field
  = hsep [quotes (ppr field), ptext SLIT("is not a record selector")]
\end{code}
