%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcExpr]{Typecheck an expression}

\begin{code}
module TcExpr ( tcCheckSigma, tcCheckRho, tcInferRho, tcMonoExpr ) where

#include "HsVersions.h"

#ifdef GHCI 	/* Only if bootstrapped */
import {-# SOURCE #-}	TcSplice( tcSpliceExpr, tcBracket )
import Id		( Id )
import Name		( isExternalName )
import TcType		( isTauTy )
import TcEnv		( checkWellStaged )
import HsSyn		( nlHsApp )
import qualified DsMeta
#endif

import HsSyn		( HsExpr(..), LHsExpr, HsLit(..), ArithSeqInfo(..), recBindFields,
			  HsMatchContext(..), HsRecordBinds, mkHsApp, nlHsVar )
import TcHsSyn		( hsLitType, (<$>) )
import TcRnMonad
import TcUnify		( Expected(..), tcInfer, zapExpectedType, zapExpectedTo, tcSubExp, tcGen,
			  unifyFunTys, zapToListTy, zapToTyConApp )
import BasicTypes	( isMarkedStrict )
import Inst		( newOverloadedLit, newMethodFromName, newIPDict,
			  newDicts, newMethodWithGivenTy, tcInstStupidTheta, tcInstCall )
import TcBinds		( tcBindsAndThen )
import TcEnv		( tcLookup, tcLookupId, checkProcLevel,
			  tcLookupDataCon, tcLookupGlobalId
			)
import TcArrows		( tcProc )
import TcMatches	( tcMatchesCase, tcMatchLambda, tcDoStmts, tcThingWithSig, TcMatchCtxt(..) )
import TcHsType		( tcHsSigType, UserTypeCtxt(..) )
import TcPat		( badFieldCon, refineTyVars )
import TcMType		( tcInstTyVars, tcInstType, newTyFlexiVarTy, zonkTcType )
import TcType		( Type, TcTyVar, TcType, TcSigmaType, TcRhoType, 
			  tcSplitFunTys, tcSplitTyConApp, mkTyVarTys,
			  isSigmaTy, mkFunTy, mkTyConApp, tyVarsOfTypes, isLinearPred,
			  tcSplitSigmaTy, tidyOpenType
			)
import Kind		( openTypeKind, liftedTypeKind, argTypeKind )

import Id		( idType, recordSelectorFieldLabel, isRecordSelector )
import DataCon		( DataCon, dataConFieldLabels, dataConStrictMarks, dataConWrapId )
import Name		( Name )
import TyCon		( TyCon, FieldLabel, tyConTyVars, tyConStupidTheta, 
			  tyConDataCons, tyConFields )
import Type		( zipTopTvSubst, substTheta, substTy )
import Var		( tyVarKind )
import VarSet		( emptyVarSet, elemVarSet )
import TysWiredIn	( boolTy, parrTyCon, tupleTyCon )
import PrelNames	( enumFromName, enumFromThenName, 
			  enumFromToName, enumFromThenToName,
			  enumFromToPName, enumFromThenToPName
			)
import ListSetOps	( minusList )
import CmdLineOpts
import HscTypes		( TyThing(..) )
import SrcLoc		( Located(..), unLoc, getLoc )
import Util
import Outputable
import FastString

#ifdef DEBUG
import TyCon		( isAlgTyCon )
#endif
\end{code}

%************************************************************************
%*									*
\subsection{Main wrappers}
%*									*
%************************************************************************

\begin{code}
-- tcCheckSigma does type *checking*; it's passed the expected type of the result
tcCheckSigma :: LHsExpr Name		-- Expession to type check
       	     -> TcSigmaType		-- Expected type (could be a polytpye)
       	     -> TcM (LHsExpr TcId)	-- Generalised expr with expected type

tcCheckSigma expr expected_ty 
  = traceTc (text "tcExpr" <+> (ppr expected_ty $$ ppr expr)) `thenM_`
    tc_expr' expr expected_ty

tc_expr' expr sigma_ty
  | isSigmaTy sigma_ty
  = tcGen sigma_ty emptyVarSet (
	\ rho_ty -> tcCheckRho expr rho_ty
    )				`thenM` \ (gen_fn, expr') ->
    returnM (L (getLoc expr') (gen_fn <$> unLoc expr'))

tc_expr' expr rho_ty	-- Monomorphic case
  = tcCheckRho expr rho_ty
\end{code}

Typecheck expression which in most cases will be an Id.
The expression can return a higher-ranked type, such as
	(forall a. a->a) -> Int
so we must create a hole to pass in as the expected tyvar.

\begin{code}
tcCheckRho :: LHsExpr Name -> TcRhoType -> TcM (LHsExpr TcId)
tcCheckRho expr rho_ty = tcMonoExpr expr (Check rho_ty)

tcInferRho :: LHsExpr Name -> TcM (LHsExpr TcId, TcRhoType)
tcInferRho (L loc (HsVar name)) = setSrcSpan loc $ do 
				  { (e,_,ty) <- tcId name; return (L loc e, ty)}
tcInferRho expr		        = tcInfer (tcMonoExpr expr)
\end{code}



%************************************************************************
%*									*
\subsection{The TAUT rules for variables}TcExpr
%*									*
%************************************************************************

\begin{code}
tcMonoExpr :: LHsExpr Name		-- Expession to type check
	   -> Expected TcRhoType 	-- Expected type (could be a type variable)
					-- Definitely no foralls at the top
					-- Can be a 'hole'.
	   -> TcM (LHsExpr TcId)

tcMonoExpr (L loc expr) res_ty
  = setSrcSpan loc (do { expr' <- tc_expr expr res_ty
		       ; return (L loc expr') })

tc_expr :: HsExpr Name -> Expected TcRhoType -> TcM (HsExpr TcId)
tc_expr (HsVar name) res_ty
  = do	{ (expr', _, id_ty) <- tcId name
	; co_fn <- tcSubExp res_ty id_ty
	; returnM (co_fn <$> expr') }

tc_expr (HsIPVar ip) res_ty
  = 	-- Implicit parameters must have a *tau-type* not a 
	-- type scheme.  We enforce this by creating a fresh
	-- type variable as its type.  (Because res_ty may not
	-- be a tau-type.)
    newTyFlexiVarTy argTypeKind		`thenM` \ ip_ty ->
	-- argTypeKind: it can't be an unboxed tuple
    newIPDict (IPOccOrigin ip) ip ip_ty `thenM` \ (ip', inst) ->
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
tc_expr in_expr@(ExprWithTySig expr poly_ty) res_ty
 = addErrCtxt (exprCtxt in_expr)			$
   tcHsSigType ExprSigCtxt poly_ty			`thenM` \ sig_tc_ty ->
   tcThingWithSig sig_tc_ty (tcCheckRho expr) res_ty	`thenM` \ (co_fn, expr') ->
   returnM (co_fn <$> ExprWithTySigOut expr' poly_ty)

tc_expr (HsType ty) res_ty
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
tc_expr (HsPar expr)    res_ty  = tcMonoExpr expr res_ty	`thenM` \ expr' -> 
				  returnM (HsPar expr')
tc_expr (HsSCC lbl expr) res_ty = tcMonoExpr expr res_ty	`thenM` \ expr' ->
				  returnM (HsSCC lbl expr')
tc_expr (HsCoreAnn lbl expr) res_ty = tcMonoExpr expr res_ty `thenM` \ expr' ->  -- hdaume: core annotation
                                         returnM (HsCoreAnn lbl expr')

tc_expr (HsLit lit) res_ty  = tcLit lit res_ty

tc_expr (HsOverLit lit) res_ty  
  = zapExpectedType res_ty liftedTypeKind		`thenM` \ res_ty' ->
 	-- Overloaded literals must have liftedTypeKind, because
 	-- we're instantiating an overloaded function here,
 	-- whereas res_ty might be openTypeKind. This was a bug in 6.2.2
    newOverloadedLit (LiteralOrigin lit) lit res_ty'	`thenM` \ lit_expr ->
    returnM (unLoc lit_expr) 	-- ToDo: nasty unLoc

tc_expr (NegApp expr neg_name) res_ty
  = tc_expr (HsApp (nlHsVar neg_name) expr) res_ty
	-- ToDo: use tcSyntaxName

tc_expr (HsLam match) res_ty
  = tcMatchLambda match res_ty 		`thenM` \ match' ->
    returnM (HsLam match')

tc_expr (HsApp e1 e2) res_ty 
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

tc_expr in_expr@(SectionL arg1 op) res_ty
  = tcInferRho op				`thenM` \ (op', op_ty) ->
    unifyFunTys 2 op_ty {- two args -}		`thenM` \ ([arg1_ty, arg2_ty], op_res_ty) ->
    tcArg op (arg1, arg1_ty, 1)			`thenM` \ arg1' ->
    addErrCtxt (exprCtxt in_expr)		$
    tcSubExp res_ty (mkFunTy arg2_ty op_res_ty)	`thenM` \ co_fn ->
    returnM (co_fn <$> SectionL arg1' op')

-- Right sections, equivalent to \ x -> x op expr, or
--	\ x -> op x expr

tc_expr in_expr@(SectionR op arg2) res_ty
  = tcInferRho op				`thenM` \ (op', op_ty) ->
    unifyFunTys 2 op_ty {- two args -}		`thenM` \ ([arg1_ty, arg2_ty], op_res_ty) ->
    tcArg op (arg2, arg2_ty, 2)			`thenM` \ arg2' ->
    addErrCtxt (exprCtxt in_expr)		$
    tcSubExp res_ty (mkFunTy arg1_ty op_res_ty)	`thenM` \ co_fn ->
    returnM (co_fn <$> SectionR op' arg2')

-- equivalent to (op e1) e2:

tc_expr in_expr@(OpApp arg1 op fix arg2) res_ty
  = tcInferRho op				`thenM` \ (op', op_ty) ->
    unifyFunTys 2 op_ty {- two args -}		`thenM` \ ([arg1_ty, arg2_ty], op_res_ty) ->
    tcArg op (arg1, arg1_ty, 1)			`thenM` \ arg1' ->
    tcArg op (arg2, arg2_ty, 2)			`thenM` \ arg2' ->
    addErrCtxt (exprCtxt in_expr)		$
    tcSubExp res_ty op_res_ty			`thenM` \ co_fn ->
    returnM (OpApp arg1' op' fix arg2')
\end{code}

\begin{code}
tc_expr (HsLet binds (L loc expr)) res_ty
  = tcBindsAndThen
	glue
	binds 			-- Bindings to check
	(setSrcSpan loc $ tc_expr expr res_ty)
  where
    glue bind expr = HsLet [bind] (L loc expr)

tc_expr in_expr@(HsCase scrut matches) exp_ty
  =	-- We used to typecheck the case alternatives first.
	-- The case patterns tend to give good type info to use
	-- when typechecking the scrutinee.  For example
	--	case (map f) of
	--	  (x:xs) -> ...
	-- will report that map is applied to too few arguments
	--
	-- But now, in the GADT world, we need to typecheck the scrutinee
	-- first, to get type info that may be refined in the case alternatives
    addErrCtxt (caseScrutCtxt scrut)
	       (tcInferRho scrut)	`thenM`    \ (scrut', scrut_ty) ->

    addErrCtxt (caseCtxt in_expr)			$
    tcMatchesCase match_ctxt scrut_ty matches exp_ty	`thenM` \ matches' ->
    returnM (HsCase scrut' matches') 
 where
    match_ctxt = MC { mc_what = CaseAlt,
		      mc_body = tcMonoExpr }

tc_expr (HsIf pred b1 b2) res_ty
  = addErrCtxt (predCtxt pred) (
    tcCheckRho pred boolTy	)	`thenM`    \ pred' ->

    zapExpectedType res_ty openTypeKind	`thenM`    \ res_ty' ->
	-- C.f. the call to zapToType in TcMatches.tcMatches

    tcCheckRho b1 res_ty'		`thenM`    \ b1' ->
    tcCheckRho b2 res_ty'		`thenM`    \ b2' ->
    returnM (HsIf pred' b1' b2')

tc_expr (HsDo do_or_lc stmts method_names _) res_ty
  = zapExpectedType res_ty liftedTypeKind		`thenM` \ res_ty' ->
	-- All comprehensions yield a monotype of kind *
    tcDoStmts do_or_lc stmts method_names res_ty'	`thenM` \ (stmts', methods') ->
    returnM (HsDo do_or_lc stmts' methods' res_ty')

tc_expr in_expr@(ExplicitList _ exprs) res_ty	-- Non-empty list
  = zapToListTy res_ty                `thenM` \ elt_ty ->  
    mappM (tc_elt elt_ty) exprs	      `thenM` \ exprs' ->
    returnM (ExplicitList elt_ty exprs')
  where
    tc_elt elt_ty expr
      = addErrCtxt (listCtxt expr) $
	tcCheckRho expr elt_ty

tc_expr in_expr@(ExplicitPArr _ exprs) res_ty	-- maybe empty
  = do	{ [elt_ty] <- zapToTyConApp parrTyCon res_ty
	; exprs' <- mappM (tc_elt elt_ty) exprs	
	; return (ExplicitPArr elt_ty exprs') }
  where
    tc_elt elt_ty expr
      = addErrCtxt (parrCtxt expr) (tcCheckRho expr elt_ty)

tc_expr (ExplicitTuple exprs boxity) res_ty
  = do	{ arg_tys <- zapToTyConApp (tupleTyCon boxity (length exprs)) res_ty
	; exprs' <-  tcCheckRhos exprs arg_tys
	; return (ExplicitTuple exprs' boxity) }

tc_expr (HsProc pat cmd) res_ty
  = tcProc pat cmd res_ty			`thenM` \ (pat', cmd') ->
    returnM (HsProc pat' cmd')

tc_expr e@(HsArrApp _ _ _ _ _) _
  = failWithTc (vcat [ptext SLIT("The arrow command"), nest 2 (ppr e), 
                      ptext SLIT("was found where an expression was expected")])

tc_expr e@(HsArrForm _ _ _) _
  = failWithTc (vcat [ptext SLIT("The arrow command"), nest 2 (ppr e), 
                      ptext SLIT("was found where an expression was expected")])
\end{code}

%************************************************************************
%*									*
		Record construction and update
%*									*
%************************************************************************

\begin{code}
tc_expr expr@(RecordCon con@(L loc con_name) rbinds) res_ty
  = addErrCtxt (recordConCtxt expr)		$
    addLocM tcId con			`thenM` \ (con_expr, _, con_tau) ->
    let
	(_, record_ty)   = tcSplitFunTys con_tau
	(tycon, ty_args) = tcSplitTyConApp record_ty
    in
    ASSERT( isAlgTyCon tycon )
    zapExpectedTo res_ty record_ty      `thenM_`

	-- Check that the record bindings match the constructor
	-- con_name is syntactically constrained to be a data constructor
    tcLookupDataCon con_name		`thenM` \ data_con ->
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

    returnM (RecordConOut data_con (L loc con_expr) rbinds')

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

tc_expr expr@(RecordUpd record_expr rbinds) res_ty
  = addErrCtxt (recordUpdCtxt	expr)		$

	-- STEP 0
	-- Check that the field names are really field names
    ASSERT( notNull rbinds )
    let 
	field_names = map fst rbinds
    in
    mappM (tcLookupGlobalId.unLoc) field_names	`thenM` \ sel_ids ->
	-- The renamer has already checked that they
	-- are all in scope
    let
	bad_guys = [ setSrcSpan loc $ addErrTc (notSelector field_name) 
		   | (L loc field_name, sel_id) <- field_names `zip` sel_ids,
		     not (isRecordSelector sel_id) 	-- Excludes class ops
		   ]
    in
    checkM (null bad_guys) (sequenceM bad_guys `thenM_` failM)	`thenM_`
    
	-- STEP 1
	-- Figure out the tycon and data cons from the first field name
    let
		-- It's OK to use the non-tc splitters here (for a selector)
	sel_id : _   = sel_ids
	(tycon, _)   = recordSelectorFieldLabel sel_id	-- We've failed already if
	data_cons    = tyConDataCons tycon		-- it's not a field label
	tycon_tyvars = tyConTyVars tycon		-- The data cons use the same type vars
    in
    tcInstTyVars tycon_tyvars		`thenM` \ (_, result_inst_tys, inst_env) ->

	-- STEP 2
	-- Check that at least one constructor has all the named fields
	-- i.e. has an empty set of bad fields returned by badFields
    checkTc (any (null . badFields rbinds) data_cons)
	    (badFieldsUpd rbinds)	`thenM_`

	-- STEP 3
	-- Typecheck the update bindings.
	-- (Do this after checking for bad fields in case there's a field that
	--  doesn't match the constructor.)
    let
	result_record_ty = mkTyConApp tycon result_inst_tys
    in
    zapExpectedTo res_ty result_record_ty	`thenM_`
    tcRecordBinds tycon result_inst_tys rbinds	`thenM` \ rbinds' ->

	-- STEP 4
	-- Use the un-updated fields to find a vector of booleans saying
	-- which type arguments must be the same in updatee and result.
	--
	-- WARNING: this code assumes that all data_cons in a common tycon
	-- have FieldLabels abstracted over the same tyvars.
    let
	upd_field_lbls      = recBindFields rbinds
	con_field_lbls_s    = map dataConFieldLabels data_cons

		-- A constructor is only relevant to this process if
		-- it contains all the fields that are being updated
	relevant_field_lbls_s      = filter is_relevant con_field_lbls_s
	is_relevant con_field_lbls = all (`elem` con_field_lbls) upd_field_lbls

	non_upd_field_lbls  = concat relevant_field_lbls_s `minusList` upd_field_lbls
	common_tyvars       = tyVarsOfTypes [ty | (fld,ty,_) <- tyConFields tycon,
						  fld `elem` non_upd_field_lbls]
 	is_common_tv tv = tv `elemVarSet` common_tyvars

	mk_inst_ty tv result_inst_ty 
	  | is_common_tv tv = returnM result_inst_ty		-- Same as result type
	  | otherwise	    = newTyFlexiVarTy (tyVarKind tv)	-- Fresh type, of correct kind
    in
    zipWithM mk_inst_ty tycon_tyvars result_inst_tys	`thenM` \ inst_tys ->

	-- STEP 5
	-- Typecheck the expression to be updated
    let
	record_ty = mkTyConApp tycon inst_tys
    in
    tcCheckRho record_expr record_ty		`thenM` \ record_expr' ->

	-- STEP 6
	-- Figure out the LIE we need.  We have to generate some 
	-- dictionaries for the data type context, since we are going to
	-- do pattern matching over the data cons.
	--
	-- What dictionaries do we need?  
	-- We just take the context of the type constructor
    let
	theta' = substTheta inst_env (tyConStupidTheta tycon)
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
tc_expr (ArithSeqIn seq@(From expr)) res_ty
  = zapToListTy res_ty 				`thenM` \ elt_ty ->  
    tcCheckRho expr elt_ty		 	`thenM` \ expr' ->

    newMethodFromName (ArithSeqOrigin seq) 
		      elt_ty enumFromName	`thenM` \ enum_from ->

    returnM (ArithSeqOut (nlHsVar enum_from) (From expr'))

tc_expr in_expr@(ArithSeqIn seq@(FromThen expr1 expr2)) res_ty
  = addErrCtxt (arithSeqCtxt in_expr) $ 
    zapToListTy  res_ty         			`thenM`    \ elt_ty ->  
    tcCheckRho expr1 elt_ty				`thenM`    \ expr1' ->
    tcCheckRho expr2 elt_ty				`thenM`    \ expr2' ->
    newMethodFromName (ArithSeqOrigin seq) 
		      elt_ty enumFromThenName		`thenM` \ enum_from_then ->

    returnM (ArithSeqOut (nlHsVar enum_from_then) (FromThen expr1' expr2'))


tc_expr in_expr@(ArithSeqIn seq@(FromTo expr1 expr2)) res_ty
  = addErrCtxt (arithSeqCtxt in_expr) $
    zapToListTy  res_ty         			`thenM`    \ elt_ty ->  
    tcCheckRho expr1 elt_ty				`thenM`    \ expr1' ->
    tcCheckRho expr2 elt_ty				`thenM`    \ expr2' ->
    newMethodFromName (ArithSeqOrigin seq) 
	  	      elt_ty enumFromToName		`thenM` \ enum_from_to ->

    returnM (ArithSeqOut (nlHsVar enum_from_to) (FromTo expr1' expr2'))

tc_expr in_expr@(ArithSeqIn seq@(FromThenTo expr1 expr2 expr3)) res_ty
  = addErrCtxt  (arithSeqCtxt in_expr) $
    zapToListTy  res_ty         			`thenM`    \ elt_ty ->  
    tcCheckRho expr1 elt_ty				`thenM`    \ expr1' ->
    tcCheckRho expr2 elt_ty				`thenM`    \ expr2' ->
    tcCheckRho expr3 elt_ty				`thenM`    \ expr3' ->
    newMethodFromName (ArithSeqOrigin seq) 
		      elt_ty enumFromThenToName		`thenM` \ eft ->

    returnM (ArithSeqOut (nlHsVar eft) (FromThenTo expr1' expr2' expr3'))

tc_expr in_expr@(PArrSeqIn seq@(FromTo expr1 expr2)) res_ty
  = addErrCtxt (parrSeqCtxt in_expr) $
    zapToTyConApp parrTyCon res_ty     			`thenM`    \ [elt_ty] ->  
    tcCheckRho expr1 elt_ty				`thenM`    \ expr1' ->
    tcCheckRho expr2 elt_ty				`thenM`    \ expr2' ->
    newMethodFromName (PArrSeqOrigin seq) 
		      elt_ty enumFromToPName 		`thenM` \ enum_from_to ->

    returnM (PArrSeqOut (nlHsVar enum_from_to) (FromTo expr1' expr2'))

tc_expr in_expr@(PArrSeqIn seq@(FromThenTo expr1 expr2 expr3)) res_ty
  = addErrCtxt  (parrSeqCtxt in_expr) $
    zapToTyConApp parrTyCon res_ty     			`thenM`    \ [elt_ty] ->  
    tcCheckRho expr1 elt_ty				`thenM`    \ expr1' ->
    tcCheckRho expr2 elt_ty				`thenM`    \ expr2' ->
    tcCheckRho expr3 elt_ty				`thenM`    \ expr3' ->
    newMethodFromName (PArrSeqOrigin seq)
		      elt_ty enumFromThenToPName	`thenM` \ eft ->

    returnM (PArrSeqOut (nlHsVar eft) (FromThenTo expr1' expr2' expr3'))

tc_expr (PArrSeqIn _) _ 
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
tc_expr (HsSpliceE splice) res_ty = tcSpliceExpr splice res_ty
tc_expr (HsBracket brack)  res_ty = do	{ e <- tcBracket brack res_ty
					; return (unLoc e) }
#endif /* GHCI */
\end{code}


%************************************************************************
%*									*
		Catch-all
%*									*
%************************************************************************

\begin{code}
tc_expr other _ = pprPanic "tcMonoExpr" (ppr other)
\end{code}


%************************************************************************
%*									*
\subsection{@tcApp@ typchecks an application}
%*									*
%************************************************************************

\begin{code}

tcApp :: LHsExpr Name -> [LHsExpr Name]   	-- Function and args
      -> Expected TcRhoType	    		-- Expected result type of application
      -> TcM (HsExpr TcId)			-- Translated fun and args

tcApp (L _ (HsApp e1 e2)) args res_ty 
  = tcApp e1 (e2:args) res_ty		-- Accumulate the arguments

tcApp fun args res_ty
  = do	{ (fun', fun_tvs, fun_tau) <- tcFun fun		-- Type-check the function

	-- Extract its argument types
	; (expected_arg_tys, actual_res_ty)
	      <- addErrCtxt (wrongArgsCtxt "too many" fun args) $ do
		 { traceTc (text "tcApp" <+> (ppr fun $$ ppr fun_tau))
		 ; unifyFunTys (length args) fun_tau }


	; case res_ty of
	    Check _ -> do 	-- Connect to result type first
				-- See Note [Push result type in]
		{ co_fn    <- tcResult fun args res_ty actual_res_ty
		; the_app' <- tcArgs fun fun' args expected_arg_tys
		; traceTc (text "tcApp: check" <+> vcat [ppr fun <+> ppr args,
							 ppr the_app', ppr actual_res_ty])
		; returnM (co_fn <$> the_app') }

	    Infer _ -> do	-- Type check args first, then
				-- refine result type, then do tcResult
		{ the_app'       <- tcArgs fun fun' args expected_arg_tys
		; subst		 <- refineTyVars fun_tvs
		; let actual_res_ty' = substTy subst actual_res_ty
		; co_fn          <- tcResult fun args res_ty actual_res_ty'
		; traceTc (text "tcApp: infer" <+> vcat [ppr fun <+> ppr args, ppr the_app',
							 ppr actual_res_ty, ppr actual_res_ty'])
		; returnM (co_fn <$> the_app') }
	}

-- 	Note [Push result type in]
--
-- Unify with expected result before (was: after) type-checking the args
-- so that the info from res_ty (was: args) percolates to args (was actual_res_ty).
-- This is when we might detect a too-few args situation.
-- (One can think of cases when the opposite order would give
-- a better error message.)
-- [March 2003: I'm experimenting with putting this first.  Here's an 
--		example where it actually makes a real difference
--    class C t a b | t a -> b
--    instance C Char a Bool
--
--    data P t a = forall b. (C t a b) => MkP b
--    data Q t   = MkQ (forall a. P t a)

--    f1, f2 :: Q Char;
--    f1 = MkQ (MkP True)
--    f2 = MkQ (MkP True :: forall a. P Char a)
--
-- With the change, f1 will type-check, because the 'Char' info from
-- the signature is propagated into MkQ's argument. With the check
-- in the other order, the extra signature in f2 is reqd.]

----------------
tcFun :: LHsExpr Name -> TcM (LHsExpr TcId, [TcTyVar], TcRhoType)
-- Instantiate the function, returning the type variables used
-- If the function isn't simple, infer its type, and return no 
-- type variables
tcFun (L loc (HsVar f)) = setSrcSpan loc $ do
			  { (fun', tvs, fun_tau) <- tcId f
			  ; return (L loc fun', tvs, fun_tau) }
tcFun fun = do { (fun', fun_tau) <- tcInfer (tcMonoExpr fun)
	       ; return (fun', [], fun_tau) }

----------------
tcArgs :: LHsExpr Name				-- The function (for error messages)
       -> LHsExpr TcId				-- The function (to build into result)
       -> [LHsExpr Name] -> [TcSigmaType]	-- Actual arguments and expected arg types
       -> TcM (HsExpr TcId)			-- Resulting application

tcArgs fun fun' args expected_arg_tys
  = do 	{ args' <- mappM (tcArg fun) (zip3 args expected_arg_tys [1..])
	; return (unLoc (foldl mkHsApp fun' args')) }

tcArg :: LHsExpr Name				-- The function (for error messages)
       -> (LHsExpr Name, TcSigmaType, Int)	-- Actual argument and expected arg type
       -> TcM (LHsExpr TcId)			-- Resulting argument
tcArg fun (arg, ty, arg_no) = addErrCtxt (funAppCtxt fun arg arg_no)
				 	 (tcCheckSigma arg ty)

----------------
tcResult fun args res_ty actual_res_ty
  = addErrCtxtM (checkArgsCtxt fun args res_ty actual_res_ty)
		(tcSubExp res_ty actual_res_ty)

----------------
-- If an error happens we try to figure out whether the
-- function has been given too many or too few arguments,
-- and say so.
-- The ~(Check...) is because in the Infer case the tcSubExp 
-- definitely won't fail, so we can be certain we're in the Check branch
checkArgsCtxt fun args (Infer _) actual_res_ty tidy_env
  = return (tidy_env, ptext SLIT("Urk infer"))

checkArgsCtxt fun args (Check expected_res_ty) actual_res_ty tidy_env
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
tcId :: Name -> TcM (HsExpr TcId, [TcTyVar], TcRhoType)
	-- Return the type variables at which the function
	-- is instantiated, as well as the translated variable and its type

tcId id_name	-- Look up the Id and instantiate its type
  = tcLookup id_name	`thenM` \ thing ->
    case thing of {
    	AGlobal (ADataCon con)	-- Similar, but instantiate the stupid theta too
	  -> do { (expr, tvs, tau) <- instantiate (dataConWrapId con)
		; tcInstStupidTheta con (mkTyVarTys tvs)
		-- Remember to chuck in the constraints from the "silly context"
		; return (expr, tvs, tau) }

    ;	AGlobal (AnId id) -> instantiate id
		-- A global cannot possibly be ill-staged
		-- nor does it need the 'lifting' treatment

    ;	ATcId id th_level proc_level 
	  -> do	{ checkProcLevel id proc_level
		; tc_local_id id th_level }

	-- THis 
    ;	other -> failWithTc (ppr other <+> ptext SLIT("used where a value identifer was expected"))
    }
  where

#ifndef GHCI
    tc_local_id id th_bind_lvl			-- Non-TH case
	= instantiate id

#else /* GHCI and TH is on */
    tc_local_id id th_bind_lvl			-- TH case
	= 	-- Check for cross-stage lifting
    	  getStage				`thenM` \ use_stage -> 
	  case use_stage of
	      Brack use_lvl ps_var lie_var
		| use_lvl > th_bind_lvl 
		-> if isExternalName id_name then	
			-- Top-level identifiers in this module,
			-- (which have External Names)
		  	-- are just like the imported case:
			-- no need for the 'lifting' treatment
			-- E.g.  this is fine:
			--   f x = x
			--   g y = [| f 3 |]
			-- But we do need to put f into the keep-alive
			-- set, because after desugaring the code will
			-- only mention f's *name*, not f itself.
			keepAliveTc id_name	`thenM_` 
			instantiate id

		   else	-- Nested identifiers, such as 'x' in
			-- E.g. \x -> [| h x |]
	   		-- We must behave as if the reference to x was
			--	h $(lift x)	
			-- We use 'x' itself as the splice proxy, used by 
			-- the desugarer to stitch it all back together.
			-- If 'x' occurs many times we may get many identical
			-- bindings of the same splice proxy, but that doesn't
			-- matter, although it's a mite untidy.
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
	           writeMutVar ps_var ((id_name, nlHsApp (nlHsVar lift) (nlHsVar id)) : ps)	`thenM_`
	   
		   returnM (HsVar id, [], id_ty))

	      other -> 
		checkWellStaged (quotes (ppr id)) th_bind_lvl use_stage	`thenM_`
		instantiate id
#endif /* GHCI */

    instantiate :: TcId -> TcM (HsExpr TcId, [TcTyVar], TcRhoType)
    instantiate fun_id = loop (HsVar fun_id) [] (idType fun_id)

    loop (HsVar fun_id) tvs fun_ty
	| want_method_inst fun_ty
	= tcInstType fun_ty		`thenM` \ (tyvars, theta, tau) ->
	  newMethodWithGivenTy orig fun_id 
		(mkTyVarTys tyvars) theta tau	`thenM` \ meth_id ->
	  loop (HsVar meth_id) (tvs ++ tyvars) tau

    loop fun tvs fun_ty 
	| isSigmaTy fun_ty
	= tcInstCall orig fun_ty	`thenM` \ (inst_fn, new_tvs, tau) ->
	  loop (inst_fn <$> fun) (tvs ++ new_tvs) tau

	| otherwise
	= returnM (fun, tvs, fun_ty)

	-- 	Hack Alert (want_method_inst)!
	-- If 	f :: (%x :: T) => Int -> Int
	-- Then if we have two separate calls, (f 3, f 4), we cannot
	-- make a method constraint that then gets shared, thus:
	--	let m = f %x in (m 3, m 4)
	-- because that loses the linearity of the constraint.
	-- The simplest thing to do is never to construct a method constraint
	-- in the first place that has a linear implicit parameter in it.
    want_method_inst fun_ty 
	| opt_NoMethodSharing = False	
	| otherwise	      = case tcSplitSigmaTy fun_ty of
				  (_,[],_)    -> False 	-- Not overloaded
				  (_,theta,_) -> not (any isLinearPred theta)

    orig = OccurrenceOf id_name
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
	-> HsRecordBinds Name
	-> TcM (HsRecordBinds TcId)

tcRecordBinds tycon ty_args rbinds
  = mappM do_bind rbinds
  where
    tenv = zipTopTvSubst (tyConTyVars tycon) ty_args

    do_bind (L loc field_lbl, rhs)
      = addErrCtxt (fieldCtxt field_lbl)	$
	let
	    field_ty  = tyConFieldType tycon field_lbl
	    field_ty' = substTy tenv field_ty
	in
	tcCheckSigma rhs field_ty' 		`thenM` \ rhs' ->
        tcLookupId field_lbl			`thenM` \ sel_id ->
	ASSERT( isRecordSelector sel_id )
	returnM (L loc sel_id, rhs')

tyConFieldType :: TyCon -> FieldLabel -> Type
tyConFieldType tycon field_lbl
  = case [ty | (f,ty,_) <- tyConFields tycon, f == field_lbl] of
	(ty:other) -> ASSERT( null other) ty
		-- This lookup and assertion will surely succeed, because
		-- we check that the fields are indeed record selectors
		-- before calling tcRecordBinds

badFields rbinds data_con
  = filter (not . (`elem` field_names)) (recBindFields rbinds)
  where
    field_names = dataConFieldLabels data_con

checkMissingFields :: DataCon -> HsRecordBinds Name -> TcM ()
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
	  	 not (fl `elem` field_names_used)
	  ]
    missing_ns_fields
	= [ fl | (fl, str) <- field_info,
	  	 not (isMarkedStrict str),
	  	 not (fl `elem` field_names_used)
	  ]

    field_names_used = recBindFields rbinds
    field_labels     = dataConFieldLabels data_con

    field_info = zipEqual "missingFields"
			  field_labels
	  		  field_strs

    field_strs = dataConStrictMarks data_con
\end{code}

%************************************************************************
%*									*
\subsection{@tcCheckRhos@ typechecks a {\em list} of expressions}
%*									*
%************************************************************************

\begin{code}
tcCheckRhos :: [LHsExpr Name] -> [TcType] -> TcM [LHsExpr TcId]

tcCheckRhos [] [] = returnM []
tcCheckRhos (expr:exprs) (ty:tys)
 = tcCheckRho  expr  ty		`thenM` \ expr' ->
   tcCheckRhos exprs tys	`thenM` \ exprs' ->
   returnM (expr':exprs')
\end{code}


%************************************************************************
%*									*
\subsection{Literals}
%*									*
%************************************************************************

Overloaded literals.

\begin{code}
tcLit :: HsLit -> Expected TcRhoType -> TcM (HsExpr TcId)
tcLit lit res_ty 
  = zapExpectedTo res_ty (hsLitType lit)		`thenM_`
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

parrSeqCtxt expr
  = hang (ptext SLIT("In a parallel array sequence:")) 4 (ppr expr)

caseCtxt expr
  = hang (ptext SLIT("In the case expression:")) 4 (ppr expr)

caseScrutCtxt expr
  = hang (ptext SLIT("In the scrutinee of a case expression:")) 4 (ppr expr)

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

appCtxt fun args
  = ptext SLIT("In the application") <+> quotes (ppr the_app)
  where
    the_app = foldl mkHsApp fun args	-- Used in error messages

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

wrongArgsCtxt too_many_or_few fun args
  = hang (ptext SLIT("Probable cause:") <+> quotes (ppr fun)
		    <+> ptext SLIT("is applied to") <+> text too_many_or_few 
		    <+> ptext SLIT("arguments in the call"))
	 4 (parens (ppr the_app))
  where
    the_app = foldl mkHsApp fun args	-- Used in error messages

#ifdef GHCI
polySpliceErr :: Id -> SDoc
polySpliceErr id
  = ptext SLIT("Can't splice the polymorphic local variable") <+> quotes (ppr id)
#endif
\end{code}
