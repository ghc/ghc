%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcPat]{Typechecking patterns}

\begin{code}
module TcPat ( tcPat, tcMonoPatBndr, tcSubPat,
	       badFieldCon, polyPatSig
  ) where

#include "HsVersions.h"

import HsSyn		( InPat(..), OutPat(..), HsLit(..), HsOverLit(..), HsExpr(..) )
import RnHsSyn		( RenamedPat )
import TcHsSyn		( TcPat, TcId, simpleHsLitTy )

import TcMonad
import Inst		( InstOrigin(..),
			  emptyLIE, plusLIE, LIE, mkLIE, unitLIE, instToId, isEmptyLIE,
			  newMethod, newOverloadedLit, newDicts
			)
import Id		( mkLocalId, mkSysLocal )
import Name		( Name )
import FieldLabel	( fieldLabelName )
import TcEnv		( tcLookupClass, tcLookupDataCon, tcLookupGlobalId, tcLookupId )
import TcMType 		( tcInstTyVars, newTyVarTy, getTcTyVar, putTcTyVar )
import TcType		( TcType, TcTyVar, TcSigmaType,
			  mkTyConApp, mkClassPred, liftedTypeKind, tcGetTyVar_maybe,
			  isHoleTyVar, openTypeKind )
import TcUnify		( tcSub, unifyTauTy, unifyListTy, unifyTupleTy, 
			  mkCoercion, idCoercion, isIdCoercion, (<$>), PatCoFn )
import TcMonoType	( tcHsSigType, UserTypeCtxt(..) )

import TysWiredIn	( stringTy )
import CmdLineOpts	( opt_IrrefutableTuples )
import DataCon		( dataConSig, dataConFieldLabels, 
			  dataConSourceArity
			)
import Subst		( substTy, substTheta )
import PrelNames	( eqStringName, eqName, geName, cCallableClassName )
import BasicTypes	( isBoxed )
import Bag
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{Variable patterns}
%*									*
%************************************************************************

\begin{code}
type BinderChecker = Name -> TcSigmaType -> TcM (PatCoFn, LIE, TcId)
			-- How to construct a suitable (monomorphic)
			-- Id for variables found in the pattern
			-- The TcSigmaType is the expected type 
			-- from the pattern context

-- The Id may have a sigma type (e.g. f (x::forall a. a->a))
-- so we want to *create* it during pattern type checking.
-- We don't want to make Ids first with a type-variable type
-- and then unify... becuase we can't unify a sigma type with a type variable.

tcMonoPatBndr :: BinderChecker
  -- This is the right function to pass to tcPat when 
  -- we're looking at a lambda-bound pattern, 
  -- so there's no polymorphic guy to worry about

tcMonoPatBndr binder_name pat_ty 
  | Just tv <- tcGetTyVar_maybe pat_ty,
    isHoleTyVar tv
	-- If there are *no constraints* on the pattern type, we
	-- revert to good old H-M typechecking, making
	-- the type of the binder into an *ordinary* 
	-- type variable.  We find out if there are no constraints
	-- by seeing if we are given an "open hole" as our info.
	-- What we are trying to avoid here is giving a binder
	-- a type that is a 'hole'.  The only place holes should
	-- appear is as an argument to tcPat and tcExpr/tcMonoExpr.
  = getTcTyVar tv	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty -> tcMonoPatBndr binder_name ty
	Nothing -> newTyVarTy openTypeKind	`thenNF_Tc` \ ty ->
		   putTcTyVar tv ty		`thenNF_Tc_`
		   returnTc (idCoercion, emptyLIE, mkLocalId binder_name ty)
  | otherwise
  = returnTc (idCoercion, emptyLIE, mkLocalId binder_name pat_ty)
\end{code}


%************************************************************************
%*									*
\subsection{Typechecking patterns}
%*									*
%************************************************************************

\begin{code}
tcPat :: BinderChecker
      -> RenamedPat

      -> TcSigmaType	-- Expected type derived from the context
			--	In the case of a function with a rank-2 signature,
			--	this type might be a forall type.

      -> TcM (TcPat, 
		LIE,			-- Required by n+k and literal pats
		Bag TcTyVar,	-- TyVars bound by the pattern
					-- 	These are just the existentially-bound ones.
					--	Any tyvars bound by *type signatures* in the
					-- 	patterns are brought into scope before we begin.
		Bag (Name, TcId),	-- Ids bound by the pattern, along with the Name under
					--	which it occurs in the pattern
					-- 	The two aren't the same because we conjure up a new
					-- 	local name for each variable.
		LIE)			-- Dicts or methods [see below] bound by the pattern
					-- 	from existential constructor patterns
\end{code}


%************************************************************************
%*									*
\subsection{Variables, wildcards, lazy pats, as-pats}
%*									*
%************************************************************************

\begin{code}
tcPat tc_bndr pat@(TypePatIn ty) pat_ty
  = failWithTc (badTypePat pat)

tcPat tc_bndr (VarPatIn name) pat_ty
  = tc_bndr name pat_ty				`thenTc` \ (co_fn, lie_req, bndr_id) ->
    returnTc (co_fn <$> VarPat bndr_id, lie_req,
	      emptyBag, unitBag (name, bndr_id), emptyLIE)

tcPat tc_bndr (LazyPatIn pat) pat_ty
  = tcPat tc_bndr pat pat_ty		`thenTc` \ (pat', lie_req, tvs, ids, lie_avail) ->
    returnTc (LazyPat pat', lie_req, tvs, ids, lie_avail)

tcPat tc_bndr pat_in@(AsPatIn name pat) pat_ty
  = tc_bndr name pat_ty			`thenTc` \ (co_fn, lie_req1, bndr_id) ->
    tcPat tc_bndr pat pat_ty		`thenTc` \ (pat', lie_req2, tvs, ids, lie_avail) ->
    returnTc (co_fn <$> (AsPat bndr_id pat'), lie_req1 `plusLIE` lie_req2, 
	      tvs, (name, bndr_id) `consBag` ids, lie_avail)

tcPat tc_bndr WildPatIn pat_ty
  = returnTc (WildPat pat_ty, emptyLIE, emptyBag, emptyBag, emptyLIE)

tcPat tc_bndr (ParPatIn parend_pat) pat_ty
  = tcPat tc_bndr parend_pat pat_ty

tcPat tc_bndr (SigPatIn pat sig) pat_ty
  = tcHsSigType PatSigCtxt sig		`thenTc` \ sig_ty ->
    tcSubPat sig_ty pat_ty		`thenTc` \ (co_fn, lie_sig) ->
    tcPat tc_bndr pat sig_ty		`thenTc` \ (pat', lie_req, tvs, ids, lie_avail) ->
    returnTc (co_fn <$> pat', lie_req `plusLIE` lie_sig, tvs, ids, lie_avail)
\end{code}


%************************************************************************
%*									*
\subsection{Explicit lists and tuples}
%*									*
%************************************************************************

\begin{code}
tcPat tc_bndr pat_in@(ListPatIn pats) pat_ty
  = tcAddErrCtxt (patCtxt pat_in)		$
    unifyListTy pat_ty				`thenTc` \ elem_ty ->
    tcPats tc_bndr pats (repeat elem_ty)	`thenTc` \ (pats', lie_req, tvs, ids, lie_avail) ->
    returnTc (ListPat elem_ty pats', lie_req, tvs, ids, lie_avail)

tcPat tc_bndr pat_in@(TuplePatIn pats boxity) pat_ty
  = tcAddErrCtxt (patCtxt pat_in)	$

    unifyTupleTy boxity arity pat_ty		`thenTc` \ arg_tys ->
    tcPats tc_bndr pats arg_tys 		`thenTc` \ (pats', lie_req, tvs, ids, lie_avail) ->

	-- possibly do the "make all tuple-pats irrefutable" test:
    let
	unmangled_result = TuplePat pats' boxity

	-- Under flag control turn a pattern (x,y,z) into ~(x,y,z)
	-- so that we can experiment with lazy tuple-matching.
	-- This is a pretty odd place to make the switch, but
	-- it was easy to do.

	possibly_mangled_result
	  | opt_IrrefutableTuples && isBoxed boxity = LazyPat unmangled_result
	  | otherwise			   	    = unmangled_result
    in
    returnTc (possibly_mangled_result, lie_req, tvs, ids, lie_avail)
  where
    arity = length pats
\end{code}


%************************************************************************
%*									*
\subsection{Other constructors}
%*									*

%************************************************************************

\begin{code}
tcPat tc_bndr pat@(ConPatIn name arg_pats) pat_ty
  = tcConPat tc_bndr pat name arg_pats pat_ty

tcPat tc_bndr pat@(ConOpPatIn pat1 op _ pat2) pat_ty
  = tcConPat tc_bndr pat op [pat1, pat2] pat_ty
\end{code}


%************************************************************************
%*									*
\subsection{Records}
%*									*
%************************************************************************

\begin{code}
tcPat tc_bndr pat@(RecPatIn name rpats) pat_ty
  = tcAddErrCtxt (patCtxt pat)	$

 	-- Check the constructor itself
    tcConstructor pat name pat_ty	`thenTc` \ (data_con, ex_tvs, dicts, lie_avail1, arg_tys) ->
    let
	-- Don't use zipEqual! If the constructor isn't really a record, then
	-- dataConFieldLabels will be empty (and each field in the pattern
	-- will generate an error below).
	field_tys = zip (map fieldLabelName (dataConFieldLabels data_con))
			arg_tys
    in

	-- Check the fields
    tc_fields field_tys rpats		`thenTc` \ (rpats', lie_req, tvs, ids, lie_avail2) ->

    returnTc (RecPat data_con pat_ty ex_tvs dicts rpats',
	      lie_req,
	      listToBag ex_tvs `unionBags` tvs,
	      ids,
	      lie_avail1 `plusLIE` lie_avail2)

  where
    tc_fields field_tys []
      = returnTc ([], emptyLIE, emptyBag, emptyBag, emptyLIE)

    tc_fields field_tys ((field_label, rhs_pat, pun_flag) : rpats)
      =	tc_fields field_tys rpats	`thenTc` \ (rpats', lie_req1, tvs1, ids1, lie_avail1) ->

	(case [ty | (f,ty) <- field_tys, f == field_label] of

		-- No matching field; chances are this field label comes from some
		-- other record type (or maybe none).  As well as reporting an
		-- error we still want to typecheck the pattern, principally to
		-- make sure that all the variables it binds are put into the
		-- environment, else the type checker crashes later:
		--	f (R { foo = (a,b) }) = a+b
		-- If foo isn't one of R's fields, we don't want to crash when
		-- typechecking the "a+b".
	   [] -> addErrTc (badFieldCon name field_label)	`thenNF_Tc_` 
		 newTyVarTy liftedTypeKind			`thenNF_Tc_` 
		 returnTc (error "Bogus selector Id", pat_ty)

		-- The normal case, when the field comes from the right constructor
	   (pat_ty : extras) -> 
		ASSERT( null extras )
		tcLookupGlobalId field_label			`thenNF_Tc` \ sel_id ->
		returnTc (sel_id, pat_ty)
	)							`thenTc` \ (sel_id, pat_ty) ->

	tcPat tc_bndr rhs_pat pat_ty	`thenTc` \ (rhs_pat', lie_req2, tvs2, ids2, lie_avail2) ->

	returnTc ((sel_id, rhs_pat', pun_flag) : rpats',
		  lie_req1 `plusLIE` lie_req2,
		  tvs1 `unionBags` tvs2,
		  ids1 `unionBags` ids2,
		  lie_avail1 `plusLIE` lie_avail2)
\end{code}

%************************************************************************
%*									*
\subsection{Literals}
%*									*
%************************************************************************

\begin{code}
tcPat tc_bndr (LitPatIn lit@(HsLitLit s _)) pat_ty 
	-- cf tcExpr on LitLits
  = tcLookupClass cCallableClassName		`thenNF_Tc` \ cCallableClass ->
    newDicts (LitLitOrigin (_UNPK_ s))
	     [mkClassPred cCallableClass [pat_ty]]	`thenNF_Tc` \ dicts ->
    returnTc (LitPat (HsLitLit s pat_ty) pat_ty, mkLIE dicts, emptyBag, emptyBag, emptyLIE)

tcPat tc_bndr pat@(LitPatIn lit@(HsString _)) pat_ty
  = unifyTauTy pat_ty stringTy			`thenTc_` 
    tcLookupGlobalId eqStringName		`thenNF_Tc` \ eq_id ->
    returnTc (NPat lit stringTy (HsVar eq_id `HsApp` HsLit lit), 
	      emptyLIE, emptyBag, emptyBag, emptyLIE)

tcPat tc_bndr (LitPatIn simple_lit) pat_ty
  = unifyTauTy pat_ty (simpleHsLitTy simple_lit)		`thenTc_` 
    returnTc (LitPat simple_lit pat_ty, emptyLIE, emptyBag, emptyBag, emptyLIE)

tcPat tc_bndr pat@(NPatIn over_lit) pat_ty
  = newOverloadedLit (PatOrigin pat) over_lit pat_ty	`thenNF_Tc` \ (over_lit_expr, lie1) ->
    tcLookupGlobalId eqName				`thenNF_Tc` \ eq_sel_id ->
    newMethod origin eq_sel_id [pat_ty]			`thenNF_Tc` \ eq ->

    returnTc (NPat lit' pat_ty (HsApp (HsVar (instToId eq)) over_lit_expr),
	      lie1 `plusLIE` unitLIE eq,
	      emptyBag, emptyBag, emptyLIE)
  where
    origin = PatOrigin pat
    lit' = case over_lit of
		HsIntegral i _   -> HsInteger i
		HsFractional f _ -> HsRat f pat_ty
\end{code}

%************************************************************************
%*									*
\subsection{n+k patterns}
%*									*
%************************************************************************

\begin{code}
tcPat tc_bndr pat@(NPlusKPatIn name lit@(HsIntegral i _) minus_name) pat_ty
  = tc_bndr name pat_ty				`thenTc` \ (co_fn, lie1, bndr_id) ->
	-- The '-' part is re-mappable syntax
    tcLookupId minus_name			`thenNF_Tc` \ minus_sel_id ->
    tcLookupGlobalId geName			`thenNF_Tc` \ ge_sel_id ->
    newOverloadedLit origin lit pat_ty		`thenNF_Tc` \ (over_lit_expr, lie2) ->
    newMethod origin ge_sel_id    [pat_ty]	`thenNF_Tc` \ ge ->
    newMethod origin minus_sel_id [pat_ty]	`thenNF_Tc` \ minus ->

    returnTc (NPlusKPat bndr_id i pat_ty
			(SectionR (HsVar (instToId ge)) over_lit_expr)
			(SectionR (HsVar (instToId minus)) over_lit_expr),
	      lie1 `plusLIE` lie2 `plusLIE` mkLIE [ge,minus],
	      emptyBag, unitBag (name, bndr_id), emptyLIE)
  where
    origin = PatOrigin pat
\end{code}

%************************************************************************
%*									*
\subsection{Lists of patterns}
%*									*
%************************************************************************

Helper functions

\begin{code}
tcPats :: BinderChecker				-- How to deal with variables
       -> [RenamedPat] -> [TcType]		-- Excess 'expected types' discarded
       -> TcM ([TcPat], 
		 LIE,				-- Required by n+k and literal pats
		 Bag TcTyVar,
		 Bag (Name, TcId),	-- Ids bound by the pattern
		 LIE)				-- Dicts bound by the pattern

tcPats tc_bndr [] tys = returnTc ([], emptyLIE, emptyBag, emptyBag, emptyLIE)

tcPats tc_bndr (ty:tys) (pat:pats)
  = tcPat tc_bndr ty pat		`thenTc` \ (pat',  lie_req1, tvs1, ids1, lie_avail1) ->
    tcPats tc_bndr tys pats	`thenTc` \ (pats', lie_req2, tvs2, ids2, lie_avail2) ->

    returnTc (pat':pats', lie_req1 `plusLIE` lie_req2,
	      tvs1 `unionBags` tvs2, ids1 `unionBags` ids2, 
	      lie_avail1 `plusLIE` lie_avail2)
\end{code}

------------------------------------------------------
\begin{code}
tcConstructor pat con_name pat_ty
  = 	-- Check that it's a constructor
    tcLookupDataCon con_name		`thenNF_Tc` \ data_con ->

	-- Instantiate it
    let 
	(tvs, _, ex_tvs, ex_theta, arg_tys, tycon) = dataConSig data_con
	     -- Ignore the theta; overloaded constructors only
	     -- behave differently when called, not when used for
	     -- matching.
    in
    tcInstTyVars (ex_tvs ++ tvs)	`thenNF_Tc` \ (all_tvs', ty_args', tenv) ->
    let
	ex_theta' = substTheta tenv ex_theta
	arg_tys'  = map (substTy tenv) arg_tys

	n_ex_tvs  = length ex_tvs
	ex_tvs'   = take n_ex_tvs all_tvs'
	result_ty = mkTyConApp tycon (drop n_ex_tvs ty_args')
    in
    newDicts (PatOrigin pat) ex_theta'	`thenNF_Tc` \ dicts ->

	-- Check overall type matches
    unifyTauTy pat_ty result_ty		`thenTc_`

    returnTc (data_con, ex_tvs', map instToId dicts, mkLIE dicts, arg_tys')
\end{code}	      

------------------------------------------------------
\begin{code}
tcConPat tc_bndr pat con_name arg_pats pat_ty
  = tcAddErrCtxt (patCtxt pat)	$

	-- Check the constructor itself
    tcConstructor pat con_name pat_ty	`thenTc` \ (data_con, ex_tvs', dicts, lie_avail1, arg_tys') ->

	-- Check correct arity
    let
	con_arity  = dataConSourceArity data_con
	no_of_args = length arg_pats
    in
    checkTc (con_arity == no_of_args)
	    (arityErr "Constructor" data_con con_arity no_of_args)	`thenTc_`

	-- Check arguments
    tcPats tc_bndr arg_pats arg_tys'	`thenTc` \ (arg_pats', lie_req, tvs, ids, lie_avail2) ->

    returnTc (ConPat data_con pat_ty ex_tvs' dicts arg_pats',
	      lie_req,
	      listToBag ex_tvs' `unionBags` tvs,
	      ids,
	      lie_avail1 `plusLIE` lie_avail2)
\end{code}


%************************************************************************
%*									*
\subsection{Subsumption}
%*									*
%************************************************************************

Example:  
	f :: (forall a. a->a) -> Int -> Int
	f (g::Int->Int) y = g y
This is ok: the type signature allows fewer callers than
the (more general) signature f :: (Int->Int) -> Int -> Int
I.e.    (forall a. a->a) <= Int -> Int
We end up translating this to:
	f = \g' :: (forall a. a->a).  let g = g' Int in g' y

tcSubPat does the work
 	sig_ty is the signature on the pattern itself 
		(Int->Int in the example)
	expected_ty is the type passed inwards from the context
		(forall a. a->a in the example)

\begin{code}
tcSubPat :: TcSigmaType -> TcSigmaType -> TcM (PatCoFn, LIE)

tcSubPat sig_ty exp_ty
 = tcSub exp_ty sig_ty			`thenTc` \ (co_fn, lie) ->
	-- co_fn is a coercion on *expressions*, and we
	-- need to make a coercion on *patterns*
   if isIdCoercion co_fn then
	ASSERT( isEmptyLIE lie )
	returnNF_Tc (idCoercion, emptyLIE)
   else
   tcGetUnique				`thenNF_Tc` \ uniq ->
   let
	arg_id  = mkSysLocal SLIT("sub") uniq exp_ty
	the_fn  = DictLam [arg_id] (co_fn <$> HsVar arg_id)
	pat_co_fn p = SigPat p exp_ty the_fn
   in
   returnNF_Tc (mkCoercion pat_co_fn, lie)
\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

\begin{code}
patCtxt pat = hang (ptext SLIT("In the pattern:")) 
		 4 (ppr pat)

badFieldCon :: Name -> Name -> SDoc
badFieldCon con field
  = hsep [ptext SLIT("Constructor") <+> quotes (ppr con),
	  ptext SLIT("does not have field"), quotes (ppr field)]

polyPatSig :: TcType -> SDoc
polyPatSig sig_ty
  = hang (ptext SLIT("Illegal polymorphic type signature in pattern:"))
	 4 (ppr sig_ty)

badTypePat pat = ptext SLIT("Illegal type pattern") <+> ppr pat
\end{code}

