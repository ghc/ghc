%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcPat]{Typechecking patterns}

\begin{code}
module TcPat ( tcPat, tcPatBndr_NoSigs, badFieldCon, polyPatSig ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcExpr )

import HsSyn		( InPat(..), OutPat(..), HsLit(..), HsExpr(..), Sig(..) )
import RnHsSyn		( RenamedPat )
import TcHsSyn		( TcPat, TcId )

import TcMonad
import Inst		( Inst, OverloadedLit(..), InstOrigin(..),
			  emptyLIE, plusLIE, LIE,
			  newMethod, newOverloadedLit, 
			  newDicts, instToIdBndr
			)
import Name		( Name, getOccName, getSrcLoc )
import FieldLabel	( fieldLabelName )
import TcEnv		( tcLookupValue, tcLookupClassByKey,
			  tcLookupValueByKey, newLocalId, badCon
			)
import TcType 		( TcType, TcTyVar, tcInstTyVars, newTyVarTy )
import TcMonoType	( tcHsType )
import TcUnify 		( unifyTauTy, unifyListTy,
			  unifyTupleTy, unifyUnboxedTupleTy
			)

import Bag		( Bag )
import CmdLineOpts	( opt_IrrefutableTuples )
import DataCon		( DataCon, dataConSig, dataConFieldLabels, 
			  dataConSourceArity
			)
import Id		( Id, idType, isDataConId_maybe )
import Type		( Type, isTauTy, mkTyConApp, boxedTypeKind )
import Subst		( substTy, substTheta )
import TysPrim		( charPrimTy, intPrimTy, floatPrimTy,
			  doublePrimTy, addrPrimTy
			)
import TysWiredIn	( charTy, stringTy, intTy )
import SrcLoc		( SrcLoc )
import Unique		( eqClassOpKey, geClassOpKey, minusClassOpKey,
			  cCallableClassKey
			)
import Bag
import Util		( zipEqual )
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{Variable patterns}
%*									*
%************************************************************************

\begin{code}
-- This is the right function to pass to tcPat when there are no signatures
tcPatBndr_NoSigs binder_name pat_ty
  =  	-- Need to make a new, monomorphic, Id
	-- The binder_name is already being used for the polymorphic Id
     newLocalId (getOccName binder_name) pat_ty loc	`thenNF_Tc` \ bndr_id ->
     returnTc bndr_id
 where
   loc = getSrcLoc binder_name
\end{code}


%************************************************************************
%*									*
\subsection{Typechecking patterns}
%*									*
%************************************************************************

\begin{code}
tcPat :: (Name -> TcType -> TcM s TcId)	-- How to construct a suitable (monomorphic)
					-- Id for variables found in the pattern
			         	-- The TcType is the expected type, see note below
      -> RenamedPat

      -> TcType		-- Expected type derived from the context
			--	In the case of a function with a rank-2 signature,
			--	this type might be a forall type.
			--	INVARIANT: if it is, the foralls will always be visible,
			--	not hidden inside a mutable type variable

      -> TcM s (TcPat, 
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
tcPat tc_bndr (VarPatIn name) pat_ty
  = tc_bndr name pat_ty		`thenTc` \ bndr_id ->
    returnTc (VarPat bndr_id, emptyLIE, emptyBag, unitBag (name, bndr_id), emptyLIE)

tcPat tc_bndr (LazyPatIn pat) pat_ty
  = tcPat tc_bndr pat pat_ty		`thenTc` \ (pat', lie_req, tvs, ids, lie_avail) ->
    returnTc (LazyPat pat', lie_req, tvs, ids, lie_avail)

tcPat tc_bndr pat_in@(AsPatIn name pat) pat_ty
  = tc_bndr name pat_ty			`thenTc` \ bndr_id ->
    tcPat tc_bndr pat pat_ty		`thenTc` \ (pat', lie_req, tvs, ids, lie_avail) ->
    tcAddErrCtxt (patCtxt pat_in) 	$
    returnTc (AsPat bndr_id pat', lie_req, 
	      tvs, (name, bndr_id) `consBag` ids, lie_avail)

tcPat tc_bndr WildPatIn pat_ty
  = returnTc (WildPat pat_ty, emptyLIE, emptyBag, emptyBag, emptyLIE)

tcPat tc_bndr (NegPatIn pat) pat_ty
  = tcPat tc_bndr (negate_lit pat) pat_ty
  where
    negate_lit (LitPatIn (HsInt  i))       = LitPatIn (HsInt  (-i))
    negate_lit (LitPatIn (HsIntPrim i))    = LitPatIn (HsIntPrim (-i))
    negate_lit (LitPatIn (HsFrac f))       = LitPatIn (HsFrac (-f))
    negate_lit (LitPatIn (HsFloatPrim f))  = LitPatIn (HsFloatPrim (-f))
    negate_lit (LitPatIn (HsDoublePrim f)) = LitPatIn (HsDoublePrim (-f))
    negate_lit _                           = panic "TcPat:negate_pat"

tcPat tc_bndr (ParPatIn parend_pat) pat_ty
  = tcPat tc_bndr parend_pat pat_ty

tcPat tc_bndr (SigPatIn pat sig) pat_ty
  = tcHsType sig					`thenTc` \ sig_ty ->

	-- Check that the signature isn't a polymorphic one, which
	-- we don't permit (at present, anyway)
    checkTc (isTauTy sig_ty) (polyPatSig sig_ty)	`thenTc_`

    unifyTauTy pat_ty sig_ty	`thenTc_`
    tcPat tc_bndr pat sig_ty
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

tcPat tc_bndr pat_in@(TuplePatIn pats boxed) pat_ty
  = tcAddErrCtxt (patCtxt pat_in)	$

    (if boxed
     then unifyTupleTy        arity pat_ty
     else unifyUnboxedTupleTy arity pat_ty)	`thenTc` \ arg_tys ->

    tcPats tc_bndr pats arg_tys 	 		`thenTc` \ (pats', lie_req, tvs, ids, lie_avail) ->

	-- possibly do the "make all tuple-pats irrefutable" test:
    let
	unmangled_result = TuplePat pats' boxed

	-- Under flag control turn a pattern (x,y,z) into ~(x,y,z)
	-- so that we can experiment with lazy tuple-matching.
	-- This is a pretty odd place to make the switch, but
	-- it was easy to do.

	possibly_mangled_result
	  | opt_IrrefutableTuples && boxed = LazyPat unmangled_result
	  | otherwise			   = unmangled_result
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
	field_tys = zipEqual "tcPat" 
			     (map fieldLabelName (dataConFieldLabels data_con))
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
		 newTyVarTy boxedTypeKind			`thenNF_Tc_` 
		 returnTc (error "Bogus selector Id", pat_ty)

		-- The normal case, when the field comes from the right constructor
	   (pat_ty : extras) -> 
		ASSERT( null extras )
		tcLookupValue field_label			`thenNF_Tc` \ sel_id ->
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
\subsection{Non-overloaded literals}
%*									*
%************************************************************************

\begin{code}
tcPat tc_bndr (LitPatIn lit@(HsChar _))       pat_ty = tcSimpleLitPat lit charTy       pat_ty
tcPat tc_bndr (LitPatIn lit@(HsIntPrim _))    pat_ty = tcSimpleLitPat lit intPrimTy    pat_ty
tcPat tc_bndr (LitPatIn lit@(HsCharPrim _))   pat_ty = tcSimpleLitPat lit charPrimTy   pat_ty
tcPat tc_bndr (LitPatIn lit@(HsStringPrim _)) pat_ty = tcSimpleLitPat lit addrPrimTy   pat_ty
tcPat tc_bndr (LitPatIn lit@(HsFloatPrim _))  pat_ty = tcSimpleLitPat lit floatPrimTy  pat_ty
tcPat tc_bndr (LitPatIn lit@(HsDoublePrim _)) pat_ty = tcSimpleLitPat lit doublePrimTy pat_ty

tcPat tc_bndr (LitPatIn lit@(HsLitLit s))     pat_ty 
	-- cf tcExpr on LitLits
  = tcLookupClassByKey cCallableClassKey		`thenNF_Tc` \ cCallableClass ->
    newDicts (LitLitOrigin (_UNPK_ s))
	     [(cCallableClass, [pat_ty])]		`thenNF_Tc` \ (dicts, _) ->
    returnTc (LitPat lit pat_ty, dicts, emptyBag, emptyBag, emptyLIE)
\end{code}

%************************************************************************
%*									*
\subsection{Overloaded patterns: int literals and \tr{n+k} patterns}
%*									*
%************************************************************************

\begin{code}
tcPat tc_bndr pat@(LitPatIn lit@(HsString str)) pat_ty
  = unifyTauTy pat_ty stringTy			`thenTc_` 
    tcLookupValueByKey eqClassOpKey		`thenNF_Tc` \ sel_id ->
    newMethod (PatOrigin pat) sel_id [stringTy]	`thenNF_Tc` \ (lie, eq_id) ->
    let
	comp_op = HsApp (HsVar eq_id) (HsLitOut lit stringTy)
    in
    returnTc (NPat lit stringTy comp_op, lie, emptyBag, emptyBag, emptyLIE)


tcPat tc_bndr pat@(LitPatIn lit@(HsInt i)) pat_ty
  = tcOverloadedLitPat pat lit (OverloadedIntegral i) pat_ty

tcPat tc_bndr pat@(LitPatIn lit@(HsFrac f)) pat_ty
  = tcOverloadedLitPat pat lit (OverloadedFractional f) pat_ty


tcPat tc_bndr pat@(NPlusKPatIn name lit@(HsInt i)) pat_ty
  = tc_bndr name pat_ty				`thenTc` \ bndr_id ->
    tcLookupValueByKey geClassOpKey		`thenNF_Tc` \ ge_sel_id ->
    tcLookupValueByKey minusClassOpKey		`thenNF_Tc` \ minus_sel_id ->

    newOverloadedLit origin
		     (OverloadedIntegral i) pat_ty	`thenNF_Tc` \ (over_lit_expr, lie1) ->

    newMethod origin ge_sel_id    [pat_ty]	`thenNF_Tc` \ (lie2, ge_id) ->
    newMethod origin minus_sel_id [pat_ty]	`thenNF_Tc` \ (lie3, minus_id) ->

    returnTc (NPlusKPat bndr_id lit pat_ty
			(SectionR (HsVar ge_id) over_lit_expr)
			(SectionR (HsVar minus_id) over_lit_expr),
	      lie1 `plusLIE` lie2 `plusLIE` lie3,
	      emptyBag, unitBag (name, bndr_id), emptyLIE)
  where
    origin = PatOrigin pat

tcPat tc_bndr (NPlusKPatIn pat other) pat_ty
  = panic "TcPat:NPlusKPat: not an HsInt literal"
\end{code}

%************************************************************************
%*									*
\subsection{Lists of patterns}
%*									*
%************************************************************************

Helper functions

\begin{code}
tcPats :: (Name -> TcType -> TcM s TcId)	-- How to deal with variables
       -> [RenamedPat] -> [TcType]		-- Excess 'expected types' discarded
       -> TcM s ([TcPat], 
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
tcSimpleLitPat lit lit_ty pat_ty
  = unifyTauTy pat_ty lit_ty	`thenTc_` 
    returnTc (LitPat lit lit_ty, emptyLIE, emptyBag, emptyBag, emptyLIE)


tcOverloadedLitPat pat lit over_lit pat_ty
  = newOverloadedLit (PatOrigin pat) over_lit pat_ty	`thenNF_Tc` \ (over_lit_expr, lie1) ->
    tcLookupValueByKey eqClassOpKey			`thenNF_Tc` \ eq_sel_id ->
    newMethod origin eq_sel_id [pat_ty]			`thenNF_Tc` \ (lie2, eq_id) ->

    returnTc (NPat lit pat_ty (HsApp (HsVar eq_id)
				     over_lit_expr),
	      lie1 `plusLIE` lie2,
	      emptyBag, emptyBag, emptyLIE)
  where
    origin = PatOrigin pat
\end{code}

------------------------------------------------------
\begin{code}
tcConstructor pat con_name pat_ty
  = 	-- Check that it's a constructor
    tcLookupValue con_name		`thenNF_Tc` \ con_id ->
    case isDataConId_maybe con_id of {
	Nothing -> failWithTc (badCon con_id);
 	Just data_con ->

	-- Instantiate it
    let 
	(tvs, theta, ex_tvs, ex_theta, arg_tys, tycon) = dataConSig data_con
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
    newDicts (PatOrigin pat) ex_theta'	`thenNF_Tc` \ (lie_avail, dicts) ->

	-- Check overall type matches
    unifyTauTy pat_ty result_ty		`thenTc_`

    returnTc (data_con, ex_tvs', dicts, lie_avail, arg_tys')
    }
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
\subsection{Errors and contexts}
%*									*
%************************************************************************

\begin{code}
patCtxt pat = hang (ptext SLIT("In the pattern:")) 
		 4 (ppr pat)

recordLabel field_label
  = hang (hcat [ptext SLIT("When matching record field"), ppr field_label])
	 4 (hcat [ptext SLIT("with its immediately enclosing constructor")])

recordRhs field_label pat
  = hang (ptext SLIT("In the record field pattern"))
	 4 (sep [ppr field_label, char '=', ppr pat])

badFieldCon :: Name -> Name -> SDoc
badFieldCon con field
  = hsep [ptext SLIT("Constructor") <+> quotes (ppr con),
	  ptext SLIT("does not have field"), quotes (ppr field)]

polyPatSig :: TcType -> SDoc
polyPatSig sig_ty
  = hang (ptext SLIT("Polymorphic type signature in pattern"))
	 4 (ppr sig_ty)
\end{code}

