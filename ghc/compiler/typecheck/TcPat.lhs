%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcPat]{Typechecking patterns}

\begin{code}
module TcPat ( tcPat, tcVarPat, badFieldCon ) where

#include "HsVersions.h"

import HsSyn		( InPat(..), OutPat(..), HsLit(..), HsExpr(..), Sig(..) )
import RnHsSyn		( RenamedPat )
import TcHsSyn		( TcPat, TcIdBndr )

import TcMonad
import Inst		( Inst, OverloadedLit(..), InstOrigin(..),
			  emptyLIE, plusLIE, LIE,
			  newMethod, newMethodWithGivenTy, newOverloadedLit, 
			  newDicts, instToIdBndr
			)
import Name		( Name, getOccName, getSrcLoc )
import FieldLabel	( fieldLabelName )
import TcEnv		( TcIdOcc(..), tcLookupGlobalValue, 
			  tcLookupGlobalValueByKey, newLocalId, badCon
			)
import TcType 		( TcType, TcTyVar, tcInstTyVars )
import TcUnify 		( unifyTauTy, unifyListTy,
			  unifyTupleTy, unifyUnboxedTupleTy
			)

import Bag		( Bag )
import CmdLineOpts	( opt_IrrefutableTuples )
import DataCon		( DataCon, dataConSig, dataConFieldLabels, dataConSourceArity )
import Id		( Id, idType, isDataConId_maybe )
import Type		( Type, substFlexiTy, substFlexiTheta, mkTyConApp )
import TysPrim		( charPrimTy, intPrimTy, floatPrimTy,
			  doublePrimTy, addrPrimTy
			)
import TysWiredIn	( charTy, stringTy, intTy )
import SrcLoc		( SrcLoc )
import Unique		( eqClassOpKey, geClassOpKey, minusClassOpKey )
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
tcVarPat :: (Name -> Maybe (TcIdBndr s))	-- Info about signatures
         -> Name
         -> TcType s			-- Expected type
         -> TcM s (TcIdBndr s)		-- The monomorphic Id; this is put in the pattern itself

tcVarPat sig_fn binder_name pat_ty
 = case sig_fn binder_name of
	Nothing -> newLocalId (getOccName binder_name) pat_ty		`thenNF_Tc` \ bndr_id ->
		   returnTc bndr_id

	Just bndr_id -> tcAddSrcLoc (getSrcLoc binder_name) 		$
			unifyTauTy pat_ty (idType bndr_id) 		`thenTc_`
			returnTc bndr_id
\end{code}


%************************************************************************
%*									*
\subsection{Typechecking patterns}
%*									*
%************************************************************************

\begin{code}
tcPat :: (Name -> Maybe (TcIdBndr s))	-- Info about signatures
      -> RenamedPat
      -> TcType s			-- Expected type
      -> TcM s (TcPat s, 
		LIE s,			-- Required by n+k and literal pats
		Bag (TcTyVar s),	-- TyVars bound by the pattern
		Bag (Name, TcIdBndr s),	-- Ids bound by the pattern, along with the Name under
					--	which it occurs in the pattern
					-- 	The two aren't the same because we conjure up a new
					-- 	local name for each variable.
		LIE s)			-- Dicts or methods [see below] bound by the pattern
\end{code}


%************************************************************************
%*									*
\subsection{Variables, wildcards, lazy pats, as-pats}
%*									*
%************************************************************************

\begin{code}
tcPat sig_fn (VarPatIn name) pat_ty
  = tcVarPat sig_fn name pat_ty		`thenTc` \ bndr_id ->
    returnTc (VarPat (TcId bndr_id), emptyLIE, emptyBag, unitBag (name, bndr_id), emptyLIE)

tcPat sig_fn (LazyPatIn pat) pat_ty
  = tcPat sig_fn pat pat_ty		`thenTc` \ (pat', lie_req, tvs, ids, lie_avail) ->
    returnTc (LazyPat pat', lie_req, tvs, ids, lie_avail)

tcPat sig_fn pat_in@(AsPatIn name pat) pat_ty
  = tcVarPat sig_fn name pat_ty		`thenTc` \ bndr_id ->
    tcPat sig_fn pat pat_ty		`thenTc` \ (pat', lie_req, tvs, ids, lie_avail) ->
    tcAddErrCtxt (patCtxt pat_in) 	$
    returnTc (AsPat (TcId bndr_id) pat', lie_req, 
	      tvs, (name, bndr_id) `consBag` ids, 
	      lie_avail)

tcPat sig_fn WildPatIn pat_ty
  = returnTc (WildPat pat_ty, emptyLIE, emptyBag, emptyBag, emptyLIE)

tcPat sig_fn (NegPatIn pat) pat_ty
  = tcPat sig_fn (negate_lit pat) pat_ty
  where
    negate_lit (LitPatIn (HsInt  i)) = LitPatIn (HsInt  (-i))
    negate_lit (LitPatIn (HsFrac f)) = LitPatIn (HsFrac (-f))
    negate_lit _                     = panic "TcPat:negate_pat"

tcPat sig_fn (ParPatIn parend_pat) pat_ty
  = tcPat sig_fn parend_pat pat_ty
\end{code}

%************************************************************************
%*									*
\subsection{Explicit lists and tuples}
%*									*
%************************************************************************

\begin{code}
tcPat sig_fn pat_in@(ListPatIn pats) pat_ty
  = tcAddErrCtxt (patCtxt pat_in)		$
    unifyListTy pat_ty				`thenTc` \ elem_ty ->
    tcPats sig_fn pats (repeat elem_ty)		`thenTc` \ (pats', lie_req, tvs, ids, lie_avail) ->
    returnTc (ListPat elem_ty pats', lie_req, tvs, ids, lie_avail)

tcPat sig_fn pat_in@(TuplePatIn pats boxed) pat_ty
  = tcAddErrCtxt (patCtxt pat_in)	$

    (if boxed
     then unifyTupleTy        arity pat_ty
     else unifyUnboxedTupleTy arity pat_ty)	`thenTc` \ arg_tys ->

    tcPats sig_fn pats arg_tys 	 		`thenTc` \ (pats', lie_req, tvs, ids, lie_avail) ->

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
tcPat sig_fn pat@(ConPatIn name arg_pats) pat_ty
  = tcConPat sig_fn pat name arg_pats pat_ty

tcPat sig_fn pat@(ConOpPatIn pat1 op _ pat2) pat_ty
  = tcConPat sig_fn pat op [pat1, pat2] pat_ty
\end{code}


%************************************************************************
%*									*
\subsection{Records}
%*									*
%************************************************************************

\begin{code}
tcPat sig_fn pat@(RecPatIn name rpats) pat_ty
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
      | null matching_fields
      = addErrTc (badFieldCon name field_label)		`thenNF_Tc_`
	tc_fields field_tys rpats

      | otherwise
      = ASSERT( null extras )
	tc_fields field_tys rpats	`thenTc` \ (rpats', lie_req1, tvs1, ids1, lie_avail1) ->

	tcLookupGlobalValue field_label	`thenNF_Tc` \ sel_id ->
	tcPat sig_fn rhs_pat rhs_ty	`thenTc` \ (rhs_pat', lie_req2, tvs2, ids2, lie_avail2) ->

	returnTc ((sel_id, rhs_pat', pun_flag) : rpats',
		  lie_req1 `plusLIE` lie_req2,
		  tvs1 `unionBags` tvs2,
		  ids1 `unionBags` ids2,
		  lie_avail1 `plusLIE` lie_avail2)
      where
 	matching_fields   = [ty | (f,ty) <- field_tys, f == field_label]
	(rhs_ty : extras) = matching_fields
\end{code}

%************************************************************************
%*									*
\subsection{Non-overloaded literals}
%*									*
%************************************************************************

\begin{code}
tcPat sig_fn (LitPatIn lit@(HsChar _))       pat_ty = tcSimpleLitPat lit charTy       pat_ty
tcPat sig_fn (LitPatIn lit@(HsIntPrim _))    pat_ty = tcSimpleLitPat lit intPrimTy    pat_ty
tcPat sig_fn (LitPatIn lit@(HsCharPrim _))   pat_ty = tcSimpleLitPat lit charPrimTy   pat_ty
tcPat sig_fn (LitPatIn lit@(HsStringPrim _)) pat_ty = tcSimpleLitPat lit addrPrimTy   pat_ty
tcPat sig_fn (LitPatIn lit@(HsFloatPrim _))  pat_ty = tcSimpleLitPat lit floatPrimTy  pat_ty
tcPat sig_fn (LitPatIn lit@(HsDoublePrim _)) pat_ty = tcSimpleLitPat lit doublePrimTy pat_ty

tcPat sig_fn (LitPatIn lit@(HsLitLit s))     pat_ty = tcSimpleLitPat lit intTy pat_ty
	-- This one looks weird!
\end{code}

%************************************************************************
%*									*
\subsection{Overloaded patterns: int literals and \tr{n+k} patterns}
%*									*
%************************************************************************

\begin{code}
tcPat sig_fn pat@(LitPatIn lit@(HsString str)) pat_ty
  = unifyTauTy pat_ty stringTy			`thenTc_` 
    tcLookupGlobalValueByKey eqClassOpKey	`thenNF_Tc` \ sel_id ->
    newMethod (PatOrigin pat) 
	      (RealId sel_id) [stringTy]	`thenNF_Tc` \ (lie, eq_id) ->
    let
	comp_op = HsApp (HsVar eq_id) (HsLitOut lit stringTy)
    in
    returnTc (NPat lit stringTy comp_op, lie, emptyBag, emptyBag, emptyLIE)


tcPat sig_fn pat@(LitPatIn lit@(HsInt i)) pat_ty
  = tcOverloadedLitPat pat lit (OverloadedIntegral i) pat_ty

tcPat sig_fn pat@(LitPatIn lit@(HsFrac f)) pat_ty
  = tcOverloadedLitPat pat lit (OverloadedFractional f) pat_ty


tcPat sig_fn pat@(NPlusKPatIn name lit@(HsInt i)) pat_ty
  = tcVarPat sig_fn name pat_ty				`thenTc` \ bndr_id ->
    tcLookupGlobalValueByKey geClassOpKey		`thenNF_Tc` \ ge_sel_id ->
    tcLookupGlobalValueByKey minusClassOpKey		`thenNF_Tc` \ minus_sel_id ->

    newOverloadedLit origin
		     (OverloadedIntegral i) pat_ty	`thenNF_Tc` \ (over_lit_expr, lie1) ->

    newMethod origin (RealId ge_sel_id)    [pat_ty]	`thenNF_Tc` \ (lie2, ge_id) ->
    newMethod origin (RealId minus_sel_id) [pat_ty]	`thenNF_Tc` \ (lie3, minus_id) ->

    returnTc (NPlusKPat (TcId bndr_id) lit pat_ty
			(SectionR (HsVar ge_id) over_lit_expr)
			(SectionR (HsVar minus_id) over_lit_expr),
	      lie1 `plusLIE` lie2 `plusLIE` lie3,
	      emptyBag, unitBag (name, bndr_id), emptyLIE)
  where
    origin = PatOrigin pat

tcPat sig_fn (NPlusKPatIn pat other) pat_ty
  = panic "TcPat:NPlusKPat: not an HsInt literal"
\end{code}

%************************************************************************
%*									*
\subsection{Lists of patterns}
%*									*
%************************************************************************

Helper functions

\begin{code}
tcPats :: (Name -> Maybe (TcIdBndr s))	-- Info about signatures
       -> [RenamedPat] -> [TcType s]	-- Excess 'expected types' discarded
       -> TcM s ([TcPat s], 
		 LIE s,				-- Required by n+k and literal pats
		 Bag (TcTyVar s),
		 Bag (Name, TcIdBndr s),	-- Ids bound by the pattern
		 LIE s)				-- Dicts bound by the pattern

tcPats sig_fn [] tys = returnTc ([], emptyLIE, emptyBag, emptyBag, emptyLIE)

tcPats sig_fn (ty:tys) (pat:pats)
  = tcPat sig_fn ty pat		`thenTc` \ (pat',  lie_req1, tvs1, ids1, lie_avail1) ->
    tcPats sig_fn tys pats	`thenTc` \ (pats', lie_req2, tvs2, ids2, lie_avail2) ->

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
    tcLookupGlobalValueByKey eqClassOpKey		`thenNF_Tc` \ eq_sel_id ->
    newMethod origin (RealId eq_sel_id) [pat_ty]	`thenNF_Tc` \ (lie2, eq_id) ->

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
    tcLookupGlobalValue con_name		`thenNF_Tc` \ con_id ->
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
	ex_theta' = substFlexiTheta tenv ex_theta
	arg_tys'  = map (substFlexiTy tenv) arg_tys

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
tcConPat sig_fn pat con_name arg_pats pat_ty
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
    tcPats sig_fn arg_pats arg_tys'	`thenTc` \ (arg_pats', lie_req, tvs, ids, lie_avail2) ->

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
\end{code}

