%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcPat]{Typechecking patterns}

\begin{code}
module TcPat ( tcPat, tcMonoPatBndr, tcSubPat,
	       badFieldCon, polyPatSig
  ) where

#include "HsVersions.h"

import HsSyn		( Pat(..), HsConDetails(..), HsLit(..), HsOverLit(..), HsExpr(..) )
import RnHsSyn		( RenamedPat )
import TcHsSyn		( TcPat, TcId, hsLitType,
			  mkCoercion, idCoercion, isIdCoercion,
			  (<$>), PatCoFn )

import TcRnMonad
import Inst		( InstOrigin(..),
			  newMethodFromName, newOverloadedLit, newDicts,
			  instToId, tcInstDataCon, tcSyntaxName
			)
import Id		( idType, mkLocalId, mkSysLocal )
import Name		( Name )
import FieldLabel	( fieldLabelName )
import TcEnv		( tcLookupClass, tcLookupDataCon, tcLookupId )
import TcMType 		( newTyVarTy, arityErr )
import TcType		( TcType, TcTyVar, TcSigmaType, 
			  mkClassPred, liftedTypeKind )
import TcUnify		( tcSubOff, Expected(..), readExpectedType, zapExpectedType, 
			  unifyTauTy, zapToListTy, zapToPArrTy, zapToTupleTy )  
import TcMonoType	( tcHsSigType, UserTypeCtxt(..) )

import TysWiredIn	( stringTy )
import CmdLineOpts	( opt_IrrefutableTuples )
import DataCon		( DataCon, dataConFieldLabels, dataConSourceArity )
import PrelNames	( eqStringName, eqName, geName, negateName, minusName, 
			  integralClassName )
import BasicTypes	( isBoxed )
import Bag
import Outputable
import FastString
\end{code}


%************************************************************************
%*									*
\subsection{Variable patterns}
%*									*
%************************************************************************

\begin{code}
type BinderChecker = Name -> Expected TcSigmaType -> TcM (PatCoFn, TcId)
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
  = zapExpectedType pat_ty	`thenM` \ pat_ty' ->
	-- If there are *no constraints* on the pattern type, we
	-- revert to good old H-M typechecking, making
	-- the type of the binder into an *ordinary* 
	-- type variable.  We find out if there are no constraints
	-- by seeing if we are given an "open hole" as our info.
	-- What we are trying to avoid here is giving a binder
	-- a type that is a 'hole'.  The only place holes should
	-- appear is as an argument to tcPat and tcExpr/tcMonoExpr.

    returnM (idCoercion, mkLocalId binder_name pat_ty')
\end{code}


%************************************************************************
%*									*
\subsection{Typechecking patterns}
%*									*
%************************************************************************

\begin{code}
tcPat :: BinderChecker
      -> RenamedPat

      -> Expected TcSigmaType	-- Expected type derived from the context
				--	In the case of a function with a rank-2 signature,
				--	this type might be a forall type.

      -> TcM (TcPat, 
		Bag TcTyVar,	-- TyVars bound by the pattern
					-- 	These are just the existentially-bound ones.
					--	Any tyvars bound by *type signatures* in the
					-- 	patterns are brought into scope before we begin.
		Bag (Name, TcId),	-- Ids bound by the pattern, along with the Name under
					--	which it occurs in the pattern
					-- 	The two aren't the same because we conjure up a new
					-- 	local name for each variable.
		[Inst])			-- Dicts or methods [see below] bound by the pattern
					-- 	from existential constructor patterns
\end{code}


%************************************************************************
%*									*
\subsection{Variables, wildcards, lazy pats, as-pats}
%*									*
%************************************************************************

\begin{code}
tcPat tc_bndr pat@(TypePat ty) pat_ty
  = failWithTc (badTypePat pat)

tcPat tc_bndr (VarPat name) pat_ty
  = tc_bndr name pat_ty				`thenM` \ (co_fn, bndr_id) ->
    returnM (co_fn <$> VarPat bndr_id, 
	      emptyBag, unitBag (name, bndr_id), [])

tcPat tc_bndr (LazyPat pat) pat_ty
  = tcPat tc_bndr pat pat_ty		`thenM` \ (pat', tvs, ids, lie_avail) ->
    returnM (LazyPat pat', tvs, ids, lie_avail)

tcPat tc_bndr pat_in@(AsPat name pat) pat_ty
  = tc_bndr name pat_ty				`thenM` \ (co_fn, bndr_id) ->
    tcPat tc_bndr pat (Check (idType bndr_id))	`thenM` \ (pat', tvs, ids, lie_avail) ->
	-- NB: if we have:
	--	\ (y@(x::forall a. a->a)) = e
	-- we'll fail.  The as-pattern infers a monotype for 'y', which then
	-- fails to unify with the polymorphic type for 'x'.  This could be
	-- fixed, but only with a bit more work.
    returnM (co_fn <$> (AsPat bndr_id pat'), 
	      tvs, (name, bndr_id) `consBag` ids, lie_avail)

tcPat tc_bndr (WildPat _) pat_ty
  = zapExpectedType pat_ty		`thenM` \ pat_ty' ->
	-- We might have an incoming 'hole' type variable; no annotation
	-- so zap it to a type.  Rather like tcMonoPatBndr.
    returnM (WildPat pat_ty', emptyBag, emptyBag, [])

tcPat tc_bndr (ParPat parend_pat) pat_ty
-- Leave the parens in, so that warnings from the
-- desugarer have parens in them
  = tcPat tc_bndr parend_pat pat_ty	`thenM` \ (pat', tvs, ids, lie_avail) ->
    returnM (ParPat pat', tvs, ids, lie_avail)

tcPat tc_bndr pat_in@(SigPatIn pat sig) pat_ty
  = addErrCtxt (patCtxt pat_in)	$
    tcHsSigType PatSigCtxt sig		`thenM` \ sig_ty ->
    tcSubPat sig_ty pat_ty		`thenM` \ co_fn ->
    tcPat tc_bndr pat (Check sig_ty)	`thenM` \ (pat', tvs, ids, lie_avail) ->
    returnM (co_fn <$> pat', tvs, ids, lie_avail)
\end{code}


%************************************************************************
%*									*
\subsection{Explicit lists, parallel arrays, and tuples}
%*									*
%************************************************************************

\begin{code}
tcPat tc_bndr pat_in@(ListPat pats _) pat_ty
  = addErrCtxt (patCtxt pat_in)		$
    zapToListTy pat_ty				`thenM` \ elem_ty ->
    tcPats tc_bndr pats (repeat elem_ty)	`thenM` \ (pats', tvs, ids, lie_avail) ->
    returnM (ListPat pats' elem_ty, tvs, ids, lie_avail)

tcPat tc_bndr pat_in@(PArrPat pats _) pat_ty
  = addErrCtxt (patCtxt pat_in)		$
    zapToPArrTy pat_ty				`thenM` \ elem_ty ->
    tcPats tc_bndr pats (repeat elem_ty)	`thenM` \ (pats', tvs, ids, lie_avail) ->
    returnM (PArrPat pats' elem_ty, tvs, ids, lie_avail)

tcPat tc_bndr pat_in@(TuplePat pats boxity) pat_ty
  = addErrCtxt (patCtxt pat_in)	$

    zapToTupleTy boxity arity pat_ty		`thenM` \ arg_tys ->
    tcPats tc_bndr pats arg_tys 		`thenM` \ (pats', tvs, ids, lie_avail) ->

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
    returnM (possibly_mangled_result, tvs, ids, lie_avail)
  where
    arity = length pats
\end{code}


%************************************************************************
%*									*
\subsection{Other constructors}
%*									*

%************************************************************************

\begin{code}
tcPat tc_bndr pat_in@(ConPatIn con_name arg_pats) pat_ty
  = addErrCtxt (patCtxt pat_in)			$

	-- Check that it's a constructor, and instantiate it
    tcLookupDataCon con_name			`thenM` \ data_con ->
    tcInstDataCon (PatOrigin pat_in) data_con	`thenM` \ (_, ex_dicts1, arg_tys, con_res_ty, ex_tvs) ->

	-- Check overall type matches.
	-- The pat_ty might be a for-all type, in which
	-- case we must instantiate to match
    tcSubPat con_res_ty pat_ty	 			`thenM` \ co_fn ->

	-- Check the argument patterns
    tcConStuff tc_bndr data_con arg_pats arg_tys	`thenM` \ (arg_pats', arg_tvs, arg_ids, ex_dicts2) ->

    returnM (co_fn <$> ConPatOut data_con arg_pats' con_res_ty ex_tvs (map instToId ex_dicts1),
	      listToBag ex_tvs `unionBags` arg_tvs,
	      arg_ids,
	      ex_dicts1 ++ ex_dicts2)
\end{code}


%************************************************************************
%*									*
\subsection{Literals}
%*									*
%************************************************************************

\begin{code}
tcPat tc_bndr pat@(LitPat lit@(HsString _)) pat_ty
  = zapExpectedType pat_ty		`thenM` \ pat_ty' ->
    unifyTauTy pat_ty' stringTy		`thenM_` 
    tcLookupId eqStringName		`thenM` \ eq_id ->
    returnM (NPatOut lit stringTy (HsVar eq_id `HsApp` HsLit lit), 
	    emptyBag, emptyBag, [])

tcPat tc_bndr (LitPat simple_lit) pat_ty
  = zapExpectedType pat_ty			`thenM` \ pat_ty' ->
    unifyTauTy pat_ty' (hsLitType simple_lit)	`thenM_` 
    returnM (LitPat simple_lit, emptyBag, emptyBag, [])

tcPat tc_bndr pat@(NPatIn over_lit mb_neg) pat_ty
  = zapExpectedType pat_ty			`thenM` \ pat_ty' ->
    newOverloadedLit origin over_lit pat_ty'	`thenM` \ pos_lit_expr ->
    newMethodFromName origin pat_ty' eqName	`thenM` \ eq ->
    (case mb_neg of
	Nothing  -> returnM pos_lit_expr	-- Positive literal
	Just neg -> 	-- Negative literal
			-- The 'negate' is re-mappable syntax
	    tcSyntaxName origin pat_ty' (negateName, HsVar neg)	`thenM` \ (_, neg_expr) ->
	    returnM (HsApp neg_expr pos_lit_expr)
    )								`thenM` \ lit_expr ->

    let
	-- The literal in an NPatIn is always positive...
	-- But in NPat, the literal is used to find identical patterns
	-- 	so we must negate the literal when necessary!
    	lit' = case (over_lit, mb_neg) of
 		 (HsIntegral i _,   Nothing) -> HsInteger i
 		 (HsIntegral i _,   Just _)  -> HsInteger (-i)
 		 (HsFractional f _, Nothing) -> HsRat f pat_ty'
 		 (HsFractional f _, Just _)  -> HsRat (-f) pat_ty'
    in
    returnM (NPatOut lit' pat_ty' (HsApp (HsVar eq) lit_expr),
	     emptyBag, emptyBag, [])
  where
    origin = PatOrigin pat
\end{code}

%************************************************************************
%*									*
\subsection{n+k patterns}
%*									*
%************************************************************************

\begin{code}
tcPat tc_bndr pat@(NPlusKPatIn name lit@(HsIntegral i _) minus_name) pat_ty
  = tc_bndr name pat_ty				 `thenM` \ (co_fn, bndr_id) ->
    let 
	pat_ty' = idType bndr_id
    in
    newOverloadedLit origin lit pat_ty'		 `thenM` \ over_lit_expr ->
    newMethodFromName origin pat_ty' geName	 `thenM` \ ge ->

	-- The '-' part is re-mappable syntax
    tcSyntaxName origin pat_ty' (minusName, HsVar minus_name)	`thenM` \ (_, minus_expr) ->

	-- The Report says that n+k patterns must be in Integral
	-- We may not want this when using re-mappable syntax, though (ToDo?)
    tcLookupClass integralClassName 			`thenM` \ icls ->
    newDicts origin [mkClassPred icls [pat_ty']]	`thenM` \ dicts ->
    extendLIEs dicts					`thenM_`
    
    returnM (NPlusKPatOut bndr_id i 
			   (SectionR (HsVar ge) over_lit_expr)
			   (SectionR minus_expr over_lit_expr),
	      emptyBag, unitBag (name, bndr_id), [])
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
tcPats :: BinderChecker			-- How to deal with variables
       -> [RenamedPat] -> [TcType]	-- Excess 'expected types' discarded
       -> TcM ([TcPat], 
		 Bag TcTyVar,
		 Bag (Name, TcId),	-- Ids bound by the pattern
		 [Inst])		-- Dicts bound by the pattern

tcPats tc_bndr [] tys = returnM ([], emptyBag, emptyBag, [])

tcPats tc_bndr (pat:pats) (ty:tys)
  = tcPat tc_bndr pat (Check ty)	`thenM` \ (pat',  tvs1, ids1, lie_avail1) ->
    tcPats tc_bndr pats tys 		`thenM` \ (pats', tvs2, ids2, lie_avail2) ->

    returnM (pat':pats', 
	      tvs1 `unionBags` tvs2, ids1 `unionBags` ids2, 
	      lie_avail1 ++ lie_avail2)
\end{code}


%************************************************************************
%*									*
\subsection{Constructor arguments}
%*									*
%************************************************************************

\begin{code}
tcConStuff tc_bndr data_con (PrefixCon arg_pats) arg_tys
  = 	-- Check correct arity
    checkTc (con_arity == no_of_args)
	    (arityErr "Constructor" data_con con_arity no_of_args)	`thenM_`

	-- Check arguments
    tcPats tc_bndr arg_pats arg_tys	`thenM` \ (arg_pats', tvs, ids, lie_avail) ->

    returnM (PrefixCon arg_pats', tvs, ids, lie_avail)
  where
    con_arity  = dataConSourceArity data_con
    no_of_args = length arg_pats

tcConStuff tc_bndr data_con (InfixCon p1 p2) arg_tys
  = 	-- Check correct arity
    checkTc (con_arity == 2)
	    (arityErr "Constructor" data_con con_arity 2)	`thenM_`

	-- Check arguments
    tcPat tc_bndr p1 (Check ty1)	`thenM` \ (p1', tvs1, ids1, lie_avail1) ->
    tcPat tc_bndr p2 (Check ty2)	`thenM` \ (p2', tvs2, ids2, lie_avail2) ->

    returnM (InfixCon p1' p2', 
	      tvs1 `unionBags` tvs2, ids1 `unionBags` ids2, 
	      lie_avail1 ++ lie_avail2)
  where
    con_arity  = dataConSourceArity data_con
    [ty1, ty2] = arg_tys

tcConStuff tc_bndr data_con (RecCon rpats) arg_tys
  = 	-- Check the fields
    tc_fields field_tys rpats	`thenM` \ (rpats', tvs, ids, lie_avail) ->
    returnM (RecCon rpats', tvs, ids, lie_avail)

  where
    field_tys = zip (map fieldLabelName (dataConFieldLabels data_con)) arg_tys
	-- Don't use zipEqual! If the constructor isn't really a record, then
	-- dataConFieldLabels will be empty (and each field in the pattern
	-- will generate an error below).

    tc_fields field_tys []
      = returnM ([], emptyBag, emptyBag, [])

    tc_fields field_tys ((field_label, rhs_pat) : rpats)
      =	tc_fields field_tys rpats	`thenM` \ (rpats', tvs1, ids1, lie_avail1) ->

	(case [ty | (f,ty) <- field_tys, f == field_label] of

		-- No matching field; chances are this field label comes from some
		-- other record type (or maybe none).  As well as reporting an
		-- error we still want to typecheck the pattern, principally to
		-- make sure that all the variables it binds are put into the
		-- environment, else the type checker crashes later:
		--	f (R { foo = (a,b) }) = a+b
		-- If foo isn't one of R's fields, we don't want to crash when
		-- typechecking the "a+b".
	   [] -> addErrTc (badFieldCon data_con field_label)	`thenM_` 
		 newTyVarTy liftedTypeKind			`thenM` \ bogus_ty ->
		 returnM (error "Bogus selector Id", bogus_ty)

		-- The normal case, when the field comes from the right constructor
	   (pat_ty : extras) -> 
		ASSERT( null extras )
		tcLookupId field_label			`thenM` \ sel_id ->
		returnM (sel_id, pat_ty)
	)						`thenM` \ (sel_id, pat_ty) ->

	tcPat tc_bndr rhs_pat (Check pat_ty)	`thenM` \ (rhs_pat', tvs2, ids2, lie_avail2) ->

	returnM ((sel_id, rhs_pat') : rpats',
		  tvs1 `unionBags` tvs2,
		  ids1 `unionBags` ids2,
		  lie_avail1 ++ lie_avail2)
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
tcSubPat :: TcSigmaType -> Expected TcSigmaType -> TcM PatCoFn

tcSubPat sig_ty exp_ty
 = tcSubOff sig_ty exp_ty		`thenM` \ co_fn ->
	-- co_fn is a coercion on *expressions*, and we
	-- need to make a coercion on *patterns*
   if isIdCoercion co_fn then
	returnM idCoercion
   else
   newUnique				`thenM` \ uniq ->
   readExpectedType exp_ty		`thenM` \ exp_ty' ->
   let
	arg_id  = mkSysLocal FSLIT("sub") uniq exp_ty'
	the_fn  = DictLam [arg_id] (co_fn <$> HsVar arg_id)
	pat_co_fn p = SigPatOut p exp_ty' the_fn
   in
   returnM (mkCoercion pat_co_fn)
\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

\begin{code}
patCtxt pat = hang (ptext SLIT("When checking the pattern:")) 
		 4 (ppr pat)

badFieldCon :: DataCon -> Name -> SDoc
badFieldCon con field
  = hsep [ptext SLIT("Constructor") <+> quotes (ppr con),
	  ptext SLIT("does not have field"), quotes (ppr field)]

polyPatSig :: TcType -> SDoc
polyPatSig sig_ty
  = hang (ptext SLIT("Illegal polymorphic type signature in pattern:"))
	 4 (ppr sig_ty)

badTypePat pat = ptext SLIT("Illegal type pattern") <+> ppr pat
\end{code}

