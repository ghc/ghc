%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcBinds]{TcBinds}

\begin{code}
#include "HsVersions.h"

module TcBinds ( tcBindsAndThen, tcPragmaSigs ) where

import Ubiq

import HsSyn		( HsBinds(..), Bind(..), Sig(..), MonoBinds(..), 
			  HsExpr, Match, PolyType, InPat, OutPat,
			  GRHSsAndBinds, ArithSeqInfo, HsLit, Fake,
			  collectBinders )
import RnHsSyn		( RenamedHsBinds(..), RenamedBind(..), RenamedSig(..), 
			  RenamedMonoBinds(..) )
import TcHsSyn		( TcHsBinds(..), TcBind(..), TcMonoBinds(..),
			  TcIdOcc(..), TcIdBndr(..) )

import TcMonad	
import GenSpecEtc	( checkSigTyVars, genBinds, TcSigInfo(..) )
import Inst		( Inst, LIE(..), emptyLIE, plusLIE, InstOrigin(..) )
import TcEnv		( tcExtendLocalValEnv, tcLookupLocalValueOK, newMonoIds )
import TcLoop		( tcGRHSsAndBinds )
import TcMatches	( tcMatchesFun )
import TcMonoType	( tcPolyType )
import TcPat		( tcPat )
import TcSimplify	( bindInstsOfLocalFuns )
import TcType		( newTcTyVar, tcInstType )
import Unify		( unifyTauTy )

import Kind		( mkBoxedTypeKind, mkTypeKind )
import Id		( GenId, idType, mkUserId )
import IdInfo		( noIdInfo )
import Name		( Name )	-- instances
import Maybes		( assocMaybe, catMaybes, Maybe(..) )
import Outputable	( pprNonOp )
import PragmaInfo	( PragmaInfo(..) )
import Pretty
import Type		( mkTyVarTy, mkTyVarTys, isTyVarTy,
			  mkSigmaTy, splitSigmaTy,
			  splitRhoTy, mkForAllTy, splitForAllTy )
import Util		( panic )
\end{code}

%************************************************************************
%*									*
\subsection{Type-checking bindings}
%*									*
%************************************************************************

@tcBindsAndThen@ typechecks a @HsBinds@.  The "and then" part is because
it needs to know something about the {\em usage} of the things bound,
so that it can create specialisations of them.  So @tcBindsAndThen@
takes a function which, given an extended environment, E, typechecks
the scope of the bindings returning a typechecked thing and (most
important) an LIE.  It is this LIE which is then used as the basis for
specialising the things bound.

@tcBindsAndThen@ also takes a "combiner" which glues together the
bindings and the "thing" to make a new "thing".

The real work is done by @tcBindAndThen@.

Recursive and non-recursive binds are handled in essentially the same
way: because of uniques there are no scoping issues left.  The only
difference is that non-recursive bindings can bind primitive values.

Even for non-recursive binding groups we add typings for each binder
to the LVE for the following reason.  When each individual binding is
checked the type of its LHS is unified with that of its RHS; and
type-checking the LHS of course requires that the binder is in scope.

At the top-level the LIE is sure to contain nothing but constant
dictionaries, which we resolve at the module level.

\begin{code}
tcBindsAndThen
	:: (TcHsBinds s -> thing -> thing)		-- Combinator
	-> RenamedHsBinds
	-> TcM s (thing, LIE s, thing_ty)
	-> TcM s (thing, LIE s, thing_ty)

tcBindsAndThen combiner EmptyBinds do_next
  = do_next 	`thenTc` \ (thing, lie, thing_ty) ->
    returnTc (combiner EmptyBinds thing, lie, thing_ty)

tcBindsAndThen combiner (SingleBind bind) do_next
  = tcBindAndThen combiner bind [] do_next

tcBindsAndThen combiner (BindWith bind sigs) do_next
  = tcBindAndThen combiner bind sigs do_next

tcBindsAndThen combiner (ThenBinds binds1 binds2) do_next
  = tcBindsAndThen combiner binds1 (tcBindsAndThen combiner binds2 do_next)
\end{code}

An aside.  The original version of @tcBindsAndThen@ which lacks a
combiner function, appears below.  Though it is perfectly well
behaved, it cannot be typed by Haskell, because the recursive call is
at a different type to the definition itself.  There aren't too many
examples of this, which is why I thought it worth preserving! [SLPJ]

\begin{pseudocode}
tcBindsAndThen
	:: RenamedHsBinds
	-> TcM s (thing, LIE s, thing_ty))
	-> TcM s ((TcHsBinds s, thing), LIE s, thing_ty)

tcBindsAndThen EmptyBinds do_next
  = do_next 		`thenTc` \ (thing, lie, thing_ty) ->
    returnTc ((EmptyBinds, thing), lie, thing_ty)

tcBindsAndThen (SingleBind bind) do_next
  = tcBindAndThen bind [] do_next

tcBindsAndThen (BindWith bind sigs) do_next
  = tcBindAndThen bind sigs do_next

tcBindsAndThen (ThenBinds binds1 binds2) do_next
  = tcBindsAndThen binds1 (tcBindsAndThen binds2 do_next)
	`thenTc` \ ((binds1', (binds2', thing')), lie1, thing_ty) ->

    returnTc ((binds1' `ThenBinds` binds2', thing'), lie1, thing_ty)
\end{pseudocode}

%************************************************************************
%*									*
\subsection{Bind}
%*									*
%************************************************************************

\begin{code}
tcBindAndThen
	:: (TcHsBinds s -> thing -> thing)		  -- Combinator
	-> RenamedBind					  -- The Bind to typecheck
	-> [RenamedSig]					  -- ...and its signatures
	-> TcM s (thing, LIE s, thing_ty)		  -- Thing to type check in
							  -- augmented envt
	-> TcM s (thing, LIE s, thing_ty)		  -- Results, incl the

tcBindAndThen combiner bind sigs do_next
  = fixTc (\ ~(prag_info_fn, _) ->
	-- This is the usual prag_info fix; the PragmaInfo field of an Id
	-- is not inspected till ages later in the compiler, so there
	-- should be no black-hole problems here.
    
    tcBindAndSigs binder_names bind 
		  sigs prag_info_fn	`thenTc` \ (poly_binds, poly_lie, poly_ids) ->

	-- Extend the environment to bind the new polymorphic Ids
    tcExtendLocalValEnv binder_names poly_ids $

	-- Build bindings and IdInfos corresponding to user pragmas
    tcPragmaSigs sigs			`thenTc` \ (prag_info_fn, prag_binds, prag_lie) ->

	-- Now do whatever happens next, in the augmented envt
    do_next				`thenTc` \ (thing, thing_lie, thing_ty) ->

	-- Create specialisations of functions bound here
    bindInstsOfLocalFuns (prag_lie `plusLIE` thing_lie)
			  poly_ids	`thenTc` \ (lie2, inst_mbinds) ->

	-- All done
    let
 	final_lie   = lie2 `plusLIE` poly_lie
	final_binds = poly_binds `ThenBinds`
		      SingleBind (NonRecBind inst_mbinds) `ThenBinds`
		      prag_binds
    in
    returnTc (prag_info_fn, (combiner final_binds thing, final_lie, thing_ty))
    )					`thenTc` \ (_, result) ->
    returnTc result
  where
    binder_names = collectBinders bind


tcBindAndSigs binder_names bind sigs prag_info_fn
  = recoverTc (
	-- If typechecking the binds fails, then return with each
	-- binder given type (forall a.a), to minimise subsequent
	-- error messages
	newTcTyVar Nothing mkBoxedTypeKind		`thenNF_Tc` \ alpha_tv ->
	let
	  forall_a_a = mkForAllTy alpha_tv (mkTyVarTy alpha_tv)
	  poly_ids   = [ mkUserId name forall_a_a (prag_info_fn name)
		       | name <- binder_names]
	in
	returnTc (EmptyBinds, emptyLIE, poly_ids)
    ) $

	-- Create a new identifier for each binder, with each being given
	-- a type-variable type.
    newMonoIds binder_names kind (\ mono_ids ->
	    tcTySigs sigs		`thenTc` \ sig_info ->
	    tc_bind bind		`thenTc` \ (bind', lie) ->
	    returnTc (mono_ids, bind', lie, sig_info)
    )
	    `thenTc` \ (mono_ids, bind', lie, sig_info) ->

	    -- Notice that genBinds gets the old (non-extended) environment
    genBinds binder_names mono_ids bind' lie sig_info prag_info_fn
  where
    kind = case bind of
	  	NonRecBind _ -> mkBoxedTypeKind	-- Recursive, so no unboxed types
		RecBind _    -> mkTypeKind	-- Non-recursive, so we permit unboxed types
\end{code}

\begin{code}
tc_bind :: RenamedBind -> TcM s (TcBind s, LIE s)

tc_bind (NonRecBind mono_binds)
  = tcMonoBinds mono_binds	`thenTc` \ (mono_binds2, lie) ->
    returnTc  (NonRecBind mono_binds2, lie)

tc_bind (RecBind mono_binds)
  = tcMonoBinds mono_binds	`thenTc` \ (mono_binds2, lie) ->
    returnTc  (RecBind mono_binds2, lie)
\end{code}

\begin{code}
tcMonoBinds :: RenamedMonoBinds -> TcM s (TcMonoBinds s, LIE s)

tcMonoBinds EmptyMonoBinds = returnTc (EmptyMonoBinds, emptyLIE)

tcMonoBinds (AndMonoBinds mb1 mb2)
  = tcMonoBinds mb1		`thenTc` \ (mb1a, lie1) ->
    tcMonoBinds mb2		`thenTc` \ (mb2a, lie2) ->
    returnTc (AndMonoBinds mb1a mb2a, lie1 `plusLIE` lie2)

tcMonoBinds bind@(PatMonoBind pat grhss_and_binds locn)
  = tcAddSrcLoc locn		 $

	-- LEFT HAND SIDE
    tcPat pat	     			`thenTc` \ (pat2, lie_pat, pat_ty) ->

	-- BINDINGS AND GRHSS
    tcGRHSsAndBinds grhss_and_binds	`thenTc` \ (grhss_and_binds2, lie, grhss_ty) ->

	-- Unify the two sides
    tcAddErrCtxt (patMonoBindsCtxt bind) $
	unifyTauTy pat_ty grhss_ty			`thenTc_`

	-- RETURN
    returnTc (PatMonoBind pat2 grhss_and_binds2 locn,
	      plusLIE lie_pat lie)

tcMonoBinds (FunMonoBind name matches locn)
  = tcAddSrcLoc locn				$
    tcLookupLocalValueOK "tcMonoBinds" name	`thenNF_Tc` \ id ->
    tcMatchesFun name (idType id) matches	`thenTc` \ (matches', lie) ->
    returnTc (FunMonoBind (TcId id) matches' locn, lie)
\end{code}

%************************************************************************
%*									*
\subsection{Signatures}
%*									*
%************************************************************************

@tcSigs@ checks the signatures for validity, and returns a list of
{\em freshly-instantiated} signatures.  That is, the types are already
split up, and have fresh type variables installed.  All non-type-signature
"RenamedSigs" are ignored.

\begin{code}
tcTySigs :: [RenamedSig] -> TcM s [TcSigInfo s]

tcTySigs (Sig v ty _ src_loc : other_sigs)
 = tcAddSrcLoc src_loc (
	tcPolyType ty			`thenTc` \ sigma_ty ->
	tcInstType [] sigma_ty		`thenNF_Tc` \ tc_sigma_ty ->
	let
	    (tyvars, theta, tau_ty) = splitSigmaTy tc_sigma_ty
	in
	tcLookupLocalValueOK "tcSig1" v	`thenNF_Tc` \ val ->
	unifyTauTy (idType val) tau_ty	`thenTc_`
	returnTc (TySigInfo val tyvars theta tau_ty src_loc)
   )		`thenTc` \ sig_info1 ->

   tcTySigs other_sigs	`thenTc` \ sig_infos ->
   returnTc (sig_info1 : sig_infos)

tcTySigs (other : sigs) = tcTySigs sigs
tcTySigs []		= returnTc []
\end{code}


%************************************************************************
%*									*
\subsection{SPECIALIZE pragmas}
%*									*
%************************************************************************


@tcPragmaSigs@ munches up the "signatures" that arise through *user*
pragmas.  It is convenient for them to appear in the @[RenamedSig]@
part of a binding because then the same machinery can be used for
moving them into place as is done for type signatures.

\begin{code}
tcPragmaSigs :: [RenamedSig]			-- The pragma signatures
	     -> TcM s (Name -> PragmaInfo,	-- Maps name to the appropriate PragmaInfo
		       TcHsBinds s,
		       LIE s)

tcPragmaSigs sigs = returnTc ( \name -> NoPragmaInfo, EmptyBinds, emptyLIE )

{- 
tcPragmaSigs sigs
  = mapAndUnzip3Tc tcPragmaSig sigs	`thenTc` \ (names_w_id_infos, binds, lies) ->
    let
	name_to_info name = foldr ($) noIdInfo
				  [info_fn | (n,info_fn) <- names_w_id_infos, n==name]
    in
    returnTc (name_to_info,
	      foldr ThenBinds EmptyBinds binds,
	      foldr plusLIE emptyLIE lies)
\end{code}

Here are the easy cases for tcPragmaSigs

\begin{code}
tcPragmaSig (DeforestSig name loc)
  = returnTc ((name, addInfo DoDeforest),EmptyBinds,emptyLIE)
tcPragmaSig (InlineSig name loc)
  = returnTc ((name, addInfo_UF (iWantToBeINLINEd UnfoldAlways)), EmptyBinds, emptyLIE)
tcPragmaSig (MagicUnfoldingSig name string loc)
  = returnTc ((name, addInfo_UF (mkMagicUnfolding string)), EmptyBinds, emptyLIE)
\end{code}

The interesting case is for SPECIALISE pragmas.  There are two forms.
Here's the first form:
\begin{verbatim}
	f :: Ord a => [a] -> b -> b
	{-# SPECIALIZE f :: [Int] -> b -> b #-}
\end{verbatim}

For this we generate:
\begin{verbatim}
	f* = /\ b -> let d1 = ...
		     in f Int b d1
\end{verbatim}

where f* is a SpecPragmaId.  The **sole** purpose of SpecPragmaIds is to
retain a right-hand-side that the simplifier will otherwise discard as
dead code... the simplifier has a flag that tells it not to discard
SpecPragmaId bindings.

In this case the f* retains a call-instance of the overloaded
function, f, (including appropriate dictionaries) so that the
specialiser will subsequently discover that there's a call of @f@ at
Int, and will create a specialisation for @f@.  After that, the
binding for @f*@ can be discarded.

The second form is this:
\begin{verbatim}
	f :: Ord a => [a] -> b -> b
	{-# SPECIALIZE f :: [Int] -> b -> b = g #-}
\end{verbatim}

Here @g@ is specified as a function that implements the specialised
version of @f@.  Suppose that g has type (a->b->b); that is, g's type
is more general than that required.  For this we generate
\begin{verbatim}
	f@Int = /\b -> g Int b
	f* = f@Int
\end{verbatim}

Here @f@@Int@ is a SpecId, the specialised version of @f@.  It inherits
f's export status etc.  @f*@ is a SpecPragmaId, as before, which just serves
to prevent @f@@Int@ from being discarded prematurely.  After specialisation,
if @f@@Int@ is going to be used at all it will be used explicitly, so the simplifier can
discard the f* binding.

Actually, there is really only point in giving a SPECIALISE pragma on exported things,
and the simplifer won't discard SpecIds for exporte things anyway, so maybe this is
a bit of overkill.

\begin{code}
tcPragmaSig (SpecSig name poly_ty maybe_spec_name src_loc)
  = tcAddSrcLoc src_loc		 		$
    tcAddErrCtxt (valSpecSigCtxt name spec_ty)	$

	-- Get and instantiate its alleged specialised type
    tcPolyType poly_ty				`thenTc` \ sig_sigma ->
    tcInstType [] (idType sig_sigma)		`thenNF_Tc` \ sig_ty ->
    let
	(sig_tyvars, sig_theta, sig_tau) = splitSigmaTy sig_ty
	origin = ValSpecOrigin name
    in

	-- Check that the SPECIALIZE pragma had an empty context
    checkTc (null sig_theta)
	    (panic "SPECIALIZE non-empty context (ToDo: msg)") `thenTc_`

	-- Get and instantiate the type of the id mentioned
    tcLookupLocalValueOK "tcPragmaSig" name	`thenNF_Tc` \ main_id ->
    tcInstType [] (idType main_id)		`thenNF_Tc` \ main_ty ->
    let
	(main_tyvars, main_rho) = splitForAllTy main_ty
	(main_theta,main_tau)   = splitRhoTy main_rho
	main_arg_tys	        = mkTyVarTys main_tyvars
    in

	-- Check that the specialised type is indeed an instance of
	-- the type of the main function.
    unifyTauTy sig_tau main_tau			`thenTc_`
    checkSigTyVars sig_tyvars sig_tau main_tau	`thenTc_`

	-- Check that the type variables of the polymorphic function are
	-- either left polymorphic, or instantiate to ground type.
	-- Also check that the overloaded type variables are instantiated to
	-- ground type; or equivalently that all dictionaries have ground type
    mapTc zonkTcType main_arg_tys	`thenNF_Tc` \ main_arg_tys' ->
    zonkTcThetaType main_theta		`thenNF_Tc` \ main_theta' ->
    tcAddErrCtxt (specGroundnessCtxt main_arg_tys')
	      (checkTc (all isGroundOrTyVarTy main_arg_tys'))      	`thenTc_`
    tcAddErrCtxt (specContextGroundnessCtxt main_theta')
	      (checkTc (and [isGroundTy ty | (_,ty) <- theta']))	`thenTc_`

	-- Build the SpecPragmaId; it is the thing that makes sure we
	-- don't prematurely dead-code-eliminate the binding we are really interested in.
    newSpecPragmaId name sig_ty		`thenNF_Tc` \ spec_pragma_id ->

	-- Build a suitable binding; depending on whether we were given
	-- a value (Maybe Name) to be used as the specialisation.
    case using of
      Nothing ->		-- No implementation function specified

		-- Make a Method inst for the occurrence of the overloaded function
	newMethodWithGivenTy (OccurrenceOf name)
		  (TcId main_id) main_arg_tys main_rho	`thenNF_Tc` \ (lie, meth_id) ->

	let
	    pseudo_bind = VarMonoBind spec_pragma_id pseudo_rhs
	    pseudo_rhs  = mkHsTyLam sig_tyvars (HsVar (TcId meth_id))
	in
	returnTc (pseudo_bind, lie, \ info -> info)

      Just spec_name ->		-- Use spec_name as the specialisation value ...

		-- Type check a simple occurrence of the specialised Id
	tcId spec_name		`thenTc` \ (spec_body, spec_lie, spec_tau) ->

		-- Check that it has the correct type, and doesn't constrain the
		-- signature variables at all
	unifyTauTy sig_tau spec_tau   	  		`thenTc_`
	checkSigTyVars sig_tyvars sig_tau spec_tau	`thenTc_`

	    -- Make a local SpecId to bind to applied spec_id
	newSpecId main_id main_arg_tys sig_ty	`thenNF_Tc` \ local_spec_id ->

	let
	    spec_rhs   = mkHsTyLam sig_tyvars spec_body
	    spec_binds = VarMonoBind local_spec_id spec_rhs
			   `AndMonoBinds`
	   		 VarMonoBind spec_pragma_id (HsVar (TcId local_spec_id))
	    spec_info  = SpecInfo spec_tys (length main_theta) local_spec_id
	in
	returnTc ((name, addInfo spec_info), spec_binds, spec_lie)
-}
\end{code}


Error contexts and messages
~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
patMonoBindsCtxt bind sty
  = ppHang (ppPStr SLIT("In a pattern binding:")) 4 (ppr sty bind)

--------------------------------------------
specContextGroundnessCtxt -- err_ctxt dicts sty
  = panic "specContextGroundnessCtxt"
{-
  = ppHang (
    	ppSep [ppBesides [ppStr "In the SPECIALIZE pragma for `", ppr sty name, ppStr "'"],
	       ppBesides [ppStr " specialised to the type `", ppr sty spec_ty,  ppStr "'"],
	       pp_spec_id sty,
	       ppStr "... not all overloaded type variables were instantiated",
	       ppStr "to ground types:"])
      4 (ppAboves [ppCat [ppr sty c, ppr sty t]
		  | (c,t) <- map getDictClassAndType dicts])
  where
    (name, spec_ty, locn, pp_spec_id)
      = case err_ctxt of
	  ValSpecSigCtxt    n ty loc      -> (n, ty, loc, \ x -> ppNil)
	  ValSpecSpecIdCtxt n ty spec loc ->
	    (n, ty, loc,
	     \ sty -> ppBesides [ppStr "... type of explicit id `", ppr sty spec, ppStr "'"])
-}

-----------------------------------------------
specGroundnessCtxt
  = panic "specGroundnessCtxt"


valSpecSigCtxt v ty sty
  = ppHang (ppPStr SLIT("In a SPECIALIZE pragma for a value:"))
	 4 (ppSep [ppBeside (pprNonOp sty v) (ppPStr SLIT(" ::")),
		  ppr sty ty])
\end{code}

