%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcBinds]{TcBinds}

\begin{code}
#include "HsVersions.h"

module TcBinds (
	tcTopBindsAndThen, tcLocalBindsAndThen,
	tcSigs, doSpecPragma
    ) where

--IMPORT_Trace		-- ToDo:rm (debugging)

import TcMonad		-- typechecking monad machinery
import TcMonadFns	( newLocalsWithOpenTyVarTys,
			  newLocalsWithPolyTyVarTys,
			  newSpecPragmaId, newSpecId,
			  applyTcSubstAndCollectTyVars
			)
import AbsSyn		-- the stuff being typechecked

import AbsUniType	( isTyVarTy, isGroundTy, isUnboxedDataType,
			  isGroundOrTyVarTy, extractTyVarsFromTy,
			  UniType
			)
import BackSubst	( applyTcSubstToBinds )
import E
import Errors		( topLevelUnboxedDeclErr, specGroundnessErr,
			  specCtxtGroundnessErr, Error(..), UnifyErrContext(..)
			)
import GenSpecEtc	( checkSigTyVars, genBinds, SignatureInfo(..) )
import Id		( getIdUniType, mkInstId )
import IdInfo		( SpecInfo(..) )
import Inst
import LIE		( nullLIE, mkLIE, plusLIE, LIE )
import Maybes		( assocMaybe, catMaybes, Maybe(..) )
import Spec		( specTy )
import TVE		( nullTVE, TVE(..), UniqFM )
import TcMonoBnds	( tcMonoBinds )
import TcPolyType	( tcPolyType )
import TcSimplify	( bindInstsOfLocalFuns )
import Unify		( unifyTauTy )
import UniqFM		( emptyUFM ) -- profiling, pragmas only
import Util
\end{code}

%************************************************************************
%*									*
\subsection{Type-checking top-level bindings}
%*									*
%************************************************************************

@tcBindsAndThen@ takes a boolean which indicates whether the binding
group is at top level or not.  The difference from inner bindings is
that
\begin{enumerate}
\item
we zero the substitution before each group
\item
we back-substitute after each group.
\end{enumerate}
We still return an LIE, but it is sure to contain nothing but constant
dictionaries, which we resolve at the module level.

@tcTopBinds@ returns an LVE, not, as you might expect, a GVE.  Why?
Because the monomorphism restriction means that is might return some
monomorphic things, with free type variables.  Hence it must be an LVE.

The LIE returned by @tcTopBinds@ may constrain some type variables,
but they are guaranteed to be a subset of those free in the
corresponding returned LVE.

%************************************************************************
%*									*
\subsection{Type-checking bindings}
%*									*
%************************************************************************

@tcBindsAndThen@ typechecks a @Binds@.  The "and then" part is because
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

\begin{code}
tcBindsAndThen 
	:: Bool
	-> E 
	-> (TypecheckedBinds -> thing -> thing)		-- Combinator
	-> RenamedBinds
	-> (E -> TcM (thing, LIE, thing_ty))
	-> TcM (thing, LIE, thing_ty)

tcBindsAndThen top_level e combiner EmptyBinds do_next
  = do_next e		`thenTc` \ (thing, lie, thing_ty) ->
    returnTc (combiner EmptyBinds thing, lie, thing_ty)

tcBindsAndThen top_level e combiner (SingleBind bind) do_next
  = tcBindAndThen top_level e combiner bind [] do_next

tcBindsAndThen top_level e combiner (BindWith bind sigs) do_next
  = tcBindAndThen top_level e combiner bind sigs do_next

tcBindsAndThen top_level e combiner (ThenBinds binds1 binds2) do_next
  = tcBindsAndThen top_level e combiner binds1 new_after
  where
    -- new_after :: E -> TcM (thing, LIE, thing_ty)
    -- Can't write this signature, cos it's monomorphic in thing and
    -- thing_ty.
    new_after e = tcBindsAndThen top_level e combiner binds2 do_next
\end{code}

Simple wrappers for export:
\begin{code}
tcTopBindsAndThen
	:: E
	-> (TypecheckedBinds -> thing -> thing)		-- Combinator
	-> RenamedBinds 
	-> (E -> TcM (thing, LIE, anything))
	-> TcM (thing, LIE, anything)

tcTopBindsAndThen e combiner binds do_next
  = tcBindsAndThen True e combiner binds do_next

tcLocalBindsAndThen
	:: E 
	-> (TypecheckedBinds -> thing -> thing)		-- Combinator
	-> RenamedBinds 
	-> (E -> TcM (thing, LIE, thing_ty))
	-> TcM (thing, LIE, thing_ty)

tcLocalBindsAndThen e combiner binds do_next
  = tcBindsAndThen False e combiner  binds do_next
\end{code}

An aside.  The original version of @tcBindsAndThen@ which lacks a
combiner function, appears below.  Though it is perfectly well
behaved, it cannot be typed by Haskell, because the recursive call is
at a different type to the definition itself.  There aren't too many
examples of this, which is why I thought it worth preserving! [SLPJ]

\begin{pseudocode}
tcBindsAndThen 
	:: Bool -> E -> RenamedBinds
	-> (E -> TcM (thing, LIE, thing_ty))
	-> TcM ((TypecheckedBinds, thing), LIE, thing_ty)

tcBindsAndThen top_level e EmptyBinds do_next
  = do_next e		`thenTc` \ (thing, lie, thing_ty) ->
    returnTc ((EmptyBinds, thing), lie, thing_ty)

tcBindsAndThen top_level e (SingleBind bind) do_next
  = tcBindAndThen top_level e bind [] do_next

tcBindsAndThen top_level e (BindWith bind sigs) do_next
  = tcBindAndThen top_level e bind sigs do_next

tcBindsAndThen top_level e (ThenBinds binds1 binds2) do_next
  = tcBindsAndThen top_level e binds1 new_after
	`thenTc` \ ((binds1', (binds2', thing')), lie1, thing_ty) ->

    returnTc ((binds1' `ThenBinds` binds2', thing'), lie1, thing_ty)

  where
    -- new_after :: E -> TcM ((TypecheckedBinds, thing), LIE, thing_ty)
    -- Can't write this signature, cos it's monomorphic in thing and thing_ty
    new_after e = tcBindsAndThen top_level e binds2 do_next
\end{pseudocode}

%************************************************************************
%*									*
\subsection{Bind}
%*									*
%************************************************************************

\begin{code}
tcBindAndThen
	:: Bool						  -- At top level
	-> E 
	-> (TypecheckedBinds -> thing -> thing)		  -- Combinator
	-> RenamedBind					  -- The Bind to typecheck
	-> [RenamedSig]					  -- ...and its signatures
	-> (E -> TcM (thing, LIE, thing_ty))		  -- Thing to type check in
							  -- augmented envt
	-> TcM (thing, LIE, thing_ty) 			  -- Results, incl the 

tcBindAndThen top_level e combiner bind sigs do_next
  = 	-- Deal with the bind
    tcBind top_level e bind sigs    `thenTc` \ (poly_binds, poly_lie, poly_lve) ->

	-- Now do whatever happens next, in the augmented envt
    do_next (growE_LVE e poly_lve)  `thenTc` \ (thing, thing_lie, thing_ty) ->
    let
	bound_ids = map snd poly_lve
    in
	-- Create specialisations
    specialiseBinds bound_ids thing_lie poly_binds poly_lie
				    `thenNF_Tc` \ (final_binds, final_lie) ->
	-- All done
    returnTc (combiner final_binds thing, final_lie, thing_ty)
\end{code}

\begin{code}
tcBind :: Bool -> E 
       -> RenamedBind -> [RenamedSig]
       -> TcM (TypecheckedBinds, LIE, LVE)	-- LIE is a fixed point of substitution

tcBind False e bind sigs  			-- Not top level
  = tcBind_help False e bind sigs

tcBind True  e bind sigs			-- Top level!
  = pruneSubstTc (tvOfE e) (

	 -- DO THE WORK
    tcBind_help True e bind sigs	`thenTc` \ (new_binds, lie, lve) ->

{-  Top-level unboxed values are now allowed
    They will be lifted by the Desugarer (see CoreLift.lhs)

	-- CHECK FOR PRIMITIVE TOP-LEVEL BINDS
	listTc [ checkTc (isUnboxedDataType (getIdUniType id))
			 (topLevelUnboxedDeclErr id (getSrcLoc id))
	       | (_,id) <- lve ]	`thenTc_`
-}

    -- Back-substitute over the binds, since we are about to discard
    -- a good chunk of the substitution.
    applyTcSubstToBinds new_binds	`thenNF_Tc` \ final_binds ->

    -- The lie is already a fixed point of the substitution; it just turns out
    -- that almost always this happens automatically, and so we made it part of
    -- the specification of genBinds.
    returnTc (final_binds, lie, lve)
    )
\end{code}

\begin{code}
tcBind_help top_level e bind sigs
  = 	-- Create an LVE binding each identifier to an appropriate type variable
    new_locals binders		`thenNF_Tc` \ bound_ids ->
    let  lve = binders `zip` bound_ids  in

	-- Now deal with type signatures, if any
    tcSigs e lve sigs		`thenTc`    \ sig_info ->

	-- Check the bindings: this is the point at which we can use
	-- error recovery.  If checking the bind fails we just
	-- return the empty bindings.  The variables will still be in
	-- scope, but bound to completely free type variables, which
	-- is just what we want to minimise subsequent error messages.
    recoverTc (NonRecBind EmptyMonoBinds, nullLIE)
	      (tc_bind (growE_LVE e lve) bind)	`thenNF_Tc` \ (bind', lie) ->

	-- Notice that genBinds gets the old (non-extended) environment
    genBinds top_level e bind' lie lve sig_info	`thenTc` \ (binds', lie, lve) ->

	-- Add bindings corresponding to SPECIALIZE pragmas in the code
    mapAndUnzipTc (doSpecPragma e (assoc "doSpecPragma" lve))
		  (get_spec_pragmas sig_info)
			`thenTc` \ (spec_binds_s, spec_lie_s) ->

    returnTc (binds' `ThenBinds` (SingleBind (NonRecBind (
		foldr AndMonoBinds EmptyMonoBinds spec_binds_s))),
	      lie `plusLIE` (foldr plusLIE nullLIE spec_lie_s),
	      lve)
  where
    binders = collectBinders bind

    new_locals binders
      = case bind of
	  NonRecBind _ -> -- Recursive, so no unboxed types
			  newLocalsWithOpenTyVarTys binders

	  RecBind _    -> -- Non-recursive, so we permit unboxed types
			  newLocalsWithPolyTyVarTys binders

    get_spec_pragmas sig_info
      = catMaybes (map get_pragma_maybe sig_info)
      where
	get_pragma_maybe s@(ValSpecInfo _ _ _ _) = Just s
	get_pragma_maybe _  	    	         = Nothing
\end{code}

\begin{verbatim}
	f :: Ord a => [a] -> b -> b
	{-# SPECIALIZE f :: [Int] -> b -> b #-}
\end{verbatim}
We generate:
\begin{verbatim}
	f@Int = /\ b -> let d1 = ...
			in f Int b d1


	h :: Ord a => [a] -> b -> b
	{-# SPECIALIZE h :: [Int] -> b -> b #-}

	spec_h = /\b -> h [Int] b dListOfInt
			^^^^^^^^^^^^^^^^^^^^ This bit created by specId
\end{verbatim}

\begin{code}
doSpecPragma :: E
	     -> (Name -> Id)
	     -> SignatureInfo
	     -> TcM (TypecheckedMonoBinds, LIE)

doSpecPragma e name_to_id (ValSpecInfo name spec_ty using src_loc)
  = let
	main_id = name_to_id name    -- Get the parent Id

	main_id_ty = getIdUniType main_id
	main_id_free_tyvars = extractTyVarsFromTy main_id_ty
	origin = ValSpecOrigin name src_loc
    	err_ctxt = ValSpecSigCtxt name spec_ty src_loc
    in
    addSrcLocTc src_loc		 (
    specTy origin spec_ty `thenNF_Tc` \ (spec_tyvars, spec_dicts, spec_tau) ->

	-- Check that the SPECIALIZE pragma had an empty context
    checkTc (not (null spec_dicts))
	    (panic "SPECIALIZE non-empty context (ToDo: msg)") `thenTc_`

	-- Make an instance of this id
    specTy origin main_id_ty `thenNF_Tc` \ (main_tyvars, main_dicts, main_tau) ->

	-- Check that the specialised type is indeed an instance of
	-- the inferred type.
	-- The unification should leave all type vars which are
	-- currently free in the environment still free, and likewise
	-- the signature type vars.
	-- The only way type vars free in the envt could possibly be affected
	-- is if main_id_ty has free type variables.  So we just extract them,
	-- and check that they are not constrained in any way by the unification.
    applyTcSubstAndCollectTyVars main_id_free_tyvars  `thenNF_Tc` \ free_tyvars' ->
    unifyTauTy spec_tau main_tau err_ctxt   `thenTc_`
    checkSigTyVars [] (spec_tyvars ++ free_tyvars')
		   spec_tau main_tau err_ctxt `thenTc_`

	-- Check that the type variables of the polymorphic function are
	-- either left polymorphic, or instantiate to ground type.
	-- Also check that the overloaded type variables are instantiated to
	-- ground type; or equivalently that all dictionaries have ground type
    applyTcSubstToTyVars main_tyvars	`thenNF_Tc` \ main_arg_tys ->
    applyTcSubstToInsts  main_dicts	`thenNF_Tc` \ main_dicts' ->

    checkTc (not (all isGroundOrTyVarTy main_arg_tys))
	    (specGroundnessErr err_ctxt main_arg_tys)
				    	`thenTc_`

    checkTc (not (and [isGroundTy ty | (_,ty) <- map getDictClassAndType main_dicts']))
	    (specCtxtGroundnessErr err_ctxt main_dicts')
					`thenTc_`

	-- Build a suitable binding; depending on whether we were given
	-- a value (Maybe Name) to be used as the specialisation.
    case using of
      Nothing ->

	    -- Make a specPragmaId to which to bind the new call-instance
	newSpecPragmaId name spec_ty Nothing
					`thenNF_Tc` \ pseudo_spec_id ->
	let
	    pseudo_bind = VarMonoBind pseudo_spec_id pseudo_rhs
	    pseudo_rhs  = mkTyLam spec_tyvars (mkDictApp (mkTyApp (Var main_id) main_arg_tys)
						         (map mkInstId main_dicts'))
	in
	returnTc (pseudo_bind, mkLIE main_dicts')

      Just spec_name -> -- use spec_name as the specialisation value ...
	let
	    spec_id      = lookupE_Value e spec_name
	    spec_id_ty   = getIdUniType spec_id

	    spec_id_free_tyvars = extractTyVarsFromTy spec_id_ty
	    spec_id_ctxt = ValSpecSpecIdCtxt name spec_ty spec_name src_loc

	    spec_tys    = map maybe_ty main_arg_tys
            maybe_ty ty | isTyVarTy ty = Nothing
			| otherwise    = Just ty
	in
	    -- Make an instance of the spec_id
	specTy origin spec_id_ty `thenNF_Tc` \ (spec_id_tyvars, spec_id_dicts, spec_id_tau) ->

	    -- Check that the specialised type is indeed an instance of
	    -- the type inferred for spec_id
	    -- The unification should leave all type vars which are
	    -- currently free in the environment still free, and likewise
	    -- the signature type vars.
	    -- The only way type vars free in the envt could possibly be affected
	    -- is if spec_id_ty has free type variables.  So we just extract them,
	    -- and check that they are not constrained in any way by the unification.
        applyTcSubstAndCollectTyVars spec_id_free_tyvars  `thenNF_Tc` \ spec_id_free_tyvars' ->
        unifyTauTy spec_tau spec_id_tau spec_id_ctxt   	  `thenTc_`
        checkSigTyVars [] (spec_tyvars ++ spec_id_free_tyvars')
		       spec_tau spec_id_tau spec_id_ctxt  `thenTc_`

	    -- Check that the type variables of the explicit spec_id are
	    -- either left polymorphic, or instantiate to ground type.
	    -- Also check that the overloaded type variables are instantiated to
	    -- ground type; or equivalently that all dictionaries have ground type
    	applyTcSubstToTyVars spec_id_tyvars	`thenNF_Tc` \ spec_id_arg_tys ->
    	applyTcSubstToInsts  spec_id_dicts	`thenNF_Tc` \ spec_id_dicts' ->

    	checkTc (not (all isGroundOrTyVarTy spec_id_arg_tys))
		(specGroundnessErr spec_id_ctxt spec_id_arg_tys)
				    		`thenTc_`

    	checkTc (not (and [isGroundTy ty | (_,ty) <- map getDictClassAndType spec_id_dicts']))
	    	(specCtxtGroundnessErr spec_id_ctxt spec_id_dicts')
						`thenTc_`

	    -- Make a local SpecId to bind to applied spec_id
	newSpecId main_id spec_tys spec_ty	`thenNF_Tc` \ local_spec_id ->

	    -- Make a specPragmaId id with a spec_info for local_spec_id
	    -- This is bound to local_spec_id
	    -- The SpecInfo will be extracted by the specialiser and
	    -- used to create a call instance for main_id (which is
	    -- extracted from the spec_id)
	    -- NB: the pseudo_local_id must stay in the scope of main_id !!!
	let
	    spec_info = SpecInfo spec_tys (length main_dicts') local_spec_id
	in
	newSpecPragmaId name spec_ty (Just spec_info)	`thenNF_Tc` \ pseudo_spec_id ->
	let
	    spec_bind   = VarMonoBind local_spec_id spec_rhs
	    spec_rhs    = mkTyLam spec_tyvars (mkDictApp (mkTyApp (Var spec_id) spec_id_arg_tys)
						         (map mkInstId spec_id_dicts'))
	    pseudo_bind = VarMonoBind pseudo_spec_id (Var local_spec_id)
	in
	returnTc (spec_bind `AndMonoBinds` pseudo_bind, mkLIE spec_id_dicts')
    )
\end{code}

\begin{code}
tc_bind :: E
	-> RenamedBind
	-> TcM (TypecheckedBind, LIE)

tc_bind e (NonRecBind mono_binds)
  = tcMonoBinds e mono_binds	`thenTc` \ (mono_binds2, lie) ->
    returnTc  (NonRecBind mono_binds2, lie)

tc_bind e (RecBind mono_binds)
  = tcMonoBinds e mono_binds	`thenTc` \ (mono_binds2, lie) ->
    returnTc  (RecBind mono_binds2, lie)
\end{code}

\begin{code}
specialiseBinds
	:: [Id] 		-- Ids bound in this group
	-> LIE			-- LIE of scope of these bindings
	-> TypecheckedBinds
	-> LIE
	-> NF_TcM (TypecheckedBinds, LIE)

specialiseBinds bound_ids lie_of_scope poly_binds poly_lie
  = bindInstsOfLocalFuns lie_of_scope bound_ids
					`thenNF_Tc` \ (lie2, inst_mbinds) ->

    returnNF_Tc (poly_binds `ThenBinds` (SingleBind (NonRecBind inst_mbinds)),
		 lie2 `plusLIE` poly_lie)
\end{code}

%************************************************************************
%*									*
\subsection{Signatures}
%*									*
%************************************************************************

@tcSigs@ checks the signatures for validity, and returns a list of
{\em freshly-instantiated} signatures.  That is, the types are already
split up, and have fresh type variables (not @TyVarTemplate@s)
installed.

\begin{code}
tcSigs :: E -> LVE
       -> [RenamedSig] 
       -> TcM [SignatureInfo]

tcSigs e lve [] = returnTc []

tcSigs e lve (s:ss)
  = tc_sig  	 s	`thenTc` \ sig_info1 ->
    tcSigs e lve ss	`thenTc` \ sig_info2 ->
    returnTc (sig_info1 : sig_info2)
  where
    tc_sig (Sig v ty _ src_loc)	-- no interesting pragmas on non-iface sigs
      = addSrcLocTc src_loc (

	babyTcMtoTcM
	  (tcPolyType (getE_CE e) (getE_TCE e) nullTVE ty) `thenTc` \ sigma_ty ->

	let  val = assoc "tcSigs" lve v  in
	    -- (The renamer/dependency-analyser should have ensured
	    -- that there are only signatures for which there is a
	    -- corresponding binding.)

	    -- Instantiate the type, and unify with the type variable
	    -- found in the Id.
	specTy SignatureOrigin sigma_ty	`thenNF_Tc` \ (tyvars, dicts, tau_ty) ->
	unifyTauTy (getIdUniType val) tau_ty
		   (panic "ToDo: unifyTauTy(tcSigs)") `thenTc_`

	returnTc (TySigInfo val tyvars dicts tau_ty src_loc)
	)

    tc_sig (SpecSig v ty using src_loc)
      = addSrcLocTc src_loc (

	babyTcMtoTcM
	  (tcPolyType (getE_CE e) (getE_TCE e) nullTVE ty) `thenTc` \ sigma_ty ->

	returnTc (ValSpecInfo v sigma_ty using src_loc)
	)

    tc_sig (InlineSig v guide locn)
      = returnTc (ValInlineInfo v guide locn)

    tc_sig (DeforestSig v locn)
      = returnTc (ValDeforestInfo v locn)

    tc_sig (MagicUnfoldingSig v str locn)
      = returnTc (ValMagicUnfoldingInfo v str locn)
\end{code}
