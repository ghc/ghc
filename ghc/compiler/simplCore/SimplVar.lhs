%
% (c) The AQUA Project, Glasgow University, 1993-1996
%
\section[SimplVar]{Simplifier stuff related to variables}
				
\begin{code}
module SimplVar (
	completeVar,
	simplBinder, simplBinders, simplTyBinder, simplTyBinders
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Simplify ( simplExpr )

import CmdLineOpts	( switchIsOn, SimplifierSwitch(..) )
import CoreSyn
import CoreUnfold	( Unfolding(..), UnfoldingGuidance(..), 
			  FormSummary, whnfOrBottom, okToInline,
			  smallEnoughToInline )
import CoreUtils	( coreExprCc )
import BinderInfo	( BinderInfo, noBinderInfo )

import CostCentre	( CostCentre, noCostCentreAttached, isCurrentCostCentre )
import Id		( idType, getIdUnfolding, externallyVisibleId,
			  getIdSpecialisation, setIdSpecialisation,
			  idMustBeINLINEd, idHasNoFreeTyVars,
			  mkIdWithNewUniq, mkIdWithNewType, 
			  IdEnv, lookupIdEnv, delOneFromIdEnv, elemIdEnv, isNullIdEnv, addOneToIdEnv
			)
import SpecEnv		( lookupSpecEnv, isEmptySpecEnv, emptySpecEnv )
import OccurAnal	( occurAnalyseGlobalExpr )
import Literal		( isNoRepLit )
import MagicUFs		( applyMagicUnfoldingFun, MagicUnfoldingFun )
import SimplEnv
import SimplMonad
import Type		( instantiateTy, mkTyVarTy )
import TyCon		( tyConFamilySize )
import TyVar		( TyVar, cloneTyVar,
			  isEmptyTyVarEnv, addToTyVarEnv, delFromTyVarEnv,
			  addOneToTyVarSet, elementOfTyVarSet
			)
import Maybes		( maybeToBool )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[Simplify-var]{Completing variables}
%*									*
%************************************************************************

This where all the heavy-duty unfolding stuff comes into its own.

\begin{code}
completeVar env inline_call var args result_ty

  | maybeToBool maybe_magic_result
  = tick MagicUnfold	`thenSmpl_`
    magic_result

	-- Look for existing specialisations before
	-- trying inlining
  | maybeToBool maybe_specialisation
  = tick SpecialisationDone	`thenSmpl_`
    simplExpr (bindTyVars env spec_bindings) 
	      (occurAnalyseGlobalExpr spec_template)
	      remaining_args
	      result_ty


	-- Look for an unfolding. There's a binding for the
	-- thing, but perhaps we want to inline it anyway
  |    has_unfolding
    && (idMustBeINLINEd var || 
	(not essential_unfoldings_only 
		-- If "essential_unfoldings_only" is true we do no inlinings at all,
		-- EXCEPT for things that absolutely have to be done
		-- (see comments with idMustBeINLINEd)
         && (inline_call || ok_to_inline)
         && costCentreOk (getEnclosingCC env) (coreExprCc unf_template)))
  =
{-
    pprTrace "Unfolding" (ppr var) $
    simplCount		`thenSmpl` \ n ->
    (if n > 1000 then
	pprTrace "Ticks > 1000 and unfolding" (sep [space, int n, ppr var])
    else
	id
    )
    (if n>4000 then
       returnSmpl (mkGenApp (Var var) args)
    else
-}
    tickUnfold var		`thenSmpl_`
    simplExpr unf_env unf_template args result_ty

  | inline_call		-- There was an InlineCall note, but we didn't inline!
  = returnSmpl (mkGenApp (Note InlineCall (Var var')) args)

  | otherwise
  = returnSmpl (mkGenApp (Var var') args)

  where
    (var', occ_info, unfolding) = case lookupOutIdEnv env var of
					Just stuff -> stuff
					Nothing    -> (var, noBinderInfo, getIdUnfolding var)

	---------- Magic unfolding stuff
    maybe_magic_result	= case unfolding of
				MagicUnfolding _ magic_fn -> applyMagicUnfoldingFun magic_fn 
										    env args
			        other 			  -> Nothing
    Just magic_result = maybe_magic_result

	---------- Unfolding stuff
    has_unfolding = case unfolding of
			CoreUnfolding _ _ _ -> True
			other		    -> False

    CoreUnfolding form guidance unf_template = unfolding
    unf_env = zapSubstEnvs env
		-- The template is already simplified, so don't re-substitute.
		-- This is VITAL.  Consider
		--	let x = e in
		--	let y = \z -> ...x... in
		--	\ x -> ...y...
		-- We'll clone the inner \x, adding x->x' in the id_subst
		-- Then when we inline y, we must *not* replace x by x' in
		-- the inlined copy!!

    	---------- Specialisation stuff
    (ty_args, remaining_args) = initialTyArgs args
    maybe_specialisation      = lookupSpecEnv (ppr var) (getIdSpecialisation var) ty_args
    Just (spec_bindings, spec_template) = maybe_specialisation


	---------- Switches
    sw_chkr		      = getSwitchChecker env
    essential_unfoldings_only = switchIsOn sw_chkr EssentialUnfoldingsOnly
    is_case_scrutinee	      = switchIsOn sw_chkr SimplCaseScrutinee
    ok_to_inline	      = okToInline var (whnfOrBottom form) small_enough occ_info 
    small_enough	      = smallEnoughToInline var arg_evals is_case_scrutinee guidance
    arg_evals		      = [is_evald arg | arg <- args, isValArg arg]

    is_evald (VarArg v) = isEvaluated (lookupUnfolding env v)
    is_evald (LitArg l) = True




-- costCentreOk checks that it's ok to inline this thing
-- The time it *isn't* is this:
--
--	f x = let y = E in
--	      scc "foo" (...y...)
--
-- Here y has a "current cost centre", and we can't inline it inside "foo",
-- regardless of whether E is a WHNF or not.

costCentreOk cc_encl cc_rhs
  = isCurrentCostCentre cc_encl || not (noCostCentreAttached cc_rhs)
\end{code}		   


%************************************************************************
%*									*
\section{Dealing with a single binder}
%*									*
%************************************************************************

When we hit a binder we may need to
  (a) apply the the type envt (if non-empty) to its type
  (b) apply the type envt and id envt to its SpecEnv (if it has one)
  (c) give it a new unique to avoid name clashes

\begin{code}
simplBinder :: SimplEnv -> InBinder -> SmplM (SimplEnv, OutId)
simplBinder env (id, occ_info)
  |  no_need_to_clone	 	-- Not in scope (or cloning disabled), so no need to clone
  && empty_ty_subst 		-- No type substitution to do inside the Id
  && isNullIdEnv id_subst	-- No id substitution to do inside the Id
  = let 
	env'          = setIdEnv env (new_in_scope_ids id, id_subst)
    in
    returnSmpl (env', id)

  | otherwise
  = 
#if DEBUG
    -- I  reckon the empty-env thing should catch
    -- most no-free-tyvars things, so this test should be redundant
--    (if idHasNoFreeTyVars id then pprTrace "applyEnvsToId" (ppr id) else (\x -> x))
#endif
    (let
       -- id1 has its type zapped
       id1 | empty_ty_subst = id
           | otherwise      = mkIdWithNewType id ty'
       -- id2 has its SpecEnv zapped (see comment inside Simplify.completeBind)
       id2 | empty_spec_env = id1
           | otherwise      = setIdSpecialisation id1 emptySpecEnv
    in
    if no_need_to_clone then
	-- No need to clone, but we *must* zap any current substitution
	-- for the variable.  For example:
	--	(\x.e) with id_subst = [x |-> e']
	-- Here we must simply zap the substitution for x
	let
	    new_id_subst = delOneFromIdEnv id_subst id
	    new_env      = setIdEnv env (new_in_scope_ids id2, new_id_subst)
	in
	returnSmpl (new_env, id2)
    else
	-- Must clone
	getUniqueSmpl         `thenSmpl` \ uniq ->
	let
	    id3     = mkIdWithNewUniq id2 uniq
	    new_env = setIdEnv env (new_in_scope_ids id3,
				    addOneToIdEnv id_subst id (SubstVar id3))
	in
	returnSmpl (new_env, id3)
    )
  where
    ((in_scope_tyvars, ty_subst), (in_scope_ids, id_subst)) = getEnvs env

    empty_ty_subst    = isEmptyTyVarEnv ty_subst
    empty_spec_env    = isEmptySpecEnv (getIdSpecialisation id)

    no_need_to_clone  = not need_to_clone
    need_to_clone     = not (externallyVisibleId id) &&
			( elemIdEnv id in_scope_ids || clone_binds_please)
     {-
       The SimplCloneBinds option isn't just here as another simplifier knob we can 
       twiddle. Prior to floating bindings outwards, we have to make sure that no
       duplicate bindings exist as floating may cause bindings with identical
       uniques to come into scope, with disastrous consequences. 

       To avoid this situation, we make sure that cloning is turned *on* in the
       simplifier pass prior to running an outward floating pass.
     -}
    clone_binds_please = switchIsOn sw_chkr SimplCloneBinds

    new_in_scope_ids id' = addOneToIdEnv in_scope_ids id' (id', occ_info, NoUnfolding)
    
    ty               	 = idType id
    ty'              	 = instantiateTy ty_subst ty

    sw_chkr		 = getSwitchChecker env


simplBinders :: SimplEnv -> [InBinder] -> SmplM (SimplEnv, [OutId])
simplBinders env binders = mapAccumLSmpl simplBinder env binders
\end{code}

\begin{code}	
simplTyBinder :: SimplEnv -> TyVar -> SmplM (SimplEnv, TyVar)
simplTyBinder env tyvar
  | no_need_to_clone
  = 	-- No need to clone; but must zap any binding for tyvar
	-- see comments with simplBinder above
    let
	env' = setTyEnv env (tyvars `addOneToTyVarSet` tyvar, 
			     delFromTyVarEnv ty_subst tyvar)
    in
    returnSmpl (env', tyvar)

  | otherwise					-- Need to clone
  = getUniqueSmpl         `thenSmpl` \ uniq ->
    let
	tyvar' = cloneTyVar tyvar uniq
	env'   = setTyEnv env (tyvars `addOneToTyVarSet` tyvar', 
			       addToTyVarEnv ty_subst tyvar (mkTyVarTy tyvar'))
    in
    returnSmpl (env', tyvar')
  where
    ((tyvars, ty_subst), (ids, id_subst)) = getEnvs env
    no_need_to_clone			  = not (tyvar `elementOfTyVarSet` tyvars) && 
					    not clone_binds_please

    clone_binds_please			  = switchIsOn sw_chkr SimplCloneBinds
    sw_chkr				  = getSwitchChecker env


simplTyBinders :: SimplEnv -> [TyVar] -> SmplM (SimplEnv, [TyVar])
simplTyBinders env binders = mapAccumLSmpl simplTyBinder env binders
\end{code}
