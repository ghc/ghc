%
% (c) The AQUA Project, Glasgow University, 1993-1996
%
\section[SimplVar]{Simplifier stuff related to variables}

\begin{code}
module SimplVar (
	completeVar
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Simplify ( simplExpr )

import CmdLineOpts	( switchIsOn, SimplifierSwitch(..) )
import CoreSyn
import CoreUnfold	( Unfolding(..), UnfoldingGuidance(..), 
			  SimpleUnfolding(..),
			  FormSummary, whnfOrBottom,
			  smallEnoughToInline )
import BinderInfo	( BinderInfo, noBinderInfo, okToInline )

import CostCentre	( CostCentre, isCurrentCostCentre )
import Id		( idType, getIdInfo, getIdUnfolding, getIdSpecialisation,
			  idMustBeINLINEd, GenId{-instance Outputable-}
			)
import SpecEnv		( matchSpecEnv )
import Literal		( isNoRepLit )
import MagicUFs		( applyMagicUnfoldingFun, MagicUnfoldingFun )
import PprType		( GenType{-instance Outputable-} )
import SimplEnv
import SimplMonad
import TyCon		( tyConFamilySize )
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
completeVar env var args result_ty

  | maybeToBool maybe_magic_result
  = tick MagicUnfold	`thenSmpl_`
    magic_result

	-- If there's an InUnfolding it means that there's no
	-- let-binding left for the thing, so we'd better inline it!
  | must_unfold
  = let
	Just (_, _, InUnfolding rhs_env rhs) = info_from_env
    in
    unfold var rhs_env rhs args result_ty


	-- Conditional unfolding. There's a binding for the
	-- thing, but perhaps we want to inline it anyway
  | (  maybeToBool maybe_unfolding_info
    && (not essential_unfoldings_only || idMustBeINLINEd var)
	-- If "essential_unfoldings_only" is true we do no inlinings at all,
	-- EXCEPT for things that absolutely have to be done
	-- (see comments with idMustBeINLINEd)
    && ok_to_inline
    && costCentreOk (getEnclosingCC env) (getEnclosingCC unf_env)
    )
  = unfold var unf_env unf_template args result_ty


  | maybeToBool maybe_specialisation
  = tick SpecialisationDone	`thenSmpl_`
    simplExpr (extendTyEnvEnv env spec_bindings) 
	      spec_template
	      remaining_args
	      result_ty

  | otherwise
  = returnSmpl (mkGenApp (Var var) args)

  where
    info_from_env     = lookupOutIdEnv env var
    unfolding_from_id = getIdUnfolding var

	---------- Magic unfolding stuff
    maybe_magic_result	= case unfolding_from_id of
				MagicUnfolding _ magic_fn -> applyMagicUnfoldingFun magic_fn 
										    env args
			        other 			  -> Nothing
    (Just magic_result)	= maybe_magic_result

	---------- Unfolding stuff
    must_unfold = case info_from_env of
			Just (_, _, InUnfolding _ _) -> True
			other			     -> False

    maybe_unfolding_info 
	= case (info_from_env, unfolding_from_id) of

	     (Just (_, occ_info, OutUnfolding enc_cc unf), _)
		-> Just (occ_info, setEnclosingCC env enc_cc, unf)	

	     (_, CoreUnfolding unf)
		-> Just (noBinderInfo, env, unf)

	     other -> Nothing

    Just (occ_info, unf_env, simple_unfolding) = maybe_unfolding_info
    SimpleUnfolding form guidance unf_template = simple_unfolding

    	---------- Specialisation stuff
    (ty_args, remaining_args) = initialTyArgs args
    maybe_specialisation = matchSpecEnv (getIdSpecialisation var) ty_args
    Just (spec_bindings, spec_template) = maybe_specialisation


	---------- Switches
    sw_chkr		      = getSwitchChecker env
    essential_unfoldings_only = switchIsOn sw_chkr EssentialUnfoldingsOnly
    is_case_scrutinee	      = switchIsOn sw_chkr SimplCaseScrutinee
    ok_to_inline	      = okToInline (whnfOrBottom form) small_enough occ_info 
    small_enough	      = smallEnoughToInline arg_evals is_case_scrutinee guidance
    arg_evals		      = [is_evald arg | arg <- args, isValArg arg]

    is_evald (VarArg v) = isEvaluated (lookupRhsInfo env v)
    is_evald (LitArg l) = True


-- Perform the unfolding
unfold var unf_env unf_template args result_ty
 =
{-
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


-- costCentreOk checks that it's ok to inline this thing
-- The time it *isn't* is this:
--
--	f x = let y = E in
--	      scc "foo" (...y...)
--
-- Here y has a "current cost centre", and we can't inline it inside "foo",
-- regardless of whether E is a WHNF or not.

costCentreOk cc_encl cc_rhs
  = isCurrentCostCentre cc_encl || not (isCurrentCostCentre cc_rhs)
\end{code}		   

