%
% (c) The AQUA Project, Glasgow University, 1993-1996
%
\section[SimplVar]{Simplifier stuff related to variables}

\begin{code}
#include "HsVersions.h"

module SimplVar (
	completeVar
    ) where

IMP_Ubiq(){-uitous-}
IMPORT_DELOOPER(SmplLoop)		( simplExpr )

import Constants	( uNFOLDING_USE_THRESHOLD,
			  uNFOLDING_CON_DISCOUNT_WEIGHT
			)
import CmdLineOpts	( switchIsOn, SimplifierSwitch(..) )
import CoreSyn
import CoreUnfold	( Unfolding(..), UfExpr, RdrName, UnfoldingGuidance(..), SimpleUnfolding(..),
			  FormSummary,
			  okToInline, smallEnoughToInline )
import BinderInfo	( BinderInfo, noBinderInfo )

import CostCentre	( CostCentre, noCostCentreAttached )
import Id		( idType, getIdInfo, getIdUnfolding, getIdSpecialisation,
			  idMustBeINLINEd, GenId{-instance Outputable-}
			)
import SpecEnv		( SpecEnv, lookupSpecEnv )
import IdInfo		( DeforestInfo(..) )
import Literal		( isNoRepLit )
import MagicUFs		( applyMagicUnfoldingFun, MagicUnfoldingFun )
import PprStyle		( PprStyle(..) )
import PprType		( GenType{-instance Outputable-} )
import SimplEnv
import SimplMonad
import TyCon		( tyConFamilySize )
import Util		( pprTrace, assertPanic, panic )
import Maybes		( maybeToBool )
import Pretty
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

  | not do_deforest &&
    maybeToBool maybe_unfolding_info &&
    (not essential_unfoldings_only || idMustBeINLINEd var) && 
    ok_to_inline &&
	-- If "essential_unfolds_only" is true we do no inlinings at all,
	-- EXCEPT for things that absolutely have to be done
	-- (see comments with idMustBeINLINEd)
	--
	-- Need to be careful: the RHS of INLINE functions is protected against inlining
	-- by essential_unfoldings_only being set true; we must not inline workers back into
	-- wrappers, even though the former have an unfold-always guidance.
    costCentreOk (getEnclosingCC env) (getEnclosingCC unfold_env)
  = 
{-
    simplCount		`thenSmpl` \ n ->
    (if n > 1000 then
	pprTrace "Ticks > 1000 and unfolding" (sep [space, int n, ppr PprDebug var])
    else
	id
    )
    (if n>4000 then
       returnSmpl (mkGenApp (Var var) args)
    else
-}

    tickUnfold var		`thenSmpl_`
    simplExpr unfold_env unf_template args result_ty

  | maybeToBool maybe_specialisation
  = tick SpecialisationDone	`thenSmpl_`
    simplExpr (extendTyEnvList env spec_bindings) 
	      spec_template
	      (map TyArg leftover_ty_args ++ remaining_args)
	      result_ty

  | otherwise
  = returnSmpl (mkGenApp (Var var) args)

  where
    unfolding_from_id = getIdUnfolding var

	---------- Magic unfolding stuff
    maybe_magic_result	= case unfolding_from_id of
				MagicUnfolding _ magic_fn -> applyMagicUnfoldingFun magic_fn 
										    env args
			        other 			  -> Nothing
    (Just magic_result)	= maybe_magic_result

	---------- Unfolding stuff
    maybe_unfolding_info 
	= case (lookupOutIdEnv env var, unfolding_from_id) of

	     (Just (_, occ_info, OutUnfolding enc_cc unf), _)
		-> Just (occ_info, setEnclosingCC env enc_cc, unf)	

	     (Just (_, occ_info, InUnfolding env_unf unf), _)
		-> -- pprTrace ("InUnfolding for ") (ppr PprDebug var) $
		   Just (occ_info, env_unf, unf)	

	     (_, CoreUnfolding unf)
		-> -- pprTrace ("CoreUnfolding for ") (ppr PprDebug var) $
		   Just (noBinderInfo, env, unf)

	     other -> Nothing

    Just (occ_info, unfold_env, simple_unfolding)     = maybe_unfolding_info
    SimpleUnfolding form guidance unf_template = simple_unfolding

	---------- Specialisation stuff
    (ty_args, remaining_args) = initialTyArgs args
    maybe_specialisation = lookupSpecEnv (getIdSpecialisation var) ty_args
    (Just (spec_template, (spec_bindings, leftover_ty_args))) = maybe_specialisation


	---------- Switches
    sw_chkr		      = getSwitchChecker env
    essential_unfoldings_only = switchIsOn sw_chkr EssentialUnfoldingsOnly
    is_case_scrutinee	      = switchIsOn sw_chkr SimplCaseScrutinee
    always_inline	      = case guidance of {UnfoldAlways -> True; other -> False}

    ok_to_inline	      = okToInline form occ_info small_enough
    small_enough	      = smallEnoughToInline arg_evals is_case_scrutinee guidance
    arg_evals		      = [is_evald arg | arg <- args, isValArg arg]

    is_evald (VarArg v) = isEvaluated (lookupRhsInfo env v)
    is_evald (LitArg l) = True

#if OMIT_DEFORESTER
    do_deforest = False
#else
    do_deforest = case (getDeforestInfo (getIdInfo var)) of { DoDeforest -> True; _ -> False }
#endif


-- costCentreOk checks that it's ok to inline this thing
-- The time it *isn't* is this:
--
--	f x = let y = E in
--	      scc "foo" (...y...)
--
-- Here y has a subsumed cost centre, and we can't inline it inside "foo",
-- regardless of whether E is a WHNF or not.

costCentreOk cc_encl cc_rhs
  = noCostCentreAttached cc_encl || not (noCostCentreAttached cc_rhs)
\end{code}		   

