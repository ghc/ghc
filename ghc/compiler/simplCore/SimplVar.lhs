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

import CgCompInfo	( uNFOLDING_USE_THRESHOLD,
			  uNFOLDING_CON_DISCOUNT_WEIGHT
			)
import CmdLineOpts	( switchIsOn, SimplifierSwitch(..) )
import CoreSyn
import CoreUnfold	( Unfolding(..), UnfoldingGuidance(..), SimpleUnfolding(..),
			  FormSummary,
			  smallEnoughToInline )
import BinderInfo	( BinderInfo, noBinderInfo, okToInline )

import CostCentre	( CostCentre, noCostCentreAttached )
import Id		( idType, getIdInfo, getIdUnfolding, getIdSpecialisation,
			  GenId{-instance Outputable-}
			)
import SpecEnv		( SpecEnv, lookupSpecEnv )
import IdInfo		( DeforestInfo(..) )
import Literal		( isNoRepLit )
import MagicUFs		( applyMagicUnfoldingFun, MagicUnfoldingFun )
import PprStyle		( PprStyle(..) )
import PprType		( GenType{-instance Outputable-} )
import Pretty		( ppBesides, ppStr )
import SimplEnv
import SimplMonad
import TyCon		( tyConFamilySize )
import Type		( isPrimType, getAppDataTyConExpandingDicts, maybeAppDataTyConExpandingDicts )
import Util		( pprTrace, assertPanic, panic )
import Maybes		( maybeToBool )
\end{code}

%************************************************************************
%*									*
\subsection[Simplify-var]{Completing variables}
%*									*
%************************************************************************

This where all the heavy-duty unfolding stuff comes into its own.

\begin{code}
completeVar env var args

  | maybeToBool maybe_magic_result
  = tick MagicUnfold	`thenSmpl_`
    magic_result

  | not do_deforest &&
    maybeToBool maybe_unfolding_info &&
    (always_inline || (ok_to_inline && not essential_unfoldings_only)) && 
    costCentreOk (getEnclosingCC env) (getEnclosingCC unfold_env)
  = tick UnfoldingDone	`thenSmpl_`
    simplExpr unfold_env unf_template args

  | maybeToBool maybe_specialisation
  = tick SpecialisationDone	`thenSmpl_`
    simplExpr (extendTyEnvList env spec_bindings) 
	      spec_template
	      (map TyArg leftover_ty_args ++ remaining_args)

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
		-> Just (occ_info, combineSimplEnv env env_unf, unf)	
	     (_, CoreUnfolding unf)
		-> Just (noBinderInfo, env, unf)

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
    always_inline	      = case guidance of {UnfoldAlways -> True; other -> False}
    ok_to_inline	      = okToInline form 
			      		   occ_info
			      		   small_enough
    small_enough = smallEnoughToInline con_disc unf_thresh arg_evals guidance
    arg_evals = [is_evald arg | arg <- args, isValArg arg]
  
    is_evald (VarArg v) = isEvaluated (lookupRhsInfo env v)
    is_evald (LitArg l) = True

    con_disc   = getSimplIntSwitch sw_chkr SimplUnfoldingConDiscount
    unf_thresh = getSimplIntSwitch sw_chkr SimplUnfoldingUseThreshold

#if OMIT_DEFORESTER
    do_deforest = False
#else
    do_deforest = case (getInfo (getIdInfo var)) of { DoDeforest -> True; _ -> False }
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

