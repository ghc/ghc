%
% (c) The AQUA Project, Glasgow University, 1993-1996
%
\section[SimplVar]{Simplifier stuff related to variables}

\begin{code}
#include "HsVersions.h"

module SimplVar (
	completeVar,
	leastItCouldCost
    ) where

IMP_Ubiq(){-uitous-}
IMPORT_DELOOPER(SmplLoop)		( simplExpr )

import CgCompInfo	( uNFOLDING_USE_THRESHOLD,
			  uNFOLDING_CON_DISCOUNT_WEIGHT
			)
import CmdLineOpts	( intSwitchSet, switchIsOn, SimplifierSwitch(..) )
import CoreSyn
import CoreUnfold	( whnfDetails, UnfoldingDetails(..), UnfoldingGuidance(..),
			  FormSummary(..)
			)
import Id		( idType, getIdInfo,
			  GenId{-instance Outputable-}
			)
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
\end{code}

%************************************************************************
%*									*
\subsection[Simplify-var]{Completing variables}
%*									*
%************************************************************************

This where all the heavy-duty unfolding stuff comes into its own.


completeVar env var args
  | has_magic_unfolding
  = tick MagicUnfold	`thenSmpl_`
    doMagicUnfold

  | has_unfolding && ok_to_inline
  = tick UnfoldingDone	`thenSmpl_`
    simplExpr env the_unfolding args

  | has_specialisation
  = tick SpecialisationDone	`thenSmpl_`
    simplExpr (extendTyEnvList env spec_bindings) 
	      the_specialisation 
	      remaining_args

  | otherwise
  = mkGenApp (Var var) args

  where
    unfolding = lookupUnfolding env var

    (has_magic_unfolding, do_magic_unfold)
	= case unfolding of
	    MagicForm str magic_fn
		   
\begin{code}
completeVar :: SimplEnv -> OutId -> [OutArg] -> SmplM OutExpr

completeVar env var args
  = let
	boring_result = mkGenApp (Var var) args
    in
    case (lookupUnfolding env var) of

      GenForm form_summary template guidance
	-> considerUnfolding env var args
			     (False{-ToDo:!-}{-txt_occ-}) form_summary template guidance

      MagicForm str magic_fun
	->  applyMagicUnfoldingFun magic_fun env args `thenSmpl` \ result ->
	    case result of
	      Nothing	        -> returnSmpl boring_result
	      Just magic_result ->
		{- pprTrace "MagicForm:- " (ppAbove
			(ppBesides [
			   ppr PprDebug var,
			   ppr PprDebug args])
			(ppBesides [
				ppStr "AFTER    :- ",
			   ppr PprDebug magic_result])) (returnSmpl ()) `thenSmpl` \ () ->
		-}
		tick MagicUnfold		`thenSmpl_`
		returnSmpl magic_result

-- LATER:
--    IWantToBeINLINEd _ -> returnSmpl boring_result

      other -> returnSmpl boring_result
\end{code}


%************************************************************************
%*									*
\subsection[considerUnfolding]{Given all the info, do (not) do the unfolding}
%*									*
%************************************************************************

We have very limited information about an unfolding expression: (1)~so
many type arguments and so many value arguments expected---for our
purposes here, we assume we've got those.  (2)~A ``size'' or ``cost,''
a single integer.  (3)~An ``argument info'' vector.  For this, what we
have at the moment is a Boolean per argument position that says, ``I
will look with great favour on an explicit constructor in this
position.''

Assuming we have enough type- and value arguments (if not, we give up
immediately), then we see if the ``discounted size'' is below some
(semi-arbitrary) threshold.  It works like this: for every argument
position where we're looking for a constructor AND WE HAVE ONE in our
hands, we get a (again, semi-arbitrary) discount [proportion to the
number of constructors in the type being scrutinized].

\begin{code}
considerUnfolding
	:: SimplEnv
	-> OutId		-- Id we're thinking about
	-> [OutArg]		-- Applied to these
	-> Bool			-- If True then *always* inline,
				-- because it's the only one
	-> FormSummary
	-> InExpr		-- Template for unfolding;
	-> UnfoldingGuidance	-- To help us decide...
	-> SmplM CoreExpr	-- Result!

considerUnfolding env var args txt_occ form_summary template guidance
  | switchIsOn sw_chkr EssentialUnfoldingsOnly
  = dont_go_for_it -- we're probably in a hurry in this simpl round...

  | do_deforest
  = pprTrace "" (ppBesides [ppStr "not attempting to unfold `",
				    ppr PprDebug var,
				    ppStr "' due to DEFOREST pragma"])
			dont_go_for_it

  | txt_occ
  = go_for_it

  | (case form_summary of {BottomForm -> True; other -> False} &&
    not (any isPrimType [ ty | (TyArg ty) <- args ]))
		-- Always inline bottoming applications, unless
		-- there's a primitive type lurking around...
  = go_for_it

  | otherwise
  =
    -- If this is a deforestable Id, then don't unfold it (the deforester
    -- will do it).

    case getInfo (getIdInfo var) of {
       DoDeforest -> pprTrace "" (ppBesides [ppStr "not unfolding `",
				    ppr PprDebug var,
				    ppStr "' due to DEFOREST pragma"])
			dont_go_for_it;
       Don'tDeforest ->

    case guidance of
      UnfoldNever  -> dont_go_for_it

      UnfoldAlways -> go_for_it

      EssentialUnfolding -> go_for_it

      UnfoldIfGoodArgs m_tys_wanted n_vals_wanted is_con_vec size
	-> if m_tys_wanted > no_tyargs
	   || n_vals_wanted > no_valargs then
	      --pprTrace "dont_go_for_it1:" (ppAbove (ppr PprDebug guidance) (ppr PprDebug var))
	      dont_go_for_it

	   else if n_vals_wanted == 0
		&& rhs_looks_like_a_Con then
	      -- we are very keen on inlining data values
	      -- (see comments elsewhere); we ignore any size issues!
	      go_for_it

	   else -- we try the fun stuff
	      let
		  discounted_size
		    = discountedCost env con_discount size no_valargs is_con_vec valargs
	      in
	      if discounted_size <= unfold_use_threshold then
		  go_for_it
	      else
		  --pprTrace "dont_go_for_it2:" (ppCat [ppr PprDebug var, ppInt size, ppInt discounted_size, ppInt unfold_use_threshold, ppr PprDebug guidance])
		  dont_go_for_it
    }
  where
    sw_chkr = getSwitchChecker env

    unfold_use_threshold
      = case (intSwitchSet sw_chkr SimplUnfoldingUseThreshold) of
	  Nothing -> uNFOLDING_USE_THRESHOLD
	  Just xx -> xx

    con_discount  -- ToDo: ************ get from a switch *********
      = uNFOLDING_CON_DISCOUNT_WEIGHT

    (_, _, tyargs, valargs) = collectArgs args_in_dummy_expr
    no_tyargs  = length tyargs
    no_valargs = length valargs
    args_in_dummy_expr = mkGenApp (Var (panic "SimplVar.dummy")) args
    -- we concoct this dummy expr, just so we can use collectArgs
    -- (rather than make up a special-purpose bit of code)

    rhs_looks_like_a_Con
      = let
	    (_,_,val_binders,body) = collectBinders template
    	in
    	case (val_binders, body) of
    	  ([], Con _ _) -> True
	  other -> False

    dont_go_for_it = returnSmpl (mkGenApp (Var var) args)

    go_for_it      = --pprTrace "unfolding:" (ppCat [ppr PprDebug var, ppChar ':', ppr PprDebug template]) (
		     tick UnfoldingDone		`thenSmpl_`
		     simplExpr env template args
		     --)

#if OMIT_DEFORESTER
    do_deforest = False
#else
    do_deforest = case (getInfo (getIdInfo var)) of { DoDeforest -> True; _ -> False }
#endif
\end{code}

\begin{code}
type ArgInfoVector = [Bool]

discountedCost
	:: SimplEnv	    -- so we can look up things about the args
	-> Int		    -- the discount for a "constructor" hit;
			    -- we multiply by the # of cons in the type.
	-> Int		    -- the size/cost of the expr
	-> Int		    -- the number of val args (== length args)
	-> ArgInfoVector    -- what we know about the *use* of the arguments
	-> [OutArg]	    -- *an actual set of value arguments*!
	-> Int

    -- If we apply an expression (usually a function) of given "costs"
    -- to a particular set of arguments (possibly none), what will
    -- the resulting expression "cost"?

discountedCost env con_discount_weight size no_args is_con_vec args
  = ASSERT(no_args == length args)
    disc (size - no_args) is_con_vec args
	-- we start w/ a "discount" equal to the # of args...
  where
    disc size [] _ = size
    disc size _ [] = size

    disc size (want_con_here:want_cons) (arg:rest_args)
      = let
	    full_price	         = disc size
	    take_something_off v = let
				     (tycon, _, _) = getAppDataTyConExpandingDicts (idType v)
				     no_cons = tyConFamilySize tycon
				     reduced_size
				       = size - (no_cons * con_discount_weight)
				   in
				   disc reduced_size
	in
	(if not want_con_here then
	    full_price
	else
	    case arg of
	      LitArg _ 					     -> full_price
	      VarArg v | whnfDetails (lookupUnfolding env v) -> take_something_off v
		       | otherwise			     -> full_price

	) want_cons rest_args
\end{code}

We use this one to avoid exporting inlinings that we ``couldn't possibly
use'' on the other side.  Can be overridden w/ flaggery.
\begin{code}
leastItCouldCost
	:: Int
	-> Int		    -- the size/cost of the expr
	-> Int		    -- number of value args
	-> ArgInfoVector    -- what we know about the *use* of the arguments
	-> [Type]	    -- NB: actual arguments *not* looked at;
			    -- but we know their types
	-> Int

leastItCouldCost con_discount_weight size no_val_args is_con_vec arg_tys
  = ASSERT(no_val_args == length arg_tys)
    disc (size - no_val_args) is_con_vec arg_tys
	-- we start w/ a "discount" equal to the # of args...
  where
    -- ToDo: rather sad that this isn't commoned-up w/ the one above...

    disc size [] _ = size
    disc size _ [] = size

    disc size (want_con_here:want_cons) (arg_ty:rest_arg_tys)
      = let
	    take_something_off tycon
	      = let
		    no_cons = tyConFamilySize tycon

		    reduced_size
		      = size - (no_cons * con_discount_weight)
		in
		reduced_size
	in
	if not want_con_here then
	    disc size want_cons rest_arg_tys
	else
	    case (maybeAppDataTyConExpandingDicts arg_ty, isPrimType arg_ty) of
	      (Just (tycon, _, _), False) ->
		disc (take_something_off tycon) want_cons rest_arg_tys

	      other -> disc size want_cons rest_arg_tys
\end{code}

