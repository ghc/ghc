%
% (c) The AQUA Project, Glasgow University, 1993-1995
%
\section[SimplVar]{Simplifier stuff related to variables}

\begin{code}
#include "HsVersions.h"

module SimplVar (
	completeVar,
	leastItCouldCost
    ) where

IMPORT_Trace

import SimplMonad
import SimplEnv
import PlainCore
import TaggedCore
import BasicLit		( isNoRepLit )

import AbsUniType	( getUniDataTyCon, getUniDataTyCon_maybe,
			  getTyConFamilySize, isPrimType
			)
import BinderInfo	( oneTextualOcc, oneSafeOcc )
import CgCompInfo	( uNFOLDING_USE_THRESHOLD,
			  uNFOLDING_CON_DISCOUNT_WEIGHT
			)
import CmdLineOpts	( switchIsOn, intSwitchSet, SimplifierSwitch(..) )
import Id		( getIdUniType, getIdInfo )
import IdInfo
import Maybes		( maybeToBool, Maybe(..) )
import Simplify		( simplExpr )
import SimplUtils	( simplIdWantsToBeINLINEd )
import MagicUFs
import Pretty
import Util
\end{code}

%************************************************************************
%*									*
\subsection[Simplify-var]{Completing variables}
%*									*
%************************************************************************

This where all the heavy-duty unfolding stuff comes into its own.

\begin{code}
completeVar :: SimplEnv -> OutId -> [OutArg] -> SmplM OutExpr

completeVar env var args
  = let
	boring_result = applyToArgs (CoVar var) args
    in
    case (lookupUnfolding env var) of
     
      LiteralForm lit 
	| not (isNoRepLit lit) 
		-- Inline literals, if they aren't no-repish things
	-> ASSERT( null args )
	   returnSmpl (CoLit lit)

      ConstructorForm con ty_args val_args
		-- Always inline constructors.
		-- See comments before completeLetBinding
	-> ASSERT( null args )
	   returnSmpl (CoCon con ty_args val_args)	

      GeneralForm txt_occ form_summary template guidance 
	-> considerUnfolding env var args
			     txt_occ form_summary template guidance

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

      IWantToBeINLINEd _ -> returnSmpl boring_result

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
	-> SmplM PlainCoreExpr	-- Result!

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
    not (any isPrimType [ ty | (TypeArg ty) <- args ]))
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
		&& rhs_looks_like_a_CoCon then
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

    (tyargs, valargs, args_left) = decomposeArgs args
    no_tyargs  = length tyargs
    no_valargs = length valargs

    rhs_looks_like_a_CoCon
      = let
	    (_,val_binders,body) = digForLambdas template
    	in
    	case (val_binders, body) of
    	  ([], CoCon _ _ _) -> True
	  other -> False

    dont_go_for_it = returnSmpl (applyToArgs (CoVar var) args)

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
	-> [OutAtom]	    -- *an actual set of value arguments*!
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
				     (tycon, _, _) = getUniDataTyCon (getIdUniType v)
				     no_cons = case (getTyConFamilySize tycon) of
						 Just n -> n
				     reduced_size
				       = size - (no_cons * con_discount_weight)
				   in
				   disc reduced_size
	in
	(if not want_con_here then
	    full_price
	else
	    case arg of
	      CoLitAtom _ -> full_price
	      CoVarAtom v -> case lookupUnfolding env v of
			       ConstructorForm _ _ _ -> take_something_off v
			       other_form   	     -> full_price

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
	-> [UniType]	    -- NB: actual arguments *not* looked at;
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
		    no_cons = case (getTyConFamilySize tycon) of { Just n -> n }

		    reduced_size
		      = size - (no_cons * con_discount_weight)
		in
		reduced_size
	in
	if not want_con_here then
	    disc size want_cons rest_arg_tys
	else
	    case (getUniDataTyCon_maybe arg_ty, isPrimType arg_ty) of
	      (Just (tycon, _, _), False) ->
	        disc (take_something_off tycon) want_cons rest_arg_tys

	      other -> disc size want_cons rest_arg_tys
\end{code}

