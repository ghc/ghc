%
% (c) The AQUA Project, Glasgow University, 1993-1995
%
\section[SimplPgm]{Interface to the ``new'' simplifier}

\begin{code}
#include "HsVersions.h"

module SimplPgm ( simplifyPgm ) where

import PlainCore
import TaggedCore

import Pretty		-- ToDo: rm debugging
IMPORT_Trace

import AbsUniType	( getTyVarMaybe )
import CmdLineOpts	( switchIsOn, intSwitchSet,
			  GlobalSwitch(..), SimplifierSwitch(..)
			)
import Id		( cmpId, externallyVisibleId )
import IdEnv
import IdInfo
import Maybes		( catMaybes, Maybe(..) )
import Outputable
import SimplEnv
import SimplMonad
import Simplify		( simplTopBinds )
import OccurAnal	-- occurAnalyseBinds
#if ! OMIT_FOLDR_BUILD
import NewOccurAnal	-- newOccurAnalyseBinds
#endif
import TyVarEnv		-- ( nullTyVarEnv )
import SplitUniq
import Unique
import Util
\end{code}

\begin{code}
simplifyPgm :: [PlainCoreBinding]		-- input
	    -> (GlobalSwitch->SwitchResult)	-- switch lookup fns (global
	    -> (SimplifierSwitch->SwitchResult) -- and this-simplification-specific)
	    -> SimplCount                       -- info about how many times
						-- each transformation has occurred
	    -> SplitUniqSupply
	    -> ([PlainCoreBinding],	-- output
		 Int,			-- info about how much happened
		 SimplCount)		-- accumulated simpl stats

simplifyPgm binds g_sw_chkr s_sw_chkr simpl_stats us
  = case (splitUniqSupply us)		     of { (s1, s2) ->
    case (initSmpl s1 (simpl_pgm 0 1 binds)) of { ((pgm2, it_count, simpl_stats2), _) ->
    case (tidy_top pgm2 s2) 	    	     of { pgm3 ->
    (pgm3, it_count, combineSimplCounts simpl_stats simpl_stats2) }}}
  where
    global_switch_is_on = switchIsOn g_sw_chkr
    simpl_switch_is_on  = switchIsOn s_sw_chkr

#if OMIT_FOLDR_BUILD
    occur_anal = occurAnalyseBinds
#else
    occur_anal = if simpl_switch_is_on SimplDoNewOccurAnal 
		 then newOccurAnalyseBinds
		 else occurAnalyseBinds
#endif

    max_simpl_iterations
      = case (intSwitchSet s_sw_chkr MaxSimplifierIterations) of
	  Nothing  -> 1    -- default
	  Just max -> max

    simpl_pgm :: Int -> Int -> [PlainCoreBinding] -> SmplM ([PlainCoreBinding], Int, SimplCount)

    simpl_pgm n iterations pgm
      =	-- find out what top-level binders are used,
	-- and prepare to unfold all the "simple" bindings
	-- pprTrace ("\niteration "++show iterations++":\n") (ppr PprDebug pgm) (
	let
	    tagged_pgm = BSCC("OccurBinds")
			 occur_anal pgm global_switch_is_on simpl_switch_is_on
			 ESCC
	in
	      -- do the business
	simplTopBinds (nullSimplEnv s_sw_chkr) tagged_pgm `thenSmpl` \ new_pgm ->

	      -- Quit if we didn't actually do anything; otherwise,
	      -- try again (if suitable flags)

	simplCount				`thenSmpl` \ r ->
	detailedSimplCount			`thenSmpl` \ dr ->
	let
	    show_status = pprTrace "NewSimpl: " (ppAboves [
		ppBesides [ppInt iterations, ppChar '/', ppInt max_simpl_iterations],
		ppStr (showSimplCount dr)
--DEBUG:	, ppAboves (map (pprPlainCoreBinding PprDebug) new_pgm)
		])
	in

	(if global_switch_is_on D_verbose_core2core
	 || simpl_switch_is_on  ShowSimplifierProgress
	 then show_status
	 else id)

	(let stop_now = r == n {-nothing happened-}
		     || (if iterations > max_simpl_iterations then
			    (if max_simpl_iterations > 1 {-otherwise too boring-} then
				trace 
				("NOTE: Simplifier still going after "++show max_simpl_iterations++" iterations; bailing out.")
			     else id)
			    True
			 else 
			    False)
	in
	if stop_now then
	    returnSmpl (new_pgm, iterations, dr)
	else
	    simpl_pgm r (iterations + 1) new_pgm
	)
	-- )
\end{code}

In @tidy_top@, we look for things at the top-level of the form...
\begin{verbatim}
x_local = ....

x_exported = x_local	-- or perhaps...

x_exported = /\ tyvars -> x_local tyvars -- where this is eta-reducible
\end{verbatim}
In cases we find like this, we go {\em backwards} and replace
\tr{x_local} with \tr{x_exported}.  This save a gratuitous jump
(from \tr{x_exported} to \tr{x_local}), and makes strictness
information propagate better.

If more than one exported thing is equal to a local thing (i.e., the
local thing really is shared), then obviously we give up.

Strategy: first collect the info; then make a \tr{Id -> Id} mapping.
Then blast the whole program (LHSs as well as RHSs) with it.

\begin{code}
type BlastEnv = IdEnv Id  -- domain is local Ids; range is exported Ids

not_elem = isn'tIn "undup"

tidy_top :: [PlainCoreBinding] -> SUniqSM [PlainCoreBinding]

tidy_top binds_in
  = if null blast_alist then
	returnSUs binds_in    -- no joy there
    else
	-- pprTrace "undup output length:" (ppInt (length blast_alist)) (
	mapSUs blast binds_in	`thenSUs` \ binds_maybe ->
	returnSUs (catMaybes binds_maybe)
	-- )
  where
    blast_alist  = undup (foldl find_cand [] binds_in)
    blast_id_env = mkIdEnv blast_alist
    blast_val_env= mkIdEnv [ (l, CoVar e) | (l,e) <- blast_alist ]
    blast_all_exps = map snd blast_alist

    ---------
    find_cand blast_list (CoRec _) = blast_list	-- recursively paranoid, as usual

    find_cand blast_list (CoNonRec binder rhs)
      = if not (isExported binder) then
	   blast_list
    	else
	   case rhs_equiv_to_local_var rhs of
	     Nothing    -> blast_list
	     Just local -> (local, binder) : blast_list -- tag it on

    ------------------------------------------
    -- if an Id appears >1 time in the domain,
    -- *all* occurrences must be expunged.
    undup :: [(Id, Id)] -> [(Id, Id)]

    undup blast_list
      = -- pprTrace "undup input length:" (ppInt (length blast_list)) (
	let
	    (singles, dups) = removeDups cmp blast_list
	    list_of_dups    = concat dups
	in
	[ s | s <- singles, s `not_elem` list_of_dups ]
	-- )
      where
        cmp (x,_) (y,_) = x `cmpId` y

    ------------------------------------------
    rhs_equiv_to_local_var (CoVar x)
      = if externallyVisibleId x then Nothing else Just x

    rhs_equiv_to_local_var expr = Nothing
{- MAYBE NOT:
      = case (digForLambdas expr) of { (tyvars, binders, body) ->
	case (collectArgs   body) of { (fun, args) ->
	case fun of
	  CoVar x -> if   null binders
		       && not (isExported x)
		       && tylams_match_tyargs tyvars args then
		       -- may need to chk for "tyvars" occurring in "x"'s type
		        Just x
		     else
		        Nothing
	  _ -> Nothing
        }}
      where
	-- looking for a very restricted special case:
	-- /\ tv1 tv2 ... -> var tv1 tv2 ...

	tylams_match_tyargs []       [] = True
	tylams_match_tyargs (tv:tvs) (TypeArg ty : args)
	  = ASSERT(not (isPrimType ty))
	    case (getTyVarMaybe ty) of
	      Nothing    -> False
	      Just tyvar -> tv == tyvar
	tylams_match_tyargs _ _ = False
-}

    ------------------------------------------
    -- "blast" does the substitution:
    -- returns Nothing  if a binding goes away
    -- returns "Just b" to give back a fixed-up binding

    blast :: PlainCoreBinding -> SUniqSM (Maybe PlainCoreBinding)

    blast (CoRec pairs)
      = mapSUs blast_pr pairs `thenSUs` \ blasted_pairs ->
	returnSUs (Just (CoRec blasted_pairs))
      where
	blast_pr (binder, rhs)
	  = subst_CoreExprUS blast_val_env nullTyVarEnv rhs `thenSUs` \ blasted_rhs ->
	    returnSUs (
	    case lookupIdEnv blast_id_env binder of
	      Just exportee -> (exportee, blasted_rhs)
	      Nothing	    -> (binder,   blasted_rhs)
	    )

    blast (CoNonRec binder rhs)
      = if binder `is_elem` blast_all_exps then
	   returnSUs Nothing -- this binding dies!
    	else
	   subst_CoreExprUS blast_val_env nullTyVarEnv rhs `thenSUs` \ blasted_rhs ->
	   returnSUs (Just (
	   case lookupIdEnv blast_id_env binder of
	     Just exportee -> CoNonRec exportee blasted_rhs
	     Nothing	   -> CoNonRec binder   blasted_rhs
	   ))
      where
	is_elem = isIn "blast"

subst_CoreExprUS e1 e2 rhs us = snd (substCoreExprUS e1 e2 rhs (mkUniqueSupplyGrimily us))
\end{code}
