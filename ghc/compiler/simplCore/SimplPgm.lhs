%
% (c) The AQUA Project, Glasgow University, 1993-1996
%
\section[SimplPgm]{Interface to the simplifier}

\begin{code}
#include "HsVersions.h"

module SimplPgm ( simplifyPgm ) where

IMP_Ubiq(){-uitous-}

import CmdLineOpts	( opt_D_verbose_core2core,
			  switchIsOn, intSwitchSet, SimplifierSwitch(..)
			)
import CoreSyn
import CoreUtils	( substCoreExpr )
import Id		( externallyVisibleId,
			  mkIdEnv, lookupIdEnv, IdEnv(..),
			  GenId{-instance Ord3-}
			)
import Maybes		( catMaybes )
import Name		( isExported )
import OccurAnal	( occurAnalyseBinds )
import Pretty		( ppAboves, ppBesides, ppInt, ppChar, ppStr )
import SimplEnv
import SimplMonad
import Simplify		( simplTopBinds )
import TyVar		( nullTyVarEnv, TyVarEnv(..) )
import UniqSupply	( thenUs, returnUs, mapUs, splitUniqSupply, UniqSM(..) )
import Util		( isIn, isn'tIn, removeDups, pprTrace )
\end{code}

\begin{code}
simplifyPgm :: [CoreBinding]	-- input
	    -> (SimplifierSwitch->SwitchResult)
	    -> SimplCount	-- info about how many times
				-- each transformation has occurred
	    -> UniqSupply
	    -> ([CoreBinding],	-- output
		 Int,		-- info about how much happened
		 SimplCount)	-- accumulated simpl stats

simplifyPgm binds s_sw_chkr simpl_stats us
  = case (splitUniqSupply us)		     of { (s1, s2) ->
    case (initSmpl s1 (simpl_pgm 0 1 binds)) of { ((pgm2, it_count, simpl_stats2), _) ->
    case (tidy_top pgm2 s2) 	    	     of { pgm3 ->
    (pgm3, it_count, combineSimplCounts simpl_stats simpl_stats2) }}}
  where
    simpl_switch_is_on  = switchIsOn s_sw_chkr

    occur_anal = occurAnalyseBinds

    max_simpl_iterations
      = case (intSwitchSet s_sw_chkr MaxSimplifierIterations) of
	  Nothing  -> 1    -- default
	  Just max -> max

    simpl_pgm :: Int -> Int -> [CoreBinding] -> SmplM ([CoreBinding], Int, SimplCount)

    simpl_pgm n iterations pgm
      =	-- find out what top-level binders are used,
	-- and prepare to unfold all the "simple" bindings
	let
	    tagged_pgm = occur_anal pgm simpl_switch_is_on
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
--DEBUG:	, ppAboves (map (pprCoreBinding PprDebug) new_pgm)
		])
	in

	(if opt_D_verbose_core2core
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

tidy_top :: [CoreBinding] -> UniqSM [CoreBinding]

tidy_top binds_in
  = if null blast_alist then
	returnUs binds_in    -- no joy there
    else
	mapUs blast binds_in	`thenUs` \ binds_maybe ->
	returnUs (catMaybes binds_maybe)
  where
    blast_alist  = undup (foldl find_cand [] binds_in)
    blast_id_env = mkIdEnv blast_alist
    blast_val_env= mkIdEnv [ (l, Var e) | (l,e) <- blast_alist ]
    blast_all_exps = map snd blast_alist

    ---------
    find_cand blast_list (Rec _) = blast_list	-- recursively paranoid, as usual

    find_cand blast_list (NonRec binder rhs)
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
      = let
	    (singles, dups) = removeDups compare blast_list
	    list_of_dups    = concat dups
	in
	[ s | s <- singles, s `not_elem` list_of_dups ]
      where
	compare (x,_) (y,_) = x `cmp` y

    ------------------------------------------
    rhs_equiv_to_local_var (Var x)
      = if externallyVisibleId x then Nothing else Just x

    rhs_equiv_to_local_var expr = Nothing

    ------------------------------------------
    -- "blast" does the substitution:
    -- returns Nothing  if a binding goes away
    -- returns "Just b" to give back a fixed-up binding

    blast :: CoreBinding -> UniqSM (Maybe CoreBinding)

    blast (Rec pairs)
      = mapUs blast_pr pairs `thenUs` \ blasted_pairs ->
	returnUs (Just (Rec blasted_pairs))
      where
	blast_pr (binder, rhs)
	  = substCoreExpr blast_val_env nullTyVarEnv rhs `thenUs` \ new_rhs ->
	    returnUs (
	    case (lookupIdEnv blast_id_env binder) of
	      Just exportee -> (exportee, new_rhs)
	      Nothing	    -> (binder,   new_rhs)
	    )

    blast (NonRec binder rhs)
      = if binder `is_elem` blast_all_exps then
	   returnUs Nothing -- this binding dies!
    	else
	   substCoreExpr blast_val_env nullTyVarEnv rhs `thenUs` \ new_rhs ->
	   returnUs (Just (
	   case (lookupIdEnv blast_id_env binder) of
	     Just exportee -> NonRec exportee new_rhs
	     Nothing	   -> NonRec binder   new_rhs
	   ))
      where
	is_elem = isIn "blast"
\end{code}
