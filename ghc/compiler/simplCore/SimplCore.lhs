%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SimplCore]{Driver for simplifying @Core@ programs}

\begin{code}
module SimplCore ( core2core ) where

#include "HsVersions.h"

import CmdLineOpts	( CoreToDo(..), SimplifierSwitch(..), 
			  SwitchResult(..), intSwitchSet,
                          opt_UsageSPOn,
			  DynFlags, DynFlag(..), dopt
			)
import CoreLint		( beginPass, endPass )
import CoreSyn
import CSE		( cseProgram )
import Rules		( RuleBase, ProtoCoreRule(..), pprProtoCoreRule, prepareLocalRuleBase,
                          prepareOrphanRuleBase, unionRuleBase, localRule )
import CoreUnfold
import PprCore		( pprCoreBindings )
import OccurAnal	( occurAnalyseBinds )
import CoreUtils	( exprIsTrivial, etaReduceExpr, coreBindsSize )
import Simplify		( simplTopBinds, simplExpr )
import SimplUtils	( simplBinders )
import SimplMonad
import ErrUtils		( dumpIfSet, dumpIfSet_dyn )
import FloatIn		( floatInwards )
import FloatOut		( floatOutwards )
import Id		( isDataConWrapId )
import VarSet
import LiberateCase	( liberateCase )
import SAT		( doStaticArgs )
import Specialise	( specProgram)
import UsageSPInf       ( doUsageSPInf )
import StrictAnal	( saBinds )
import WorkWrap	        ( wwTopBinds )
import CprAnalyse       ( cprAnalyse )

import UniqSupply	( UniqSupply, mkSplitUniqSupply, splitUniqSupply )
import IO		( hPutStr, stderr )
import Outputable

import List             ( partition )
\end{code}

%************************************************************************
%*									*
\subsection{The driver for the simplifier}
%*									*
%************************************************************************

\begin{code}
core2core :: DynFlags 
	  -> [CoreToDo]		-- Spec of what core-to-core passes to do
	  -> [CoreBind]		-- Binds in
	  -> [ProtoCoreRule]	-- Rules in
	  -> IO ([CoreBind], RuleBase)  -- binds, local orphan rules out

core2core dflags core_todos binds rules
  = do
	us <-  mkSplitUniqSupply 's'
	let (cp_us, ru_us) = splitUniqSupply us

        let (local_rules, imported_rules) = partition localRule rules

        better_local_rules <- simplRules dflags ru_us local_rules binds

        let (binds1, local_rule_base) = prepareLocalRuleBase binds better_local_rules
            imported_rule_base        = prepareOrphanRuleBase imported_rules

	-- Do the main business
	(stats, processed_binds, processed_local_rules)
            <- doCorePasses dflags (zeroSimplCount dflags) cp_us binds1 local_rule_base
			    imported_rule_base Nothing core_todos

	dumpIfSet_dyn dflags Opt_D_dump_simpl_stats
		  "Grand total simplifier statistics"
		  (pprSimplCount stats)

	-- Return results
        -- We only return local orphan rules, i.e., local rules not attached to an Id
	return (processed_binds, processed_local_rules)


doCorePasses :: DynFlags
	     -> SimplCount      -- simplifier stats
             -> UniqSupply      -- uniques
             -> [CoreBind]      -- local binds in (with rules attached)
             -> RuleBase        -- local orphan rules
             -> RuleBase        -- imported and builtin rules
             -> Maybe RuleBase  -- combined rulebase, or Nothing to ask for it to be rebuilt
             -> [CoreToDo]      -- which passes to do
             -> IO (SimplCount, [CoreBind], RuleBase)  -- stats, binds, local orphan rules

doCorePasses dflags stats us binds lrb irb rb0 []
  = return (stats, binds, lrb)

doCorePasses dflags stats us binds lrb irb rb0 (to_do : to_dos) 
  = do
	let (us1, us2) = splitUniqSupply us

        -- recompute rulebase if necessary
        let rb         = maybe (irb `unionRuleBase` lrb) id rb0

	(stats1, binds1, mlrb1) <- doCorePass dflags us1 binds lrb rb to_do

        -- request rulebase recomputation if pass returned a new local rulebase
        let (lrb1,rb1) = maybe (lrb, Just rb) (\ lrb1 -> (lrb1, Nothing)) mlrb1

	doCorePasses dflags (stats `plusSimplCount` stats1) us2 binds1 lrb1 irb rb1 to_dos

doCorePass dfs us binds lrb rb (CoreDoSimplify sw_chkr) 
   = _scc_ "Simplify"      simplifyPgm dfs rb sw_chkr us binds
doCorePass dfs us binds lrb rb CoreCSE		        
   = _scc_ "CommonSubExpr" noStats dfs (cseProgram dfs binds)
doCorePass dfs us binds lrb rb CoreLiberateCase	        
   = _scc_ "LiberateCase"  noStats dfs (liberateCase dfs binds)
doCorePass dfs us binds lrb rb CoreDoFloatInwards       
   = _scc_ "FloatInwards"  noStats dfs (floatInwards dfs binds)
doCorePass dfs us binds lrb rb (CoreDoFloatOutwards f)  
   = _scc_ "FloatOutwards" noStats dfs (floatOutwards dfs f us binds)
doCorePass dfs us binds lrb rb CoreDoStaticArgs	        
   = _scc_ "StaticArgs"    noStats dfs (doStaticArgs us binds)
doCorePass dfs us binds lrb rb CoreDoStrictness	        
   = _scc_ "Stranal"       noStats dfs (saBinds dfs binds)
doCorePass dfs us binds lrb rb CoreDoWorkerWrapper      
   = _scc_ "WorkWrap"      noStats dfs (wwTopBinds dfs us binds)
doCorePass dfs us binds lrb rb CoreDoSpecialising       
   = _scc_ "Specialise"    noStats dfs (specProgram dfs us binds)
doCorePass dfs us binds lrb rb CoreDoCPResult	        
   = _scc_ "CPResult"      noStats dfs (cprAnalyse dfs binds)
doCorePass dfs us binds lrb rb CoreDoPrintCore	        
   = _scc_ "PrintCore"     noStats dfs (printCore binds)
doCorePass dfs us binds lrb rb CoreDoUSPInf             
   = _scc_ "CoreUsageSPInf" noStats dfs (doUsageSPInf dfs us binds lrb)
doCorePass dfs us binds lrb rb CoreDoGlomBinds	        
   = noStats dfs (glomBinds dfs binds)

printCore binds = do dumpIfSet True "Print Core"
			       (pprCoreBindings binds)
		     return binds

-- most passes return no stats and don't change rules
noStats dfs thing = do { binds <- thing; return (zeroSimplCount dfs, binds, Nothing) }
\end{code}



%************************************************************************
%*									*
\subsection{Dealing with rules}
%*									*
%************************************************************************

We must do some gentle simplification on the template (but not the RHS)
of each rule.  The case that forced me to add this was the fold/build rule,
which without simplification looked like:
	fold k z (build (/\a. g a))  ==>  ...
This doesn't match unless you do eta reduction on the build argument.

\begin{code}
simplRules :: DynFlags -> UniqSupply -> [ProtoCoreRule] -> [CoreBind] 
	   -> IO [ProtoCoreRule]
simplRules dflags us rules binds
  = do  let (better_rules,_) 
               = initSmpl dflags sw_chkr us bind_vars black_list_all 
                          (mapSmpl simplRule rules)
	
	dumpIfSet_dyn dflags Opt_D_dump_rules
		  "Transformation rules"
		  (vcat (map pprProtoCoreRule better_rules))

	return better_rules
  where
    black_list_all v = not (isDataConWrapId v)
		-- This stops all inlining except the
		-- wrappers for data constructors

    sw_chkr any = SwBool False			-- A bit bogus

	-- Boringly, we need to gather the in-scope set.
	-- Typically this thunk won't even be force, but the test in
	-- simpVar fails if it isn't right, and it might conceivably matter
    bind_vars = foldr (unionVarSet . mkVarSet . bindersOf) emptyVarSet binds


simplRule rule@(ProtoCoreRule is_local id (BuiltinRule _))
  = returnSmpl rule
simplRule rule@(ProtoCoreRule is_local id (Rule name bndrs args rhs))
  | not is_local
  = returnSmpl rule	-- No need to fiddle with imported rules
  | otherwise
  = simplBinders bndrs			$ \ bndrs' -> 
    mapSmpl simpl_arg args		`thenSmpl` \ args' ->
    simplExpr rhs			`thenSmpl` \ rhs' ->
    returnSmpl (ProtoCoreRule is_local id (Rule name bndrs' args' rhs'))

simpl_arg e 
--  I've seen rules in which a LHS like 
--	augment g (build h) 
-- turns into
--	augment (\a. g a) (build h)
-- So it's a help to eta-reduce the args as we simplify them.
-- Otherwise we don't match when given an argument like
--	(\a. h a a)
  = simplExpr e 	`thenSmpl` \ e' ->
    returnSmpl (etaReduceExpr e')
\end{code}

\begin{code}
glomBinds :: DynFlags -> [CoreBind] -> IO [CoreBind]
-- Glom all binds together in one Rec, in case any
-- transformations have introduced any new dependencies
--
-- NB: the global invariant is this:
--	*** the top level bindings are never cloned, and are always unique ***
--
-- We sort them into dependency order, but applying transformation rules may
-- make something at the top refer to something at the bottom:
--	f = \x -> p (q x)
--	h = \y -> 3
--	
--	RULE:  p (q x) = h x
--
-- Applying this rule makes f refer to h, 
-- although it doesn't appear to in the source program.  
-- This pass lets us control where it happens.
--
-- NOTICE that this cannot happen for rules whose head is a locally-defined
-- function.  It only happens for rules whose head is an imported function
-- (p in the example above).  So, for example, the rule had been
--	RULE: f (p x) = h x
-- then the rule for f would be attached to f itself (in its IdInfo) 
-- by prepareLocalRuleBase and h would be regarded by the occurrency 
-- analyser as free in f.

glomBinds dflags binds
  = do { beginPass dflags "GlomBinds" ;
	 let { recd_binds = [Rec (flattenBinds binds)] } ;
	 return recd_binds }
	-- Not much point in printing the result... 
	-- just consumes output bandwidth
\end{code}

%************************************************************************
%*									*
\subsection{The driver for the simplifier}
%*									*
%************************************************************************

\begin{code}
simplifyPgm :: DynFlags 
	    -> RuleBase
	    -> (SimplifierSwitch -> SwitchResult)
	    -> UniqSupply
	    -> [CoreBind]				    -- Input
	    -> IO (SimplCount, [CoreBind], Maybe RuleBase)  -- New bindings

simplifyPgm dflags (imported_rule_ids, rule_lhs_fvs) 
	    sw_chkr us binds
  = do {
	beginPass dflags "Simplify";

	(termination_msg, it_count, counts_out, binds') 
	   <- iteration us 1 (zeroSimplCount dflags) binds;

	dumpIfSet (dopt Opt_D_verbose_core2core dflags 
                   && dopt Opt_D_dump_simpl_stats dflags)
		  "Simplifier statistics"
		  (vcat [text termination_msg <+> text "after" <+> ppr it_count <+> text "iterations",
			 text "",
			 pprSimplCount counts_out]);

	endPass dflags "Simplify" 
		(dopt Opt_D_verbose_core2core dflags 
                 && not (dopt Opt_D_dump_simpl_iterations dflags))
		binds' ;

	return (counts_out, binds', Nothing)
    }
  where
    max_iterations = getSimplIntSwitch sw_chkr MaxSimplifierIterations
    black_list_fn  = blackListed rule_lhs_fvs (intSwitchSet sw_chkr SimplInlinePhase)

    iteration us iteration_no counts binds
      -- Try and force thunks off the binds; significantly reduces
      -- space usage, especially with -O.  JRS, 000620.
      | let sz = coreBindsSize binds in sz == sz
      = do {
		-- Occurrence analysis
	   let { tagged_binds = _scc_ "OccAnal" occurAnalyseBinds binds } ;

	   dumpIfSet_dyn dflags Opt_D_dump_occur_anal "Occurrence analysis"
		     (pprCoreBindings tagged_binds);

		-- SIMPLIFY
		-- We do this with a *case* not a *let* because lazy pattern
		-- matching bit us with bad space leak!
		-- With a let, we ended up with
		--   let
		--	t = initSmpl ...
		--	counts' = snd t
		--   in
		-- 	case t of {(_,counts') -> if counts'=0 then ...
		-- So the conditional didn't force counts', because the
		-- selection got duplicated.  Sigh!
	   case initSmpl dflags sw_chkr us1 imported_rule_ids black_list_fn 
			 (simplTopBinds tagged_binds)
	  	of { (binds', counts') -> do {
			-- The imported_rule_ids are used by initSmpl to initialise
			-- the in-scope set.  That way, the simplifier will change any
			-- occurrences of the imported id to the one in the imported_rule_ids
			-- set, which are decorated with their rules.

	   let { all_counts = counts `plusSimplCount` counts' } ;

		-- Stop if nothing happened; don't dump output
	   if isZeroSimplCount counts' then
		return ("Simplifier reached fixed point", iteration_no, all_counts, binds')
	   else do {

		-- Dump the result of this iteration
	   dumpIfSet_dyn dflags Opt_D_dump_simpl_iterations
		     ("Simplifier iteration " ++ show iteration_no 
		      ++ " out of " ++ show max_iterations)
		     (pprSimplCount counts') ;

	   if dopt Opt_D_dump_simpl_iterations dflags then
		endPass dflags 
                        ("Simplifier iteration " ++ show iteration_no ++ " result")
			(dopt Opt_D_verbose_core2core dflags)
			binds'
	   else
		return [] ;

		-- Stop if we've run out of iterations
	   if iteration_no == max_iterations then
		do {
#ifdef DEBUG
		    if  max_iterations > 2 then
			    hPutStr stderr ("NOTE: Simplifier still going after " ++ 
				    show max_iterations ++ 
				    " iterations; bailing out.\n")
		    else 
#endif
			return ();

		    return ("Simplifier baled out", iteration_no, all_counts, binds')
		}

		-- Else loop
  	   else iteration us2 (iteration_no + 1) all_counts binds'
	}  } } }
      where
  	  (us1, us2) = splitUniqSupply us
\end{code}
