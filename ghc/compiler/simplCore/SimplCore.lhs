%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SimplCore]{Driver for simplifying @Core@ programs}

\begin{code}
module SimplCore ( core2core, simplifyExpr ) where

#include "HsVersions.h"

import CmdLineOpts	( CoreToDo(..), SimplifierSwitch(..), 
			  SwitchResult(..), intSwitchSet,
			  DynFlags, DynFlag(..), dopt, dopt_CoreToDo
			)
import CoreLint		( showPass, endPass )
import CoreSyn
import CoreFVs		( ruleRhsFreeVars )
import HscTypes		( PersistentCompilerState(..),
			  PackageRuleBase, HomeSymbolTable, IsExported, ModDetails(..)
			)
import CSE		( cseProgram )
import Rules		( RuleBase, emptyRuleBase, ruleBaseFVs, ruleBaseIds, 
			  extendRuleBaseList, addRuleBaseFVs )
import Module		( moduleEnvElts )
import CoreUnfold
import PprCore		( pprCoreBindings, pprIdCoreRule, pprCoreExpr )
import OccurAnal	( occurAnalyseBinds )
import CoreUtils	( etaReduceExpr, coreBindsSize )
import Simplify		( simplTopBinds, simplExpr )
import SimplUtils	( simplBinders )
import SimplMonad
import ErrUtils		( dumpIfSet, dumpIfSet_dyn )
import FloatIn		( floatInwards )
import FloatOut		( floatOutwards )
import Id		( idName, isDataConWrapId, setIdNoDiscard, isLocalId )
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

import Maybes		( orElse )
import List             ( partition )
\end{code}

%************************************************************************
%*									*
\subsection{The driver for the simplifier}
%*									*
%************************************************************************

\begin{code}
core2core :: DynFlags		-- includes spec of what core-to-core passes to do
	  -> PersistentCompilerState
	  -> HomeSymbolTable
	  -> IsExported
	  -> [CoreBind]		-- Binds in
	  -> [IdCoreRule]	-- Rules in
	  -> IO ([CoreBind], [IdCoreRule])  -- binds, local orphan rules out

core2core dflags pcs hst is_exported binds rules
  = do
        let core_todos    = dopt_CoreToDo dflags
	let pkg_rule_base = pcs_rules pcs		-- Rule-base accumulated from imported packages

	us <-  mkSplitUniqSupply 's'
	let (cp_us, ru_us) = splitUniqSupply us

		-- COMPUTE THE RULE BASE TO USE
	(rule_base, local_rule_stuff, orphan_rules)
		<- prepareRules dflags pkg_rule_base hst ru_us binds rules

		-- PREPARE THE BINDINGS
	let binds1 = updateBinders local_rule_stuff is_exported binds

		-- DO THE BUSINESS
	(stats, processed_binds)
		<- doCorePasses dflags rule_base (zeroSimplCount dflags) cp_us binds1 core_todos

	dumpIfSet_dyn dflags Opt_D_dump_simpl_stats
		  "Grand total simplifier statistics"
		  (pprSimplCount stats)

	-- Return results
        -- We only return local orphan rules, i.e., local rules not attached to an Id
	-- The bindings cotain more rules, embedded in the Ids
	return (processed_binds, orphan_rules)


simplifyExpr :: DynFlags -- includes spec of what core-to-core passes to do
	     -> PersistentCompilerState
	     -> HomeSymbolTable
	     -> CoreExpr
	     -> IO CoreExpr
simplifyExpr dflags pcs hst expr
  = do	{
	; showPass dflags "Simplify"

	; us <-  mkSplitUniqSupply 's'

	; let (expr', counts) = initSmpl dflags sw_chkr us emptyVarSet black_list_all 	
		                         (simplExpr expr)

	; dumpIfSet_dyn dflags Opt_D_dump_simpl "Simplfied expression"
			(pprCoreExpr expr')

	; return expr'
	}
  where
    sw_chkr any	     = SwBool False	-- A bit bogus
    black_list_all v = True		-- Black list everything


doCorePasses :: DynFlags
             -> RuleBase        -- the main rule base
	     -> SimplCount      -- simplifier stats
             -> UniqSupply      -- uniques
             -> [CoreBind]      -- local binds in (with rules attached)
             -> [CoreToDo]      -- which passes to do
             -> IO (SimplCount, [CoreBind])  -- stats, binds, local orphan rules

doCorePasses dflags rb stats us binds []
  = return (stats, binds)

doCorePasses dflags rb stats us binds (to_do : to_dos) 
  = do
	let (us1, us2) = splitUniqSupply us

	(stats1, binds1) <- doCorePass dflags rb us1 binds to_do

	doCorePasses dflags rb (stats `plusSimplCount` stats1) us2 binds1 to_dos

doCorePass dfs rb us binds (CoreDoSimplify sw_chkr) 
   = _scc_ "Simplify"      simplifyPgm dfs rb sw_chkr us binds
doCorePass dfs rb us binds CoreCSE		        
   = _scc_ "CommonSubExpr" noStats dfs (cseProgram dfs binds)
doCorePass dfs rb us binds CoreLiberateCase	        
   = _scc_ "LiberateCase"  noStats dfs (liberateCase dfs binds)
doCorePass dfs rb us binds CoreDoFloatInwards       
   = _scc_ "FloatInwards"  noStats dfs (floatInwards dfs binds)
doCorePass dfs rb us binds (CoreDoFloatOutwards f)  
   = _scc_ "FloatOutwards" noStats dfs (floatOutwards dfs f us binds)
doCorePass dfs rb us binds CoreDoStaticArgs	        
   = _scc_ "StaticArgs"    noStats dfs (doStaticArgs us binds)
doCorePass dfs rb us binds CoreDoStrictness	        
   = _scc_ "Stranal"       noStats dfs (saBinds dfs binds)
doCorePass dfs rb us binds CoreDoWorkerWrapper      
   = _scc_ "WorkWrap"      noStats dfs (wwTopBinds dfs us binds)
doCorePass dfs rb us binds CoreDoSpecialising       
   = _scc_ "Specialise"    noStats dfs (specProgram dfs us binds)
doCorePass dfs rb us binds CoreDoCPResult	        
   = _scc_ "CPResult"      noStats dfs (cprAnalyse dfs binds)
doCorePass dfs rb us binds CoreDoPrintCore	        
   = _scc_ "PrintCore"     noStats dfs (printCore binds)
doCorePass dfs rb us binds CoreDoUSPInf             
   = _scc_ "CoreUsageSPInf" noStats dfs (doUsageSPInf dfs us binds)
doCorePass dfs rb us binds CoreDoGlomBinds	        
   = noStats dfs (glomBinds dfs binds)
doCorePass dfs rb us binds CoreDoNothing
   = noStats dfs (return binds)

printCore binds = do dumpIfSet True "Print Core"
			       (pprCoreBindings binds)
		     return binds

-- most passes return no stats and don't change rules
noStats dfs thing = do { binds <- thing; return (zeroSimplCount dfs, binds) }
\end{code}



%************************************************************************
%*									*
\subsection{Dealing with rules}
%*									*
%************************************************************************

-- prepareLocalRuleBase takes the CoreBinds and rules defined in this module.
-- It attaches those rules that are for local Ids to their binders, and
-- returns the remainder attached to Ids in an IdSet.  It also returns
-- Ids mentioned on LHS of some rule; these should be blacklisted.

-- The rule Ids and LHS Ids are black-listed; that is, they aren't inlined
-- so that the opportunity to apply the rule isn't lost too soon

\begin{code}
prepareRules :: DynFlags -> PackageRuleBase -> HomeSymbolTable
	     -> UniqSupply
	     -> [CoreBind]
	     -> [IdCoreRule]		-- Local rules
	     -> IO (RuleBase, 		-- Full rule base
		    (IdSet,IdSet),	-- Local rule Ids, and RHS fvs
		    [IdCoreRule]) 	-- Orphan rules

prepareRules dflags pkg_rule_base hst us binds rules
  = do	{ let (better_rules,_) = initSmpl dflags sw_chkr us local_ids black_list_all 
		                          (mapSmpl simplRule rules)

	; dumpIfSet_dyn dflags Opt_D_dump_rules "Transformation rules"
		        (vcat (map pprIdCoreRule better_rules))

	; let (local_rules, orphan_rules) = partition (isLocalId . fst) better_rules
	      local_rule_rhs_fvs	  = unionVarSets (map (ruleRhsFreeVars . snd) local_rules)
	      local_rule_base		  = extendRuleBaseList emptyRuleBase local_rules
	      local_rule_ids		  = ruleBaseIds local_rule_base	-- Local Ids with rules attached
	      imp_rule_base		  = foldl add_rules pkg_rule_base (moduleEnvElts hst)
	      rule_base			  = extendRuleBaseList imp_rule_base orphan_rules
	      final_rule_base		  = addRuleBaseFVs rule_base (ruleBaseFVs local_rule_base)
		-- The last step black-lists the free vars of local rules too

	; return (final_rule_base, (local_rule_ids, local_rule_rhs_fvs), orphan_rules)
    }
  where
    sw_chkr any	     = SwBool False			-- A bit bogus
    black_list_all v = not (isDataConWrapId v)
		-- This stops all inlining except the
		-- wrappers for data constructors

    add_rules rule_base mds = extendRuleBaseList rule_base (md_rules mds)

	-- Boringly, we need to gather the in-scope set.
	-- Typically this thunk won't even be forced, but the test in
	-- simpVar fails if it isn't right, and it might conceiveably matter
    local_ids = foldr (unionVarSet . mkVarSet . bindersOf) emptyVarSet binds


updateBinders :: (IdSet, 		-- Locally defined ids with their Rules attached
		  IdSet)		-- Ids free in the RHS of local rules
	      -> IsExported
	      -> [CoreBind] -> [CoreBind]
	-- A horrible function

-- Update the binders of top-level bindings as follows
-- 	a) Attach the rules for each locally-defined Id to that Id.
--	b) Set the no-discard flag if either the Id is exported,
--	   or it's mentoined in the RHS of a rule
-- 
-- Reason for (a)
-- 	- It makes the rules easier to look up
--	- It means that transformation rules and specialisations for
--	  locally defined Ids are handled uniformly
--	- It keeps alive things that are referred to only from a rule
--	  (the occurrence analyser knows about rules attached to Ids)
--	- It makes sure that, when we apply a rule, the free vars
--	  of the RHS are more likely to be in scope
--
-- Reason for (b)
--     It means that the binding won't be discarded EVEN if the binding
--     ends up being trivial (v = w) -- the simplifier would usually just 
--     substitute w for v throughout, but we don't apply the substitution to
--     the rules (maybe we should?), so this substitution would make the rule
--     bogus.

updateBinders (rule_ids, rule_rhs_fvs) is_exported binds
  = map update_bndrs binds
  where
    update_bndrs (NonRec b r) = NonRec (update_bndr b) r
    update_bndrs (Rec prs)    = Rec [(update_bndr b, r) | (b,r) <- prs]

    update_bndr bndr 
	|  is_exported (idName bndr)
	|| bndr `elemVarSet` rule_rhs_fvs = setIdNoDiscard bndr'
	| otherwise			  = bndr'
	where
	  bndr' = lookupVarSet rule_ids bndr `orElse` bndr
\end{code}


We must do some gentle simplification on the template (but not the RHS)
of each rule.  The case that forced me to add this was the fold/build rule,
which without simplification looked like:
	fold k z (build (/\a. g a))  ==>  ...
This doesn't match unless you do eta reduction on the build argument.

\begin{code}
simplRule rule@(id, BuiltinRule _)
  = returnSmpl rule
simplRule rule@(id, Rule name bndrs args rhs)
  = simplBinders bndrs			$ \ bndrs' -> 
    mapSmpl simpl_arg args		`thenSmpl` \ args' ->
    simplExpr rhs			`thenSmpl` \ rhs' ->
    returnSmpl (id, Rule name bndrs' args' rhs')

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


%************************************************************************
%*									*
\subsection{Glomming}
%*									*
%************************************************************************

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
  = do { showPass dflags "GlomBinds" ;
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
	    -> [CoreBind]		    -- Input
	    -> IO (SimplCount, [CoreBind])  -- New bindings

simplifyPgm dflags rule_base
	    sw_chkr us binds
  = do {
	showPass dflags "Simplify";

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

	return (counts_out, binds')
    }
  where
    max_iterations    = getSimplIntSwitch sw_chkr MaxSimplifierIterations
    black_list_fn     = blackListed rule_lhs_fvs (intSwitchSet sw_chkr SimplInlinePhase)
    imported_rule_ids = ruleBaseIds rule_base
    rule_lhs_fvs      = ruleBaseFVs rule_base
 
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
