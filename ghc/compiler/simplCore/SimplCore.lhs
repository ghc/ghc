%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SimplCore]{Driver for simplifying @Core@ programs}

\begin{code}
module SimplCore ( core2core, simplifyExpr ) where

#include "HsVersions.h"

import CmdLineOpts	( CoreToDo(..), SimplifierSwitch(..), SimplifierMode(..),
			  DynFlags, DynFlag(..), dopt, dopt_CoreToDo
			)
import CoreSyn
import CoreFVs		( ruleRhsFreeVars )
import HscTypes		( PersistentCompilerState(..),
			  PackageRuleBase, HomeSymbolTable, IsExported, ModDetails(..)
			)
import CSE		( cseProgram )
import Rules		( RuleBase, emptyRuleBase, ruleBaseFVs, ruleBaseIds, 
			  extendRuleBaseList, addRuleBaseFVs, pprRuleBase, 
			  ruleCheckProgram )
import Module		( moduleEnvElts )
import PprCore		( pprCoreBindings, pprCoreExpr )
import OccurAnal	( occurAnalyseBinds, occurAnalyseGlobalExpr )
import CoreUtils	( coreBindsSize )
import Simplify		( simplTopBinds, simplExpr )
import SimplUtils	( simplBinders )
import SimplMonad
import ErrUtils		( dumpIfSet, dumpIfSet_dyn, showPass )
import CoreLint		( endPass )
import FloatIn		( floatInwards )
import FloatOut		( floatOutwards )
import Id		( idName, setIdLocalExported )
import VarSet
import LiberateCase	( liberateCase )
import SAT		( doStaticArgs )
import Specialise	( specProgram)
import SpecConstr	( specConstrProgram)
import UsageSPInf       ( doUsageSPInf )
import StrictAnal	( saBinds )
import DmdAnal		( dmdAnalPgm )
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
	  -> ModDetails
	  -> IO ModDetails

core2core dflags pcs hst is_exported 
	  mod_details@(ModDetails { md_binds = binds_in, md_rules = rules_in })
  = do
        let core_todos    = dopt_CoreToDo dflags
	let pkg_rule_base = pcs_rules pcs		-- Rule-base accumulated from imported packages
	

	us <-  mkSplitUniqSupply 's'
	let (cp_us, ru_us) = splitUniqSupply us

		-- COMPUTE THE RULE BASE TO USE
	(rule_base, local_rule_ids, orphan_rules, rule_rhs_fvs)
		<- prepareRules dflags pkg_rule_base hst ru_us binds_in rules_in

		-- PREPARE THE BINDINGS
	let binds1 = updateBinders local_rule_ids rule_rhs_fvs is_exported binds_in

		-- DO THE BUSINESS
	(stats, processed_binds)
		<- doCorePasses dflags rule_base (zeroSimplCount dflags) cp_us binds1 core_todos

	dumpIfSet_dyn dflags Opt_D_dump_simpl_stats
		  "Grand total simplifier statistics"
		  (pprSimplCount stats)

	-- Return results
        -- We only return local orphan rules, i.e., local rules not attached to an Id
	-- The bindings cotain more rules, embedded in the Ids
	return (mod_details { md_binds = processed_binds, md_rules = orphan_rules})


simplifyExpr :: DynFlags -- includes spec of what core-to-core passes to do
	     -> PersistentCompilerState
	     -> HomeSymbolTable
	     -> CoreExpr
	     -> IO CoreExpr
-- simplifyExpr is called by the driver to simplify an
-- expression typed in at the interactive prompt
simplifyExpr dflags pcs hst expr
  = do	{
	; showPass dflags "Simplify"

	; us <-  mkSplitUniqSupply 's'

	; let env	       = emptySimplEnv (SimplPhase 0) [] emptyVarSet
	      (expr', _counts) = initSmpl dflags us (simplExprGently env expr)

	; dumpIfSet_dyn dflags Opt_D_dump_simpl "Simplified expression"
			(pprCoreExpr expr')

	; return expr'
	}


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

doCorePass dfs rb us binds (CoreDoSimplify mode switches) 
   = _scc_ "Simplify"      simplifyPgm dfs rb mode switches us binds
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
   = _scc_ "Stranal"       noStats dfs (strictAnal dfs binds)
doCorePass dfs rb us binds CoreDoWorkerWrapper      
   = _scc_ "WorkWrap"      noStats dfs (wwTopBinds dfs us binds)
doCorePass dfs rb us binds CoreDoSpecialising       
   = _scc_ "Specialise"    noStats dfs (specProgram dfs us binds)
doCorePass dfs rb us binds CoreDoSpecConstr
   = _scc_ "SpecConstr"    noStats dfs (specConstrProgram dfs us binds)
#ifdef DEBUG
doCorePass dfs rb us binds CoreDoCPResult	        
   = _scc_ "CPResult"      noStats dfs (cprAnalyse dfs binds)
#endif
doCorePass dfs rb us binds CoreDoPrintCore	        
   = _scc_ "PrintCore"     noStats dfs (printCore binds)
doCorePass dfs rb us binds CoreDoUSPInf             
   = _scc_ "CoreUsageSPInf" noStats dfs (doUsageSPInf dfs us binds)
doCorePass dfs rb us binds CoreDoGlomBinds	        
   = noStats dfs (glomBinds dfs binds)
doCorePass dfs rb us binds (CoreDoRuleCheck phase pat)
   = noStats dfs (ruleCheck dfs phase pat binds)
doCorePass dfs rb us binds CoreDoNothing
   = noStats dfs (return binds)

strictAnal dfs binds = do
#ifdef DEBUG
     binds <- saBinds dfs binds
#endif
     dmdAnalPgm dfs binds

printCore binds = do dumpIfSet True "Print Core"
			       (pprCoreBindings binds)
		     return binds

ruleCheck dflags phase pat binds = do showPass dflags "RuleCheck"
				      printDump (ruleCheckProgram phase pat binds)
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
		    IdSet,		-- Local rule Ids
		    [IdCoreRule],	-- Orphan rules
		    IdSet) 		-- RHS free vars of all rules

prepareRules dflags pkg_rule_base hst us binds local_rules
  = do	{ let env	       = emptySimplEnv SimplGently [] local_ids 
	      (better_rules,_) = initSmpl dflags us (mapSmpl (simplRule env) local_rules)

	; let (local_rules, orphan_rules) = partition ((`elemVarSet` local_ids) . fst) better_rules
		-- We use (`elemVarSet` local_ids) rather than isLocalId because
		-- isLocalId isn't true of class methods.
		-- If we miss any rules for Ids defined here, then we end up
		-- giving the local decl a new Unique (because the in-scope-set is the
		-- same as the rule-id set), and now the binding for the class method 
		-- doesn't have the same Unique as the one in the Class and the tc-env
		--	Example:	class Foo a where
		--			  op :: a -> a
		--			{-# RULES "op" op x = x #-}

	      rule_rhs_fvs		  = unionVarSets (map (ruleRhsFreeVars . snd) better_rules)
	      local_rule_base		  = extendRuleBaseList emptyRuleBase local_rules
	      local_rule_ids		  = ruleBaseIds local_rule_base	-- Local Ids with rules attached
	      imp_rule_base		  = foldl add_rules pkg_rule_base (moduleEnvElts hst)
	      rule_base			  = extendRuleBaseList imp_rule_base orphan_rules
	      final_rule_base		  = addRuleBaseFVs rule_base (ruleBaseFVs local_rule_base)
		-- The last step black-lists the free vars of local rules too

	; dumpIfSet_dyn dflags Opt_D_dump_rules "Transformation rules"
		(vcat [text "Local rules", pprRuleBase local_rule_base,
		       text "",
		       text "Imported rules", pprRuleBase final_rule_base])

	; return (final_rule_base, local_rule_ids, orphan_rules, rule_rhs_fvs)
    }
  where
    add_rules rule_base mds = extendRuleBaseList rule_base (md_rules mds)

	-- Boringly, we need to gather the in-scope set.
    local_ids = foldr (unionVarSet . mkVarSet . bindersOf) emptyVarSet binds


updateBinders :: IdSet	 		-- Locally defined ids with their Rules attached
	      -> IdSet			-- Ids free in the RHS of local rules
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

updateBinders rule_ids rule_rhs_fvs is_exported binds
  = map update_bndrs binds
  where
    update_bndrs (NonRec b r) = NonRec (update_bndr b) r
    update_bndrs (Rec prs)    = Rec [(update_bndr b, r) | (b,r) <- prs]

    update_bndr bndr 
	| dont_discard bndr = setIdLocalExported bndr_with_rules
	| otherwise	    = bndr_with_rules
	where
	  bndr_with_rules = lookupVarSet rule_ids bndr `orElse` bndr

    dont_discard bndr =  is_exported (idName bndr)
		      || bndr `elemVarSet` rule_rhs_fvs 
\end{code}


We must do some gentle simplification on the template (but not the RHS)
of each rule.  The case that forced me to add this was the fold/build rule,
which without simplification looked like:
	fold k z (build (/\a. g a))  ==>  ...
This doesn't match unless you do eta reduction on the build argument.

\begin{code}
simplRule env rule@(id, BuiltinRule _ _)
  = returnSmpl rule
simplRule env rule@(id, Rule act name bndrs args rhs)
  = simplBinders env bndrs		`thenSmpl` \ (env, bndrs') -> 
    mapSmpl (simplExprGently env) args	`thenSmpl` \ args' ->
    simplExprGently env rhs		`thenSmpl` \ rhs' ->
    returnSmpl (id, Rule act name bndrs' args' rhs')

-- It's important that simplExprGently does eta reduction.
-- For example, in a rule like:
--	augment g (build h) 
-- we do not want to get
--	augment (\a. g a) (build h)
-- otherwise we don't match when given an argument like
--	(\a. h a a)
--
-- The simplifier does indeed do eta reduction (it's in
-- Simplify.completeLam) but only if -O is on.
\end{code}

\begin{code}
simplExprGently :: SimplEnv -> CoreExpr -> SimplM CoreExpr
-- Simplifies an expression 
-- 	does occurrence analysis, then simplification
--	and repeats (twice currently) because one pass
--	alone leaves tons of crud.
-- Used (a) for user expressions typed in at the interactive prompt
--	(b) the LHS and RHS of a RULE
simplExprGently env expr
  = simplExpr env (occurAnalyseGlobalExpr expr) 	`thenSmpl` \ expr1 ->
    simplExpr env (occurAnalyseGlobalExpr expr1)
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
	    -> SimplifierMode
	    -> [SimplifierSwitch]
	    -> UniqSupply
	    -> [CoreBind]		    -- Input
	    -> IO (SimplCount, [CoreBind])  -- New bindings

simplifyPgm dflags rule_base
	    mode switches us binds
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

	endPass dflags "Simplify" Opt_D_verbose_core2core binds';

	return (counts_out, binds')
    }
  where
    phase_info	      = case mode of
			  SimplGently  -> "gentle"
			  SimplPhase n -> show n

    imported_rule_ids = ruleBaseIds rule_base
    simpl_env 	      = emptySimplEnv mode switches imported_rule_ids
    sw_chkr	      = getSwitchChecker simpl_env
    max_iterations    = intSwitchSet sw_chkr MaxSimplifierIterations `orElse` 2
 
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
	   case initSmpl dflags us1 (simplTopBinds simpl_env tagged_binds) of {
	  	(binds', counts') -> do {
			-- The imported_rule_ids are used by initSmpl to initialise
			-- the in-scope set.  That way, the simplifier will change any
			-- occurrences of the imported id to the one in the imported_rule_ids
			-- set, which are decorated with their rules.

	   let { all_counts = counts `plusSimplCount` counts' ;
		 herald     = "Simplifier phase " ++ phase_info ++ 
			      ", iteration " ++ show iteration_no ++
			      " out of " ++ show max_iterations
	        } ;

		-- Stop if nothing happened; don't dump output
	   if isZeroSimplCount counts' then
		return ("Simplifier reached fixed point", iteration_no, all_counts, binds')
	   else do {

		-- Dump the result of this iteration
	   dumpIfSet_dyn dflags Opt_D_dump_simpl_iterations herald
		     (pprSimplCount counts') ;

	   endPass dflags herald Opt_D_dump_simpl_iterations binds' ;

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
