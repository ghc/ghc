%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SimplCore]{Driver for simplifying @Core@ programs}

\begin{code}
module SimplCore ( core2core, simplifyExpr ) where

#include "HsVersions.h"

import CmdLineOpts	( CoreToDo(..), SimplifierSwitch(..),
			  SimplifierMode(..), DynFlags, DynFlag(..), dopt,
			  dopt_CoreToDo, buildCoreToDo
			)
import CoreSyn
import TcIface		( loadImportedRules )
import HscTypes		( HscEnv(..), ModGuts(..), ExternalPackageState(..),
			  ModDetails(..), HomeModInfo(..), HomePackageTable, Dependencies( dep_mods ), 
			  hscEPS, hptRules )
import CSE		( cseProgram )
import Rules		( RuleBase, ruleBaseIds, emptyRuleBase,
			  extendRuleBaseList, pprRuleBase, ruleCheckProgram )
import Module		( elemModuleEnv, lookupModuleEnv )
import PprCore		( pprCoreBindings, pprCoreExpr, pprIdRules )
import OccurAnal	( occurAnalyseBinds, occurAnalyseGlobalExpr )
import CoreUtils	( coreBindsSize )
import Simplify		( simplTopBinds, simplExpr )
import SimplEnv		( SimplEnv, simplBinders, mkSimplEnv, setInScopeSet )
import SimplMonad
import ErrUtils		( dumpIfSet, dumpIfSet_dyn, showPass )
import CoreLint		( endPass )
import VarEnv		( mkInScopeSet )
import FloatIn		( floatInwards )
import FloatOut		( floatOutwards )
import Id		( idIsFrom, idSpecialisation, setIdSpecialisation )
import VarSet
import LiberateCase	( liberateCase )
import SAT		( doStaticArgs )
import Specialise	( specProgram)
import SpecConstr	( specConstrProgram)
import DmdAnal		( dmdAnalPgm )
import WorkWrap	        ( wwTopBinds )
#ifdef OLD_STRICTNESS
import StrictAnal	( saBinds )
import CprAnalyse       ( cprAnalyse )
#endif

import UniqSupply	( UniqSupply, mkSplitUniqSupply, splitUniqSupply )
import IO		( hPutStr, stderr )
import Outputable
import List		( partition )
import Maybes		( orElse, fromJust )
\end{code}

%************************************************************************
%*									*
\subsection{The driver for the simplifier}
%*									*
%************************************************************************

\begin{code}
core2core :: HscEnv
	  -> ModGuts
	  -> IO ModGuts

core2core hsc_env guts
  = do
        let dflags = hsc_dflags hsc_env
	    core_todos
		| Just todo <- dopt_CoreToDo dflags  =  todo
		| otherwise			     =  buildCoreToDo dflags

	us <- mkSplitUniqSupply 's'
	let (cp_us, ru_us) = splitUniqSupply us

		-- COMPUTE THE RULE BASE TO USE
	(imp_rule_base, guts') <- prepareRules hsc_env guts ru_us

		-- DO THE BUSINESS
	(stats, guts'') <- doCorePasses hsc_env cp_us
			 		(zeroSimplCount dflags) 
					imp_rule_base guts' core_todos

	dumpIfSet_dyn dflags Opt_D_dump_simpl_stats
		  "Grand total simplifier statistics"
		  (pprSimplCount stats)

	return guts''


simplifyExpr :: DynFlags -- includes spec of what core-to-core passes to do
	     -> CoreExpr
	     -> IO CoreExpr
-- simplifyExpr is called by the driver to simplify an
-- expression typed in at the interactive prompt
simplifyExpr dflags expr
  = do	{
	; showPass dflags "Simplify"

	; us <-  mkSplitUniqSupply 's'

	; let (expr', _counts) = initSmpl dflags us $
				 simplExprGently gentleSimplEnv expr

	; dumpIfSet_dyn dflags Opt_D_dump_simpl "Simplified expression"
			(pprCoreExpr expr')

	; return expr'
	}

gentleSimplEnv :: SimplEnv
gentleSimplEnv = mkSimplEnv SimplGently 
			    (isAmongSimpl [])
			    emptyRuleBase

doCorePasses :: HscEnv
             -> UniqSupply      -- uniques
	     -> SimplCount      -- simplifier stats
             -> RuleBase        -- the main rule base
             -> ModGuts	        -- local binds in (with rules attached)
             -> [CoreToDo]      -- which passes to do
             -> IO (SimplCount, ModGuts)

doCorePasses hsc_env us stats rb guts []
  = return (stats, guts)

doCorePasses hsc_env us stats rb guts (to_do : to_dos) 
  = do
	let (us1, us2) = splitUniqSupply us
	(stats1, rb1, guts1) <- doCorePass to_do hsc_env us1 rb guts
	doCorePasses hsc_env us2 (stats `plusSimplCount` stats1) rb1 guts1 to_dos

doCorePass (CoreDoSimplify mode sws)   = _scc_ "Simplify"      simplifyPgm mode sws
doCorePass CoreCSE		       = _scc_ "CommonSubExpr" trBinds  cseProgram
doCorePass CoreLiberateCase	       = _scc_ "LiberateCase"  trBinds  liberateCase
doCorePass CoreDoFloatInwards          = _scc_ "FloatInwards"  trBinds  floatInwards
doCorePass (CoreDoFloatOutwards f)     = _scc_ "FloatOutwards" trBindsU (floatOutwards f)
doCorePass CoreDoStaticArgs	       = _scc_ "StaticArgs"    trBinds  doStaticArgs
doCorePass CoreDoStrictness	       = _scc_ "Stranal"       trBinds  dmdAnalPgm
doCorePass CoreDoWorkerWrapper         = _scc_ "WorkWrap"      trBindsU wwTopBinds
doCorePass CoreDoSpecialising          = _scc_ "Specialise"    trBindsU specProgram
doCorePass CoreDoSpecConstr	       = _scc_ "SpecConstr"    trBindsU specConstrProgram
doCorePass CoreDoGlomBinds	       = trBinds glomBinds
doCorePass CoreDoPrintCore	       = observe printCore
doCorePass (CoreDoRuleCheck phase pat) = observe (ruleCheck phase pat)
doCorePass CoreDoNothing	       = observe (\ _ _ -> return ())
#ifdef OLD_STRICTNESS		       
doCorePass CoreDoOldStrictness	       = _scc_ "OldStrictness" trBinds doOldStrictness
#endif

#ifdef OLD_STRICTNESS
doOldStrictness dfs binds
  = do binds1 <- saBinds dfs binds
       binds2 <- cprAnalyse dfs binds1
       return binds2
#endif

printCore _ binds = dumpIfSet True "Print Core" (pprCoreBindings binds)

ruleCheck phase pat dflags binds = do showPass dflags "RuleCheck"
				      printDump (ruleCheckProgram phase pat binds)

-- Most passes return no stats and don't change rules
trBinds :: (DynFlags -> [CoreBind] -> IO [CoreBind])
	-> HscEnv -> UniqSupply -> RuleBase -> ModGuts
	-> IO (SimplCount, RuleBase, ModGuts)
trBinds do_pass hsc_env us rb guts
  = do	{ binds' <- do_pass dflags (mg_binds guts)
	; return (zeroSimplCount dflags, rb, guts { mg_binds = binds' }) }
  where
    dflags = hsc_dflags hsc_env

trBindsU :: (DynFlags -> UniqSupply -> [CoreBind] -> IO [CoreBind])
	-> HscEnv -> UniqSupply -> RuleBase -> ModGuts
	-> IO (SimplCount, RuleBase, ModGuts)
trBindsU do_pass hsc_env us rb guts
  = do	{ binds' <- do_pass dflags us (mg_binds guts)
	; return (zeroSimplCount dflags, rb, guts { mg_binds = binds' }) }
  where
    dflags = hsc_dflags hsc_env

-- Observer passes just peek; don't modify the bindings at all
observe :: (DynFlags -> [CoreBind] -> IO a)
	-> HscEnv -> UniqSupply -> RuleBase -> ModGuts
	-> IO (SimplCount, RuleBase, ModGuts)
observe do_pass hsc_env us rb guts 
  = do	{ binds <- do_pass dflags (mg_binds guts)
	; return (zeroSimplCount dflags, rb, guts) }
  where
    dflags = hsc_dflags hsc_env
\end{code}



%************************************************************************
%*									*
\subsection{Dealing with rules}
%*									*
%************************************************************************

-- prepareLocalRuleBase takes the CoreBinds and rules defined in this module.
-- It attaches those rules that are for local Ids to their binders, and
-- returns the remainder attached to Ids in an IdSet.  

\begin{code}
prepareRules :: HscEnv 
	     -> ModGuts
	     -> UniqSupply
	     -> IO (RuleBase, 		-- Rule base for imported things, incl
					-- (a) rules defined in this module (orphans)
					-- (b) rules from other packages
					-- (c) rules from other modules in home package
		    ModGuts)		-- Modified fields are 
					--	(a) Bindings have rules attached,
					-- 	(b) Rules are now just orphan rules

prepareRules hsc_env@(HscEnv { hsc_dflags = dflags, hsc_HPT = hpt })
	     guts@(ModGuts { mg_binds = binds, mg_deps = deps, mg_rules = local_rules })
	     us 
  = do	{ eps <- hscEPS hsc_env

	; let 	-- Simplify the local rules; boringly, we need to make an in-scope set
		-- from the local binders, to avoid warnings from Simplify.simplVar
	      local_ids        = mkInScopeSet (mkVarSet (bindersOfBinds binds))
	      env	       = setInScopeSet gentleSimplEnv local_ids 
	      (better_rules,_) = initSmpl dflags us (mapSmpl (simplRule env) local_rules)
	      home_pkg_rules   = hptRules hsc_env (dep_mods deps)

	      (orphan_rules, rules_for_locals) = partition isOrphanRule better_rules
		-- Get the rules for locally-defined Ids out of the RuleBase
		-- If we miss any rules for Ids defined here, then we end up
		-- giving the local decl a new Unique (because the in-scope-set is (hackily) the
		-- same as the non-local-rule-id set, so the Id looks as if it's in scope
		-- and hence should be cloned), and now the binding for the class method 
		-- doesn't have the same Unique as the one in the Class and the tc-env
		--	Example:	class Foo a where
		--			  op :: a -> a
		--			{-# RULES "op" op x = x #-}

		-- NB: we assume that the imported rules dont include 
		--     rules for Ids in this module; if there is, the above bad things may happen

	      pkg_rule_base = eps_rule_base eps
	      hpt_rule_base = extendRuleBaseList pkg_rule_base home_pkg_rules
	      imp_rule_base = extendRuleBaseList hpt_rule_base orphan_rules

		-- Update the binders in the local bindings with the lcoal rules
		-- Update the binders of top-level bindings by
		-- attaching the rules for each locally-defined Id to that Id.
		-- 
		-- Reason
		-- 	- It makes the rules easier to look up
		--	- It means that transformation rules and specialisations for
		--	  locally defined Ids are handled uniformly
		--	- It keeps alive things that are referred to only from a rule
		--	  (the occurrence analyser knows about rules attached to Ids)
		--	- It makes sure that, when we apply a rule, the free vars
		--	  of the RHS are more likely to be in scope
		--	- The imported rules are carried in the in-scope set
		--	  which is extended on each iteration by the new wave of
		--	  local binders; any rules which aren't on the binding will
		--	  thereby get dropped
	      local_rule_base = extendRuleBaseList emptyRuleBase rules_for_locals
	      binds_w_rules   = updateBinders local_rule_base binds

	; dumpIfSet_dyn dflags Opt_D_dump_rules "Transformation rules"
		(vcat [text "Local rules", pprIdRules better_rules,
		       text "",
		       text "Imported rules", pprRuleBase imp_rule_base])

#ifdef DEBUG
	; let bad_rules = filter (idIsFrom (mg_module guts)) 
				 (varSetElems (ruleBaseIds imp_rule_base))
	; WARN( not (null bad_rules), ppr bad_rules ) return ()
#endif
	; return (imp_rule_base, guts { mg_binds = binds_w_rules, mg_rules = orphan_rules })
    }

updateBinders :: RuleBase -> [CoreBind] -> [CoreBind]
updateBinders rule_base binds
  = map update_bndrs binds
  where
    rule_ids = ruleBaseIds rule_base

    update_bndrs (NonRec b r) = NonRec (update_bndr b) r
    update_bndrs (Rec prs)    = Rec [(update_bndr b, r) | (b,r) <- prs]

    update_bndr bndr = case lookupVarSet rule_ids bndr of
			  Nothing -> bndr
			  Just id -> bndr `setIdSpecialisation` idSpecialisation id
\end{code}


We must do some gentle simplification on the template (but not the RHS)
of each rule.  The case that forced me to add this was the fold/build rule,
which without simplification looked like:
	fold k z (build (/\a. g a))  ==>  ...
This doesn't match unless you do eta reduction on the build argument.

\begin{code}
simplRule env rule@(IdCoreRule id _ (BuiltinRule _ _))
  = returnSmpl rule
simplRule env (IdCoreRule id is_orph (Rule act name bndrs args rhs))
  = simplBinders env bndrs		`thenSmpl` \ (env, bndrs') -> 
    mapSmpl (simplExprGently env) args	`thenSmpl` \ args' ->
    simplExprGently env rhs		`thenSmpl` \ rhs' ->
    returnSmpl (IdCoreRule id is_orph (Rule act name bndrs' args' rhs'))

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
--
-- The name 'Gently' suggests that the SimplifierMode is SimplGently,
-- and in fact that is so.... but the 'Gently' in simplExprGently doesn't
-- enforce that; it just simplifies the expression twice

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
simplifyPgm :: SimplifierMode
	    -> [SimplifierSwitch]
	    -> HscEnv
	    -> UniqSupply
	    -> RuleBase
	    -> ModGuts
	    -> IO (SimplCount, RuleBase, ModGuts)  -- New bindings

simplifyPgm mode switches hsc_env us rule_base guts
  = do {
	showPass dflags "Simplify";

	(termination_msg, it_count, counts_out, rule_base', guts') 
	   <- do_iteration us rule_base 1 (zeroSimplCount dflags) guts;

	dumpIfSet (dopt Opt_D_verbose_core2core dflags 
                   && dopt Opt_D_dump_simpl_stats dflags)
		  "Simplifier statistics"
		  (vcat [text termination_msg <+> text "after" <+> ppr it_count <+> text "iterations",
			 text "",
			 pprSimplCount counts_out]);

	endPass dflags "Simplify" Opt_D_verbose_core2core (mg_binds guts');

	return (counts_out, rule_base', guts')
    }
  where
    dflags 	      = hsc_dflags hsc_env
    phase_info	      = case mode of
			  SimplGently  -> "gentle"
			  SimplPhase n -> show n

    sw_chkr	      = isAmongSimpl switches
    max_iterations    = intSwitchSet sw_chkr MaxSimplifierIterations `orElse` 2
 
    do_iteration us rule_base iteration_no counts guts
	-- iteration_no is the number of the iteration we are
	-- about to begin, with '1' for the first
      | iteration_no > max_iterations	-- Stop if we've run out of iterations
      = do {
#ifdef DEBUG
	    if  max_iterations > 2 then
		hPutStr stderr ("NOTE: Simplifier still going after " ++ 
				show max_iterations ++ 
			    	" iterations; bailing out.\n")
	    else 
		return ();
#endif
		-- Subtract 1 from iteration_no to get the
		-- number of iterations we actually completed
	    return ("Simplifier baled out", iteration_no - 1, counts, rule_base, guts)
	}

      -- Try and force thunks off the binds; significantly reduces
      -- space usage, especially with -O.  JRS, 000620.
      | let sz = coreBindsSize (mg_binds guts) in sz == sz
      = do {
		-- Occurrence analysis
	   let { tagged_binds = _scc_ "OccAnal" occurAnalyseBinds (mg_binds guts) } ;

	   dumpIfSet_dyn dflags Opt_D_dump_occur_anal "Occurrence analysis"
		     (pprCoreBindings tagged_binds);

	   	-- Get any new rules, and extend the rule base
		-- (on the side this extends the package rule base in the
		--  ExternalPackageTable, ready for the next complation 
		--  in --make mode)
		-- We need to do this regularly, because simplification can
		-- poke on IdInfo thunks, which in turn brings in new rules
		-- behind the scenes.  Otherwise there's a danger we'll simply
		-- miss the rules for Ids hidden inside imported inlinings
	   new_rules <- loadImportedRules hsc_env guts ;
	   let	{ rule_base' = extendRuleBaseList rule_base new_rules
		; simpl_env  = mkSimplEnv mode sw_chkr rule_base' } ;
			-- The new rule base Ids are used to initialise
			-- the in-scope set.  That way, the simplifier will change any
			-- occurrences of the imported id to the one in the imported_rule_ids
			-- set, which are decorated with their rules.
	   
		-- Simplify the program
		-- We do this with a *case* not a *let* because lazy pattern
		-- matching bit us with bad space leak!
		-- With a let, we ended up with
		--   let
		--	t = initSmpl ...
		--	counts' = snd t
		--   in
		-- 	case t of {(_,counts') -> if counts'=0 then ... }
		-- So the conditional didn't force counts', because the
		-- selection got duplicated.  Sigh!
	   case initSmpl dflags us1 (_scc_ "SimplTopBinds" simplTopBinds simpl_env tagged_binds) of {
	  	(binds', counts') -> do {

	   let	{ guts'      = guts { mg_binds = binds' }
		; all_counts = counts `plusSimplCount` counts'
		; herald     = "Simplifier phase " ++ phase_info ++ 
			      ", iteration " ++ show iteration_no ++
			      " out of " ++ show max_iterations
	        } ;

		-- Stop if nothing happened; don't dump output
	   if isZeroSimplCount counts' then
		return ("Simplifier reached fixed point", iteration_no, 
			all_counts, rule_base', guts')
	   else do {

		-- Dump the result of this iteration
	   dumpIfSet_dyn dflags Opt_D_dump_simpl_iterations herald
		         (pprSimplCount counts') ;

	   endPass dflags herald Opt_D_dump_simpl_iterations binds' ;

		-- Loop
  	   do_iteration us2 rule_base' (iteration_no + 1) all_counts guts'
	}  } } }
      where
  	  (us1, us2) = splitUniqSupply us
\end{code}
