%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SimplCore]{Driver for simplifying @Core@ programs}

\begin{code}
module SimplCore ( core2core ) where

#include "HsVersions.h"

import CmdLineOpts	( CoreToDo(..), SimplifierSwitch(..), 
			  SwitchResult(..), switchIsOn, intSwitchSet,
			  opt_D_dump_occur_anal, opt_D_dump_rules,
			  opt_D_dump_simpl_iterations,
			  opt_D_dump_simpl_stats,
			  opt_D_dump_simpl, opt_D_dump_rules,
			  opt_D_verbose_core2core,
			  opt_D_dump_occur_anal,
                          opt_UsageSPOn,
			)
import CoreLint		( beginPass, endPass )
import CoreSyn
import CSE		( cseProgram )
import Rules		( RuleBase, ProtoCoreRule(..), pprProtoCoreRule, prepareLocalRuleBase,
                          prepareOrphanRuleBase, unionRuleBase, localRule, orphanRule )
import CoreUnfold
import PprCore		( pprCoreBindings )
import OccurAnal	( occurAnalyseBinds )
import CoreUtils	( exprIsTrivial, etaReduceExpr )
import Simplify		( simplTopBinds, simplExpr )
import SimplUtils	( findDefault, simplBinders )
import SimplMonad
import Literal		( Literal(..), literalType, mkMachInt )
import ErrUtils		( dumpIfSet )
import FloatIn		( floatInwards )
import FloatOut		( floatOutwards )
import Id		( Id, mkSysLocal, mkVanillaId, isBottomingId, isDataConWrapId,
			  idType, setIdType, idName, idInfo, setIdNoDiscard
			)
import VarEnv
import VarSet
import Module		( Module )
import Name		( mkLocalName, tidyOccName, tidyTopName, 
			  NamedThing(..), OccName
			)
import TyCon		( TyCon, isDataTyCon )
import Type		( Type, 
			  isUnLiftedType,
			  tidyType, tidyTypes, tidyTopType, tidyTyVar, tidyTyVars,
			  Type
			)
import TysWiredIn	( smallIntegerDataCon, isIntegerTy )
import LiberateCase	( liberateCase )
import SAT		( doStaticArgs )
import Specialise	( specProgram)
import UsageSPInf       ( doUsageSPInf )
import StrictAnal	( saBinds )
import WorkWrap	        ( wwTopBinds )
import CprAnalyse       ( cprAnalyse )

import Unique		( Unique, Uniquable(..) )
import UniqSupply	( UniqSupply, mkSplitUniqSupply, splitUniqSupply, uniqFromSupply )
import Util		( mapAccumL )
import SrcLoc		( noSrcLoc )
import Bag
import Maybes
import IO		( hPutStr, stderr )
import Outputable

import Ratio 		( numerator, denominator )
import List             ( partition )
\end{code}

%************************************************************************
%*									*
\subsection{The driver for the simplifier}
%*									*
%************************************************************************

\begin{code}
core2core :: [CoreToDo]		-- Spec of what core-to-core passes to do
	  -> [CoreBind]		-- Binds in
	  -> [ProtoCoreRule]	-- Rules in
	  -> IO ([CoreBind], RuleBase)  -- binds, local orphan rules out

core2core core_todos binds rules
  = do
	us <-  mkSplitUniqSupply 's'
	let (cp_us, us1)   = splitUniqSupply us
	    (ru_us, ps_us) = splitUniqSupply us1

        let (local_rules, imported_rules) = partition localRule rules

        better_local_rules <- simplRules ru_us local_rules binds

        let (binds1, local_rule_base) = prepareLocalRuleBase binds better_local_rules
            imported_rule_base        = prepareOrphanRuleBase imported_rules

	-- Do the main business
	(stats, processed_binds, processed_local_rules)
            <- doCorePasses zeroSimplCount cp_us binds1 local_rule_base
			    imported_rule_base Nothing core_todos

	dumpIfSet opt_D_dump_simpl_stats
		  "Grand total simplifier statistics"
		  (pprSimplCount stats)

	-- Return results
        -- We only return local orphan rules, i.e., local rules not attached to an Id
	return (processed_binds, processed_local_rules)


doCorePasses :: SimplCount      -- simplifier stats
             -> UniqSupply      -- uniques
             -> [CoreBind]      -- local binds in (with rules attached)
             -> RuleBase        -- local orphan rules
             -> RuleBase        -- imported and builtin rules
             -> Maybe RuleBase  -- combined rulebase, or Nothing to ask for it to be rebuilt
             -> [CoreToDo]      -- which passes to do
             -> IO (SimplCount, [CoreBind], RuleBase)  -- stats, binds, local orphan rules

doCorePasses stats us binds lrb irb rb0 []
  = return (stats, binds, lrb)

doCorePasses stats us binds lrb irb rb0 (to_do : to_dos) 
  = do
	let (us1, us2) = splitUniqSupply us

        -- recompute rulebase if necessary
        let rb         = maybe (irb `unionRuleBase` lrb) id rb0

	(stats1, binds1, mlrb1) <- doCorePass us1 binds lrb rb to_do

        -- request rulebase recomputation if pass returned a new local rulebase
        let (lrb1,rb1) = maybe (lrb, Just rb) (\ lrb1 -> (lrb1, Nothing)) mlrb1

	doCorePasses (stats `plusSimplCount` stats1) us2 binds1 lrb1 irb rb1 to_dos

doCorePass us binds lrb rb (CoreDoSimplify sw_chkr) = _scc_ "Simplify"      simplifyPgm rb sw_chkr us binds
doCorePass us binds lrb rb CoreCSE		    = _scc_ "CommonSubExpr" noStats (cseProgram binds)
doCorePass us binds lrb rb CoreLiberateCase	    = _scc_ "LiberateCase"  noStats (liberateCase binds)
doCorePass us binds lrb rb CoreDoFloatInwards       = _scc_ "FloatInwards"  noStats (floatInwards binds)
doCorePass us binds lrb rb (CoreDoFloatOutwards f)  = _scc_ "FloatOutwards" noStats (floatOutwards f us binds)
doCorePass us binds lrb rb CoreDoStaticArgs	    = _scc_ "StaticArgs"    noStats (doStaticArgs us binds)
doCorePass us binds lrb rb CoreDoStrictness	    = _scc_ "Stranal"       noStats (saBinds binds)
doCorePass us binds lrb rb CoreDoWorkerWrapper      = _scc_ "WorkWrap"      noStats (wwTopBinds us binds)
doCorePass us binds lrb rb CoreDoSpecialising       = _scc_ "Specialise"    noStats (specProgram us binds)
doCorePass us binds lrb rb CoreDoCPResult	    = _scc_ "CPResult"      noStats (cprAnalyse binds)
doCorePass us binds lrb rb CoreDoPrintCore	    = _scc_ "PrintCore"     noStats (printCore binds)
doCorePass us binds lrb rb CoreDoUSPInf
  = _scc_ "CoreUsageSPInf" 
    if opt_UsageSPOn then
      do
         (binds1, rules1) <- doUsageSPInf us binds lrb
         return (zeroSimplCount, binds1, rules1)
    else
      trace "WARNING: ignoring requested -fusagesp pass; requires -fusagesp-on" $
      return (zeroSimplCount, binds, Nothing)

printCore binds = do dumpIfSet True "Print Core"
			       (pprCoreBindings binds)
		     return binds

-- most passes return no stats and don't change rules
noStats thing = do { binds <- thing; return (zeroSimplCount, binds, Nothing) }
\end{code}


%************************************************************************
%*									*
\subsection{Dealing with rules}
%*									*
%************************************************************************

We must do some gentle simplifiation on the template (but not the RHS)
of each rule.  The case that forced me to add this was the fold/build rule,
which without simplification looked like:
	fold k z (build (/\a. g a))  ==>  ...
This doesn't match unless you do eta reduction on the build argument.

\begin{code}
simplRules :: UniqSupply -> [ProtoCoreRule] -> [CoreBind] -> IO [ProtoCoreRule]
simplRules us rules binds
  = do  let (better_rules,_) = initSmpl sw_chkr us bind_vars black_list_all (mapSmpl simplRule rules)
	
	dumpIfSet opt_D_dump_rules
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

%************************************************************************
%*									*
\subsection{The driver for the simplifier}
%*									*
%************************************************************************

\begin{code}
simplifyPgm :: RuleBase
	    -> (SimplifierSwitch -> SwitchResult)
	    -> UniqSupply
	    -> [CoreBind]				    -- Input
	    -> IO (SimplCount, [CoreBind], Maybe RuleBase)  -- New bindings

simplifyPgm (imported_rule_ids, rule_lhs_fvs) 
	    sw_chkr us binds
  = do {
	beginPass "Simplify";

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
	-- Applying this rule makes f refer to h, although it doesn't appear to in the
	-- source program.  Our solution is to do this occasional glom-together step,
	-- just once per overall simplfication step.

	let { recd_binds = [Rec (flattenBinds binds)] };

	(termination_msg, it_count, counts_out, binds') <- iteration us 1 zeroSimplCount recd_binds;

	dumpIfSet (opt_D_verbose_core2core && opt_D_dump_simpl_stats)
		  "Simplifier statistics"
		  (vcat [text termination_msg <+> text "after" <+> ppr it_count <+> text "iterations",
			 text "",
			 pprSimplCount counts_out]);

	endPass "Simplify" 
		(opt_D_verbose_core2core && not opt_D_dump_simpl_iterations)
		binds' ;

	return (counts_out, binds', Nothing)
    }
  where
    max_iterations = getSimplIntSwitch sw_chkr MaxSimplifierIterations
    black_list_fn  = blackListed rule_lhs_fvs (intSwitchSet sw_chkr SimplInlinePhase)

    core_iter_dump binds | opt_D_verbose_core2core = pprCoreBindings binds
		         | otherwise		   = empty

    iteration us iteration_no counts binds
      = do {
		-- Occurrence analysis
	   let { tagged_binds = _scc_ "OccAnal" occurAnalyseBinds binds } ;

	   dumpIfSet opt_D_dump_occur_anal "Occurrence analysis"
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
	   case initSmpl sw_chkr us1 imported_rule_ids black_list_fn 
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
	   dumpIfSet opt_D_dump_simpl_iterations
		     ("Simplifier iteration " ++ show iteration_no 
		      ++ " out of " ++ show max_iterations)
		     (pprSimplCount counts') ;

	   if opt_D_dump_simpl_iterations then
		endPass ("Simplifier iteration " ++ show iteration_no ++ " result")
			opt_D_verbose_core2core
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
