%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SimplCore]{Driver for simplifying @Core@ programs}

\begin{code}
module SimplCore ( core2core, simplifyExpr ) where

#include "HsVersions.h"

import DynFlags		( CoreToDo(..), SimplifierSwitch(..),
			  SimplifierMode(..), DynFlags, DynFlag(..), dopt,
			  getCoreToDo )
import CoreSyn
import HscTypes		( HscEnv(..), ModGuts(..), ExternalPackageState(..),
			  Dependencies( dep_mods ), 
			  hscEPS, hptRules )
import CSE		( cseProgram )
import Rules		( RuleBase, emptyRuleBase, mkRuleBase, unionRuleBase,
			  extendRuleBaseList, pprRuleBase, ruleCheckProgram,
			  addSpecInfo, addIdSpecialisations )
import PprCore		( pprCoreBindings, pprCoreExpr, pprRules )
import OccurAnal	( occurAnalysePgm, occurAnalyseExpr )
import IdInfo		( setNewStrictnessInfo, newStrictnessInfo, 
			  setWorkerInfo, workerInfo,
			  setSpecInfo, specInfo, specInfoRules )
import CoreUtils	( coreBindsSize )
import Simplify		( simplTopBinds, simplExpr )
import SimplEnv		( SimplEnv, simplBinders, mkSimplEnv, setInScopeSet )
import SimplMonad
import ErrUtils		( dumpIfSet, dumpIfSet_dyn, showPass )
import CoreLint		( endPass )
import FloatIn		( floatInwards )
import FloatOut		( floatOutwards )
import Id		( Id, modifyIdInfo, idInfo, isExportedId, isLocalId,
			  idSpecialisation, idName )
import VarSet
import VarEnv
import NameEnv		( lookupNameEnv )
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
import Maybes		( orElse )
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
	    core_todos = getCoreToDo dflags

	us <- mkSplitUniqSupply 's'
	let (cp_us, ru_us) = splitUniqSupply us

		-- COMPUTE THE RULE BASE TO USE
	(imp_rule_base, guts') <- prepareRules hsc_env guts ru_us

		-- DO THE BUSINESS
	(stats, guts'') <- doCorePasses hsc_env imp_rule_base cp_us
			 		(zeroSimplCount dflags) 
					guts' core_todos

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
             -> RuleBase        -- the imported main rule base
             -> UniqSupply      -- uniques
	     -> SimplCount      -- simplifier stats
             -> ModGuts	        -- local binds in (with rules attached)
             -> [CoreToDo]      -- which passes to do
             -> IO (SimplCount, ModGuts)

doCorePasses hsc_env rb us stats guts []
  = return (stats, guts)

doCorePasses hsc_env rb us stats guts (to_do : to_dos) 
  = do
	let (us1, us2) = splitUniqSupply us
	(stats1, guts1) <- doCorePass to_do hsc_env us1 rb guts
	doCorePasses hsc_env rb us2 (stats `plusSimplCount` stats1) guts1 to_dos

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
	-> IO (SimplCount, ModGuts)
trBinds do_pass hsc_env us rb guts
  = do	{ binds' <- do_pass dflags (mg_binds guts)
	; return (zeroSimplCount dflags, guts { mg_binds = binds' }) }
  where
    dflags = hsc_dflags hsc_env

trBindsU :: (DynFlags -> UniqSupply -> [CoreBind] -> IO [CoreBind])
	-> HscEnv -> UniqSupply -> RuleBase -> ModGuts
	-> IO (SimplCount, ModGuts)
trBindsU do_pass hsc_env us rb guts
  = do	{ binds' <- do_pass dflags us (mg_binds guts)
	; return (zeroSimplCount dflags, guts { mg_binds = binds' }) }
  where
    dflags = hsc_dflags hsc_env

-- Observer passes just peek; don't modify the bindings at all
observe :: (DynFlags -> [CoreBind] -> IO a)
	-> HscEnv -> UniqSupply -> RuleBase -> ModGuts
	-> IO (SimplCount, ModGuts)
observe do_pass hsc_env us rb guts 
  = do	{ binds <- do_pass dflags (mg_binds guts)
	; return (zeroSimplCount dflags, guts) }
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
					-- (b) rules from other modules in home package
					-- but not things from other packages

		    ModGuts)		-- Modified fields are 
					--	(a) Bindings have rules attached,
					-- 	(b) Rules are now just orphan rules

prepareRules hsc_env@(HscEnv { hsc_dflags = dflags, hsc_HPT = hpt })
	     guts@(ModGuts { mg_binds = binds, mg_deps = deps, mg_rules = local_rules })
	     us 
  = do	{ let 	-- Simplify the local rules; boringly, we need to make an in-scope set
		-- from the local binders, to avoid warnings from Simplify.simplVar
	      local_ids        = mkInScopeSet (mkVarSet (bindersOfBinds binds))
	      env	       = setInScopeSet gentleSimplEnv local_ids 
	      (better_rules,_) = initSmpl dflags us (mapSmpl (simplRule env) local_rules)
	      home_pkg_rules   = hptRules hsc_env (dep_mods deps)

		-- Find the rules for locally-defined Ids; then we can attach them
		-- to the binders in the top-level bindings
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
	      (rules_for_locals, rules_for_imps) = partition isLocalRule better_rules
	      local_rule_base = extendRuleBaseList emptyRuleBase rules_for_locals
	      binds_w_rules   = updateBinders local_rule_base binds

	      hpt_rule_base = mkRuleBase home_pkg_rules
	      imp_rule_base = extendRuleBaseList hpt_rule_base rules_for_imps

	; dumpIfSet_dyn dflags Opt_D_dump_rules "Transformation rules"
		(vcat [text "Local rules", pprRules better_rules,
		       text "",
		       text "Imported rules", pprRuleBase imp_rule_base])

	; return (imp_rule_base, guts { mg_binds = binds_w_rules, 
					mg_rules = rules_for_imps })
    }

updateBinders :: RuleBase -> [CoreBind] -> [CoreBind]
updateBinders local_rules binds
  = map update_bndrs binds
  where
    update_bndrs (NonRec b r) = NonRec (update_bndr b) r
    update_bndrs (Rec prs)    = Rec [(update_bndr b, r) | (b,r) <- prs]

    update_bndr bndr = case lookupNameEnv local_rules (idName bndr) of
			  Nothing    -> bndr
			  Just rules -> bndr `addIdSpecialisations` rules
				-- The binder might have some existing rules,
				-- arising from specialisation pragmas
\end{code}


We must do some gentle simplification on the template (but not the RHS)
of each rule.  The case that forced me to add this was the fold/build rule,
which without simplification looked like:
	fold k z (build (/\a. g a))  ==>  ...
This doesn't match unless you do eta reduction on the build argument.

\begin{code}
simplRule env rule@(BuiltinRule {})
  = returnSmpl rule
simplRule env rule@(Rule { ru_bndrs = bndrs, ru_args = args, ru_rhs = rhs })
  = simplBinders env bndrs		`thenSmpl` \ (env, bndrs') -> 
    mapSmpl (simplExprGently env) args	`thenSmpl` \ args' ->
    simplExprGently env rhs		`thenSmpl` \ rhs' ->
    returnSmpl (rule { ru_bndrs = bndrs', ru_args = args', ru_rhs = rhs' })

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
  = simplExpr env (occurAnalyseExpr expr) 	`thenSmpl` \ expr1 ->
    simplExpr env (occurAnalyseExpr expr1)
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
	    -> IO (SimplCount, ModGuts)  -- New bindings

simplifyPgm mode switches hsc_env us imp_rule_base guts
  = do {
	showPass dflags "Simplify";

	(termination_msg, it_count, counts_out, binds') 
	   <- do_iteration us 1 (zeroSimplCount dflags) (mg_binds guts) ;

	dumpIfSet (dopt Opt_D_verbose_core2core dflags 
                   && dopt Opt_D_dump_simpl_stats dflags)
		  "Simplifier statistics"
		  (vcat [text termination_msg <+> text "after" <+> ppr it_count <+> text "iterations",
			 text "",
			 pprSimplCount counts_out]);

	endPass dflags "Simplify" Opt_D_verbose_core2core binds';

	return (counts_out, guts { mg_binds = binds' })
    }
  where
    dflags 	   = hsc_dflags hsc_env
    phase_info	   = case mode of
		   	  SimplGently  -> "gentle"
		   	  SimplPhase n -> show n
		   
    sw_chkr	   = isAmongSimpl switches
    max_iterations = intSwitchSet sw_chkr MaxSimplifierIterations `orElse` 2
 
    do_iteration us iteration_no counts binds
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
	    return ("Simplifier baled out", iteration_no - 1, counts, binds)
	}

      -- Try and force thunks off the binds; significantly reduces
      -- space usage, especially with -O.  JRS, 000620.
      | let sz = coreBindsSize binds in sz == sz
      = do {
		-- Occurrence analysis
	   let { tagged_binds = _scc_ "OccAnal" occurAnalysePgm binds } ;
	   dumpIfSet_dyn dflags Opt_D_dump_occur_anal "Occurrence analysis"
		     (pprCoreBindings tagged_binds);

	   	-- Get any new rules, and extend the rule base
		-- We need to do this regularly, because simplification can
		-- poke on IdInfo thunks, which in turn brings in new rules
		-- behind the scenes.  Otherwise there's a danger we'll simply
		-- miss the rules for Ids hidden inside imported inlinings
	   eps <- hscEPS hsc_env ;
	   let	{ rule_base' = unionRuleBase imp_rule_base (eps_rule_base eps)
		; simpl_env  = mkSimplEnv mode sw_chkr rule_base' } ;
	   
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

	   let	{ all_counts = counts `plusSimplCount` counts'
		; herald     = "Simplifier phase " ++ phase_info ++ 
			      ", iteration " ++ show iteration_no ++
			      " out of " ++ show max_iterations
	        } ;

		-- Stop if nothing happened; don't dump output
	   if isZeroSimplCount counts' then
		return ("Simplifier reached fixed point", iteration_no, 
			all_counts, binds')
	   else do {
		-- Short out indirections
		-- We do this *after* at least one run of the simplifier 
		-- because indirection-shorting uses the export flag on *occurrences*
		-- and that isn't guaranteed to be ok until after the first run propagates
		-- stuff from the binding site to its occurrences
	   let { binds'' = _scc_ "ZapInd" shortOutIndirections binds' } ;

		-- Dump the result of this iteration
	   dumpIfSet_dyn dflags Opt_D_dump_simpl_iterations herald
		         (pprSimplCount counts') ;
	   endPass dflags herald Opt_D_dump_simpl_iterations binds'' ;

		-- Loop
  	   do_iteration us2 (iteration_no + 1) all_counts binds''
	}  } } }
      where
  	  (us1, us2) = splitUniqSupply us
\end{code}


%************************************************************************
%*									*
		Shorting out indirections
%*									*
%************************************************************************

If we have this:

	x_local = <expression>
	...bindings...
	x_exported = x_local

where x_exported is exported, and x_local is not, then we replace it with this:

	x_exported = <expression>
	x_local = x_exported
	...bindings...

Without this we never get rid of the x_exported = x_local thing.  This
save a gratuitous jump (from \tr{x_exported} to \tr{x_local}), and
makes strictness information propagate better.  This used to happen in
the final phase, but it's tidier to do it here.

STRICTNESS: if we have done strictness analysis, we want the strictness info on
x_local to transfer to x_exported.  Hence the copyIdInfo call.

RULES: we want to *add* any RULES for x_local to x_exported.

Note [Rules and indirection-zapping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Problem: what if x_exported has a RULE that mentions something in ...bindings...?
Then the things mentioned can be out of scope!  Solution
 a) Make sure that in this pass the usage-info from x_exported is 
	available for ...bindings...
 b) If there are any such RULES, rec-ify the entire top-level. 
    It'll get sorted out next time round

Messing up the rules
~~~~~~~~~~~~~~~~~~~~
The example that went bad on me at one stage was this one:
	
    iterate :: (a -> a) -> a -> [a]
	[Exported]
    iterate = iterateList	
    
    iterateFB c f x = x `c` iterateFB c f (f x)
    iterateList f x =  x : iterateList f (f x)
    	[Not exported]
    
    {-# RULES
    "iterate"	forall f x.	iterate f x = build (\c _n -> iterateFB c f x)
    "iterateFB" 		iterateFB (:) = iterateList
     #-}

This got shorted out to:

    iterateList :: (a -> a) -> a -> [a]
    iterateList = iterate
    
    iterateFB c f x = x `c` iterateFB c f (f x)
    iterate f x =  x : iterate f (f x)
    
    {-# RULES
    "iterate"	forall f x.	iterate f x = build (\c _n -> iterateFB c f x)
    "iterateFB" 		iterateFB (:) = iterate
     #-}

And now we get an infinite loop in the rule system 
	iterate f x -> build (\cn -> iterateFB c f x)
		    -> iterateFB (:) f x
		    -> iterate f x

Tiresome old solution: 
	don't do shorting out if f has rewrite rules (see shortableIdInfo)

New solution (I think): 
	use rule switching-off pragmas to get rid 
	of iterateList in the first place


Other remarks
~~~~~~~~~~~~~
If more than one exported thing is equal to a local thing (i.e., the
local thing really is shared), then we do one only:
\begin{verbatim}
	x_local = ....
	x_exported1 = x_local
	x_exported2 = x_local
==>
	x_exported1 = ....

	x_exported2 = x_exported1
\end{verbatim}

We rely on prior eta reduction to simplify things like
\begin{verbatim}
	x_exported = /\ tyvars -> x_local tyvars
==>
	x_exported = x_local
\end{verbatim}
Hence,there's a possibility of leaving unchanged something like this:
\begin{verbatim}
	x_local = ....
	x_exported1 = x_local Int
\end{verbatim}
By the time we've thrown away the types in STG land this 
could be eliminated.  But I don't think it's very common
and it's dangerous to do this fiddling in STG land 
because we might elminate a binding that's mentioned in the
unfolding for something.

\begin{code}
type IndEnv = IdEnv Id		-- Maps local_id -> exported_id

shortOutIndirections :: [CoreBind] -> [CoreBind]
shortOutIndirections binds
  | isEmptyVarEnv ind_env = binds
  | no_need_to_flatten	  = binds'
  | otherwise 		  = [Rec (flattenBinds binds')]	-- See Note [Rules and indirect-zapping]
  where
    ind_env 	       = makeIndEnv binds
    exp_ids 	       = varSetElems ind_env	-- These exported Ids are the subjects
    exp_id_set	       = mkVarSet exp_ids	-- of the indirection-elimination
    no_need_to_flatten = all (null . specInfoRules . idSpecialisation) exp_ids
    binds' 	       = concatMap zap binds

    zap (NonRec bndr rhs) = [NonRec b r | (b,r) <- zapPair (bndr,rhs)]
    zap (Rec pairs)	  = [Rec (concatMap zapPair pairs)]

    zapPair (bndr, rhs)
	| bndr `elemVarSet` exp_id_set 		   = []
	| Just exp_id <- lookupVarEnv ind_env bndr = [(transferIdInfo exp_id bndr, rhs),
						      (bndr, Var exp_id)]
	| otherwise				   = [(bndr,rhs)]
			     
makeIndEnv :: [CoreBind] -> IndEnv
makeIndEnv binds
  = foldr add_bind emptyVarEnv binds
  where
    add_bind :: CoreBind -> IndEnv -> IndEnv
    add_bind (NonRec exported_id rhs) env = add_pair (exported_id, rhs) env
    add_bind (Rec pairs)	      env = foldr add_pair env pairs

    add_pair :: (Id,CoreExpr) -> IndEnv -> IndEnv
    add_pair (exported_id, Var local_id) env
	| shortMeOut env exported_id local_id = extendVarEnv env local_id exported_id
    add_pair (exported_id, rhs) env
	= env
			
shortMeOut ind_env exported_id local_id
-- The if-then-else stuff is just so I can get a pprTrace to see
-- how often I don't get shorting out becuase of IdInfo stuff
  = if isExportedId exported_id &&		-- Only if this is exported

       isLocalId local_id &&			-- Only if this one is defined in this
						-- 	module, so that we *can* change its
				 	 	-- 	binding to be the exported thing!

       not (isExportedId local_id) &&		-- Only if this one is not itself exported,
					   	--	since the transformation will nuke it
   
       not (local_id `elemVarEnv` ind_env)	-- Only if not already substituted for
    then
	True

{- No longer needed
	if isEmptySpecInfo (specInfo (idInfo exported_id)) 	-- Only if no rules
	then True	-- See note on "Messing up rules"
	else 
#ifdef DEBUG 
          pprTrace "shortMeOut:" (ppr exported_id)
#endif
                                                False
-}
    else
	False


-----------------
transferIdInfo :: Id -> Id -> Id
transferIdInfo exported_id local_id
  = modifyIdInfo transfer exported_id
  where
    local_info = idInfo local_id
    transfer exp_info = exp_info `setNewStrictnessInfo` newStrictnessInfo local_info
				 `setWorkerInfo`        workerInfo local_info
				 `setSpecInfo`	        addSpecInfo (specInfo exp_info)
							            (specInfo local_info)
\end{code}
