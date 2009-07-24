%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SimplCore]{Driver for simplifying @Core@ programs}

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module SimplCore ( core2core, simplifyExpr ) where

#include "HsVersions.h"

import DynFlags		( CoreToDo(..), SimplifierSwitch(..),
			  SimplifierMode(..), DynFlags, DynFlag(..), dopt,
			  getCoreToDo, shouldDumpSimplPhase )
import CoreSyn
import HscTypes
import CSE		( cseProgram )
import Rules		( RuleBase, emptyRuleBase, mkRuleBase, unionRuleBase,
			  extendRuleBaseList, pprRuleBase, pprRulesForUser,
			  ruleCheckProgram, rulesOfBinds,
			  addSpecInfo, addIdSpecialisations )
import PprCore		( pprCoreBindings, pprCoreExpr, pprRules )
import OccurAnal	( occurAnalysePgm, occurAnalyseExpr )
import IdInfo
import CoreUtils	( coreBindsSize )
import Simplify		( simplTopBinds, simplExpr )
import SimplEnv		( SimplEnv, simplBinders, mkSimplEnv, setInScopeSet )
import SimplMonad
import CoreMonad
import qualified ErrUtils as Err        ( dumpIfSet_dyn, dumpIfSet, showPass )
import CoreLint		( showPass, endPass, endPassIf, endIteration )
import FloatIn		( floatInwards )
import FloatOut		( floatOutwards )
import FamInstEnv
import Id
import DataCon
import TyCon		( tyConDataCons )
import Class		( classSelIds )
import BasicTypes       ( CompilerPhase, isActive, isDefaultInlinePragma )
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
import Vectorise        ( vectorise )
import FastString
import Util

import UniqSupply	( UniqSupply, mkSplitUniqSupply, splitUniqSupply )
import Outputable
import Control.Monad
import Data.List
import System.IO
import Maybes
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

core2core hsc_env guts = do
    let dflags = hsc_dflags hsc_env

    us <- mkSplitUniqSupply 's'
    let (cp_us, ru_us) = splitUniqSupply us

    -- COMPUTE THE ANNOTATIONS TO USE
    ann_env <- prepareAnnotations hsc_env (Just guts)

    -- COMPUTE THE RULE BASE TO USE
    (imp_rule_base, guts1) <- prepareRules hsc_env guts ru_us

    -- Get the module out of the current HscEnv so we can retrieve it from the monad.
    -- This is very convienent for the users of the monad (e.g. plugins do not have to
    -- consume the ModGuts to find the module) but somewhat ugly because mg_module may
    -- _theoretically_ be changed during the Core pipeline (it's part of ModGuts), which
    -- would mean our cached value would go out of date.
    let mod = mg_module guts
    (guts2, stats) <- runCoreM hsc_env ann_env imp_rule_base cp_us mod $ do
        -- FIND BUILT-IN PASSES
        let builtin_core_todos = getCoreToDo dflags

        -- DO THE BUSINESS
        doCorePasses builtin_core_todos guts1

    Err.dumpIfSet_dyn dflags Opt_D_dump_simpl_stats
        "Grand total simplifier statistics"
        (pprSimplCount stats)

    return guts2


type CorePass = CoreToDo

simplifyExpr :: DynFlags -- includes spec of what core-to-core passes to do
	     -> CoreExpr
	     -> IO CoreExpr
-- simplifyExpr is called by the driver to simplify an
-- expression typed in at the interactive prompt
simplifyExpr dflags expr
  = do	{
	; Err.showPass dflags "Simplify"

	; us <-  mkSplitUniqSupply 's'

	; let (expr', _counts) = initSmpl dflags emptyRuleBase emptyFamInstEnvs us $
				 simplExprGently gentleSimplEnv expr

	; Err.dumpIfSet_dyn dflags Opt_D_dump_simpl "Simplified expression"
			(pprCoreExpr expr')

	; return expr'
	}

gentleSimplEnv :: SimplEnv
gentleSimplEnv = mkSimplEnv SimplGently  (isAmongSimpl [])

doCorePasses :: [CorePass] -> ModGuts -> CoreM ModGuts
doCorePasses passes guts = foldM (flip doCorePass) guts passes

doCorePass :: CorePass -> ModGuts -> CoreM ModGuts
doCorePass (CoreDoSimplify mode sws) = {-# SCC "Simplify" #-}
                                       simplifyPgm mode sws

doCorePass CoreCSE                   = {-# SCC "CommonSubExpr" #-}   
	   			       describePass "Common sub-expression" Opt_D_dump_cse $ 
				       doPass cseProgram

doCorePass CoreLiberateCase          = {-# SCC "LiberateCase" #-}
	   			       describePass "Liberate case" Opt_D_verbose_core2core $ 
                                       doPassD liberateCase

doCorePass CoreDoFloatInwards        = {-# SCC "FloatInwards" #-}
                                       describePass "Float inwards" Opt_D_verbose_core2core $ 
                                       doPass floatInwards

doCorePass (CoreDoFloatOutwards f)   = {-# SCC "FloatOutwards" #-}
                                       describePassD (text "Float out" <+> parens (ppr f)) 
                                                     Opt_D_verbose_core2core $ 
                                       doPassDUM (floatOutwards f)

doCorePass CoreDoStaticArgs          = {-# SCC "StaticArgs" #-}
                                       describePass "Static argument" Opt_D_verbose_core2core $ 
                                       doPassU doStaticArgs

doCorePass CoreDoStrictness          = {-# SCC "Stranal" #-}
                                       describePass "Demand analysis" Opt_D_dump_stranal $
                                       doPassDM dmdAnalPgm

doCorePass CoreDoWorkerWrapper       = {-# SCC "WorkWrap" #-}
                                       describePass "Worker Wrapper binds" Opt_D_dump_worker_wrapper $
                                       doPassU wwTopBinds

doCorePass CoreDoSpecialising        = {-# SCC "Specialise" #-}
                                       describePassR "Specialise" Opt_D_dump_spec $ 
                                       doPassU specProgram

doCorePass CoreDoSpecConstr          = {-# SCC "SpecConstr" #-}
                                       describePassR "SpecConstr" Opt_D_dump_spec $
                                       doPassDU  specConstrProgram

doCorePass (CoreDoVectorisation be)  = {-# SCC "Vectorise" #-}
                                       describePass "Vectorisation" Opt_D_dump_vect $ 
                                       vectorise be

doCorePass CoreDoGlomBinds              = dontDescribePass $ doPassDM  glomBinds
doCorePass CoreDoPrintCore              = dontDescribePass $ observe   printCore
doCorePass (CoreDoRuleCheck phase pat)  = dontDescribePass $ ruleCheck phase pat

#ifdef OLD_STRICTNESS
doCorePass CoreDoOldStrictness          = {-# SCC "OldStrictness" #-} doOldStrictness
#endif

doCorePass CoreDoNothing                = return
doCorePass (CoreDoPasses passes)        = doCorePasses passes

#ifdef OLD_STRICTNESS
doOldStrictness :: ModGuts -> CoreM ModGuts
doOldStrictness guts
  = do dfs <- getDynFlags
       guts'  <- describePass "Strictness analysis" Opt_D_dump_stranal $ 
                 doPassM (saBinds dfs) guts
       guts'' <- describePass "Constructed Product analysis" Opt_D_dump_cpranal $ 
                 doPass cprAnalyse guts'
       return guts''
#endif

\end{code}

%************************************************************************
%*									*
\subsection{Core pass combinators}
%*									*
%************************************************************************

\begin{code}

dontDescribePass :: (ModGuts -> CoreM ModGuts) -> ModGuts -> CoreM ModGuts
dontDescribePass = ($)

describePass :: String -> DynFlag -> (ModGuts -> CoreM ModGuts) -> ModGuts -> CoreM ModGuts
describePass name dflag pass guts = do
    dflags <- getDynFlags
    
    liftIO $ showPass dflags name
    guts' <- pass guts
    liftIO $ endPass dflags name dflag (mg_binds guts')
    
    return guts'

describePassD :: SDoc -> DynFlag -> (ModGuts -> CoreM ModGuts) -> ModGuts -> CoreM ModGuts
describePassD doc = describePass (showSDoc doc)

describePassR :: String -> DynFlag -> (ModGuts -> CoreM ModGuts) -> ModGuts -> CoreM ModGuts
describePassR name dflag pass guts = do
    guts' <- describePass name dflag pass guts
    dumpIfSet_dyn Opt_D_dump_rules "Top-level specialisations"
                (pprRulesForUser (rulesOfBinds (mg_binds guts')))
    return guts'

printCore _ binds = Err.dumpIfSet True "Print Core" (pprCoreBindings binds)

ruleCheck :: CompilerPhase -> String -> ModGuts -> CoreM ModGuts
ruleCheck current_phase pat guts = do
    let is_active = isActive current_phase
    rb <- getRuleBase
    dflags <- getDynFlags
    liftIO $ Err.showPass dflags "RuleCheck"
    liftIO $ printDump (ruleCheckProgram is_active pat rb (mg_binds guts))
    return guts


doPassDMS :: (DynFlags -> [CoreBind] -> IO (SimplCount, [CoreBind])) -> ModGuts -> CoreM ModGuts
doPassDMS do_pass = doPassM $ \binds -> do
    dflags <- getDynFlags
    liftIOWithCount $ do_pass dflags binds

doPassDUM :: (DynFlags -> UniqSupply -> [CoreBind] -> IO [CoreBind]) -> ModGuts -> CoreM ModGuts
doPassDUM do_pass = doPassM $ \binds -> do
    dflags <- getDynFlags
    us     <- getUniqueSupplyM
    liftIO $ do_pass dflags us binds

doPassDM :: (DynFlags -> [CoreBind] -> IO [CoreBind]) -> ModGuts -> CoreM ModGuts
doPassDM do_pass = doPassDUM (\dflags -> const (do_pass dflags))

doPassD :: (DynFlags -> [CoreBind] -> [CoreBind]) -> ModGuts -> CoreM ModGuts
doPassD do_pass = doPassDM (\dflags -> return . do_pass dflags)

doPassDU :: (DynFlags -> UniqSupply -> [CoreBind] -> [CoreBind]) -> ModGuts -> CoreM ModGuts
doPassDU do_pass = doPassDUM (\dflags us -> return . do_pass dflags us)

doPassU :: (UniqSupply -> [CoreBind] -> [CoreBind]) -> ModGuts -> CoreM ModGuts
doPassU do_pass = doPassDU (const do_pass)

-- Most passes return no stats and don't change rules: these combinators
-- let us lift them to the full blown ModGuts+CoreM world
doPassM :: Monad m => ([CoreBind] -> m [CoreBind]) -> ModGuts -> m ModGuts
doPassM bind_f guts = do
    binds' <- bind_f (mg_binds guts)
    return (guts { mg_binds = binds' })

doPassMG :: Monad m => (ModGuts -> m [CoreBind]) -> ModGuts -> m ModGuts
doPassMG bind_f guts = do
    binds' <- bind_f guts
    return (guts { mg_binds = binds' })

doPass :: ([CoreBind] -> [CoreBind]) -> ModGuts -> CoreM ModGuts
doPass bind_f guts = return $ guts { mg_binds = bind_f (mg_binds guts) }

-- Observer passes just peek; don't modify the bindings at all
observe :: (DynFlags -> [CoreBind] -> IO a) -> ModGuts -> CoreM ModGuts
observe do_pass = doPassM $ \binds -> do
    dflags <- getDynFlags
    liftIO $ do_pass dflags binds
    return binds
\end{code}


%************************************************************************
%*									*
	Dealing with rules
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
	     guts@(ModGuts { mg_binds = binds, mg_deps = deps 
	     		   , mg_rules = local_rules, mg_rdr_env = rdr_env })
	     us 
  = do	{ let 	-- Simplify the local rules; boringly, we need to make an in-scope set
		-- from the local binders, to avoid warnings from Simplify.simplVar
	      local_ids        = mkInScopeSet (mkVarSet (bindersOfBinds binds))
	      env	       = setInScopeSet gentleSimplEnv local_ids 
	      (better_rules,_) = initSmpl dflags emptyRuleBase emptyFamInstEnvs us $
				 (mapM (simplRule env) local_rules)
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

	; Err.dumpIfSet_dyn dflags Opt_D_dump_rules "Transformation rules"
		(withPprStyle (mkUserStyle (mkPrintUnqualified dflags rdr_env) AllTheWay) $
		 vcat [text "Local rules", pprRules better_rules,
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

Note [Simplifying the left-hand side of a RULE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must do some gentle simplification on the lhs (template) of each
rule.  The case that forced me to add this was the fold/build rule,
which without simplification looked like:
	fold k z (build (/\a. g a))  ==>  ...
This doesn't match unless you do eta reduction on the build argument.
Similarly for a LHS like
	augment g (build h) 
we do not want to get
	augment (\a. g a) (build h)
otherwise we don't match when given an argument like
	augment (\a. h a a) (build h)

\begin{code}
simplRule env rule@(BuiltinRule {})
  = return rule
simplRule env rule@(Rule { ru_bndrs = bndrs, ru_args = args, ru_rhs = rhs })
  = do (env, bndrs') <- simplBinders env bndrs
       args' <- mapM (simplExprGently env) args
       rhs' <- simplExprGently env rhs
       return (rule { ru_bndrs = bndrs', ru_args = args', ru_rhs = rhs' })

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
--	(c) Template Haskell splices
--
-- The name 'Gently' suggests that the SimplifierMode is SimplGently,
-- and in fact that is so.... but the 'Gently' in simplExprGently doesn't
-- enforce that; it just simplifies the expression twice

-- It's important that simplExprGently does eta reduction; see
-- Note [Simplifying the left-hand side of a RULE] above.  The
-- simplifier does indeed do eta reduction (it's in Simplify.completeLam)
-- but only if -O is on.

simplExprGently env expr = do
    expr1 <- simplExpr env (occurAnalyseExpr expr)
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
  = do { Err.showPass dflags "GlomBinds" ;
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
simplifyPgm :: SimplifierMode -> [SimplifierSwitch] -> ModGuts -> CoreM ModGuts
simplifyPgm mode switches
  = describePassD doc Opt_D_dump_simpl_phases $ \guts -> 
    do { hsc_env <- getHscEnv
       ; us <- getUniqueSupplyM
       ; rb <- getRuleBase
       ; let fam_inst_env = mg_fam_inst_env guts
             dump_phase = shouldDumpSimplPhase (hsc_dflags hsc_env) mode
	     simplify_pgm = simplifyPgmIO dump_phase mode switches 
                                          hsc_env us rb fam_inst_env

       ; doPassM (liftIOWithCount . simplify_pgm) guts }
  where
    doc = ptext (sLit "Simplifier Phase") <+> text (showPpr mode) 

simplifyPgmIO :: Bool
            -> SimplifierMode
	    -> [SimplifierSwitch]
	    -> HscEnv
	    -> UniqSupply
	    -> RuleBase
	    -> FamInstEnv
	    -> [CoreBind]
	    -> IO (SimplCount, [CoreBind])  -- New bindings

simplifyPgmIO dump_phase mode switches hsc_env us imp_rule_base fam_inst_env binds
  = do {
	(termination_msg, it_count, counts_out, binds') 
	   <- do_iteration us 1 (zeroSimplCount dflags) binds ;

	Err.dumpIfSet (dump_phase && dopt Opt_D_dump_simpl_stats dflags)
		  "Simplifier statistics for following pass"
		  (vcat [text termination_msg <+> text "after" <+> ppr it_count <+> text "iterations",
			 text "",
			 pprSimplCount counts_out]);

	return (counts_out, binds')
    }
  where
    dflags 	   = hsc_dflags hsc_env
		   
    sw_chkr	   = isAmongSimpl switches
    max_iterations = intSwitchSet sw_chkr MaxSimplifierIterations `orElse` 2
 
    do_iteration us iteration_no counts binds
	-- iteration_no is the number of the iteration we are
	-- about to begin, with '1' for the first
      | iteration_no > max_iterations	-- Stop if we've run out of iterations
      =  WARN(debugIsOn && (max_iterations > 2),
                text ("Simplifier still going after " ++
				show max_iterations ++
			    	" iterations; bailing out.  Size = " ++ show (coreBindsSize binds) ++ "\n" ))
		-- Subtract 1 from iteration_no to get the
		-- number of iterations we actually completed
	    return ("Simplifier bailed out", iteration_no - 1, counts, binds)

      -- Try and force thunks off the binds; significantly reduces
      -- space usage, especially with -O.  JRS, 000620.
      | let sz = coreBindsSize binds in sz == sz
      = do {
		-- Occurrence analysis
	   let { tagged_binds = {-# SCC "OccAnal" #-} occurAnalysePgm binds } ;
	   Err.dumpIfSet_dyn dflags Opt_D_dump_occur_anal "Occurrence analysis"
		     (pprCoreBindings tagged_binds);

	   	-- Get any new rules, and extend the rule base
		-- We need to do this regularly, because simplification can
		-- poke on IdInfo thunks, which in turn brings in new rules
		-- behind the scenes.  Otherwise there's a danger we'll simply
		-- miss the rules for Ids hidden inside imported inlinings
	   eps <- hscEPS hsc_env ;
	   let	{ rule_base' = unionRuleBase imp_rule_base (eps_rule_base eps)
		; simpl_env  = mkSimplEnv mode sw_chkr 
		; simpl_binds = {-# SCC "SimplTopBinds" #-} 
				simplTopBinds simpl_env tagged_binds
		; fam_envs = (eps_fam_inst_env eps, fam_inst_env) } ;
	   
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
	   case initSmpl dflags rule_base' fam_envs us1 simpl_binds of {
	  	(binds', counts') -> do {

	   let	{ all_counts = counts `plusSimplCount` counts'
		; herald     = "Simplifier mode " ++ showPpr mode ++ 
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
		--
		-- ToDo: alas, this means that indirection-shorting does not happen at all
		--	 if the simplifier does nothing (not common, I know, but unsavoury)
	   let { binds'' = {-# SCC "ZapInd" #-} shortOutIndirections binds' } ;

		-- Dump the result of this iteration
	   Err.dumpIfSet_dyn dflags Opt_D_dump_simpl_iterations herald
		         (pprSimplCount counts') ;
	   endIteration dflags herald Opt_D_dump_simpl_iterations binds'' ;

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

Note [Transferring IdInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to propagage any useful IdInfo on x_local to x_exported.

STRICTNESS: if we have done strictness analysis, we want the strictness info on
x_local to transfer to x_exported.  Hence the copyIdInfo call.

RULES: we want to *add* any RULES for x_local to x_exported.


Note [Messing up the exported Id's IdInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must be careful about discarding the IdInfo on the old Id

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

Old "solution": 
	use rule switching-off pragmas to get rid 
	of iterateList in the first place

But in principle the user *might* want rules that only apply to the Id
he says.  And inline pragmas are similar
   {-# NOINLINE f #-}
   f = local
   local = <stuff>
Then we do not want to get rid of the NOINLINE.

Hence hasShortableIdinfo.


Note [Rules and indirection-zapping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Problem: what if x_exported has a RULE that mentions something in ...bindings...?
Then the things mentioned can be out of scope!  Solution
 a) Make sure that in this pass the usage-info from x_exported is 
	available for ...bindings...
 b) If there are any such RULES, rec-ify the entire top-level. 
    It'll get sorted out next time round

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
  | no_need_to_flatten	  = binds'			-- See Note [Rules and indirect-zapping]
  | otherwise 		  = [Rec (flattenBinds binds')]	-- for this no_need_to_flatten stuff
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
			
-----------------
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
	if hasShortableIdInfo exported_id
	then True	-- See Note [Messing up the exported Id's IdInfo]
	else WARN( True, ptext (sLit "Not shorting out:") <+> ppr exported_id )
             False
    else
        False

-----------------
hasShortableIdInfo :: Id -> Bool
-- True if there is no user-attached IdInfo on exported_id,
-- so we can safely discard it
-- See Note [Messing up the exported Id's IdInfo]
hasShortableIdInfo id
  =  isEmptySpecInfo (specInfo info)
  && isDefaultInlinePragma (inlinePragInfo info)
  where
     info = idInfo id

-----------------
transferIdInfo :: Id -> Id -> Id
-- See Note [Transferring IdInfo]
-- If we have
--	lcl_id = e; exp_id = lcl_id
-- and lcl_id has useful IdInfo, we don't want to discard it by going
--	gbl_id = e; lcl_id = gbl_id
-- Instead, transfer IdInfo from lcl_id to exp_id
-- Overwriting, rather than merging, seems to work ok.
transferIdInfo exported_id local_id
  = modifyIdInfo transfer exported_id
  where
    local_info = idInfo local_id
    transfer exp_info = exp_info `setNewStrictnessInfo` newStrictnessInfo local_info
				 `setWorkerInfo`        workerInfo local_info
				 `setInlinePragInfo`	inlinePragInfo local_info
				 `setSpecInfo`	        addSpecInfo (specInfo exp_info) new_info
    new_info = setSpecInfoHead (idName exported_id) 
			       (specInfo local_info)
	-- Remember to set the function-name field of the
	-- rules as we transfer them from one function to another
\end{code}
