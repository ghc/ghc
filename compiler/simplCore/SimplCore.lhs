%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SimplCore]{Driver for simplifying @Core@ programs}

\begin{code}
module SimplCore ( core2core, simplifyExpr ) where

#include "HsVersions.h"

import DynFlags		( DynFlags, DynFlag(..), dopt )
import CoreSyn
import CoreSubst
import HscTypes
import CSE		( cseProgram )
import Rules		( RuleBase, emptyRuleBase, mkRuleBase, unionRuleBase,
			  extendRuleBaseList, ruleCheckProgram, addSpecInfo, )
import PprCore		( pprCoreBindings, pprCoreExpr )
import OccurAnal	( occurAnalysePgm, occurAnalyseExpr )
import IdInfo
import CoreUtils	( coreBindsSize )
import Simplify		( simplTopBinds, simplExpr )
import SimplUtils	( simplEnvForGHCi, activeRule )
import SimplEnv
import SimplMonad
import CoreMonad
import qualified ErrUtils as Err 
import FloatIn		( floatInwards )
import FloatOut		( floatOutwards )
import FamInstEnv
import Id
import BasicTypes       ( CompilerPhase, isDefaultInlinePragma )
import VarSet
import VarEnv
import LiberateCase	( liberateCase )
import SAT		( doStaticArgs )
import Specialise	( specProgram)
import SpecConstr	( specConstrProgram)
import DmdAnal		( dmdAnalPgm )
import WorkWrap	        ( wwTopBinds )
import Vectorise        ( vectorise )
import FastString
import Util

import UniqSupply	( UniqSupply, mkSplitUniqSupply, splitUniqSupply )
import Outputable
import Control.Monad
\end{code}

%************************************************************************
%*									*
\subsection{The driver for the simplifier}
%*									*
%************************************************************************

\begin{code}
core2core :: HscEnv -> ModGuts -> IO ModGuts
core2core hsc_env guts 
  = do { us <- mkSplitUniqSupply 's'
       ; (guts2, stats) <- runCoreM hsc_env hpt_rule_base us mod $ 
                           doCorePasses (getCoreToDo dflags) guts

       ; Err.dumpIfSet_dyn dflags Opt_D_dump_simpl_stats
             "Grand total simplifier statistics"
             (pprSimplCount stats)

       ; return guts2 }
  where
    dflags         = hsc_dflags hsc_env
    home_pkg_rules = hptRules hsc_env (dep_mods (mg_deps guts))
    hpt_rule_base  = mkRuleBase home_pkg_rules
    mod            = mg_module guts
    -- mod: get the module out of the current HscEnv so we can retrieve it from the monad.
    -- This is very convienent for the users of the monad (e.g. plugins do not have to
    -- consume the ModGuts to find the module) but somewhat ugly because mg_module may
    -- _theoretically_ be changed during the Core pipeline (it's part of ModGuts), which
    -- would mean our cached value would go out of date.


type CorePass = CoreToDo

doCorePasses :: [CorePass] -> ModGuts -> CoreM ModGuts
doCorePasses passes guts 
  = foldM do_pass guts passes
  where
    do_pass guts CoreDoNothing = return guts
    do_pass guts (CoreDoPasses ps) = doCorePasses ps guts
    do_pass guts pass 
       = do { dflags <- getDynFlags
       	    ; liftIO $ showPass dflags pass
       	    ; guts' <- doCorePass pass guts
       	    ; liftIO $ endPass dflags pass (mg_binds guts') (mg_rules guts')
       	    ; return guts' }

doCorePass :: CorePass -> ModGuts -> CoreM ModGuts
doCorePass pass@(CoreDoSimplify {})  = {-# SCC "Simplify" #-}
                                       simplifyPgm pass

doCorePass CoreCSE                   = {-# SCC "CommonSubExpr" #-}   
				       doPass cseProgram

doCorePass CoreLiberateCase          = {-# SCC "LiberateCase" #-}
                                       doPassD liberateCase

doCorePass CoreDoFloatInwards        = {-# SCC "FloatInwards" #-}
                                       doPass floatInwards

doCorePass (CoreDoFloatOutwards f)   = {-# SCC "FloatOutwards" #-}
                                       doPassDUM (floatOutwards f)

doCorePass CoreDoStaticArgs          = {-# SCC "StaticArgs" #-}
                                       doPassU doStaticArgs

doCorePass CoreDoStrictness          = {-# SCC "Stranal" #-}
                                       doPassDM dmdAnalPgm

doCorePass CoreDoWorkerWrapper       = {-# SCC "WorkWrap" #-}
                                       doPassU wwTopBinds

doCorePass CoreDoSpecialising        = {-# SCC "Specialise" #-}
                                       specProgram

doCorePass CoreDoSpecConstr          = {-# SCC "SpecConstr" #-}
                                       specConstrProgram

doCorePass CoreDoVectorisation       = {-# SCC "Vectorise" #-}
                                       vectorise

doCorePass CoreDoGlomBinds              = doPassDM  glomBinds
doCorePass CoreDoPrintCore              = observe   printCore
doCorePass (CoreDoRuleCheck phase pat)  = ruleCheck phase pat
doCorePass CoreDoNothing                = return
doCorePass (CoreDoPasses passes)        = doCorePasses passes
doCorePass pass = pprPanic "doCorePass" (ppr pass)
\end{code}

%************************************************************************
%*									*
\subsection{Core pass combinators}
%*									*
%************************************************************************

\begin{code}
printCore :: a -> [CoreBind] -> IO ()
printCore _ binds = Err.dumpIfSet True "Print Core" (pprCoreBindings binds)

ruleCheck :: CompilerPhase -> String -> ModGuts -> CoreM ModGuts
ruleCheck current_phase pat guts = do
    rb <- getRuleBase
    dflags <- getDynFlags
    liftIO $ Err.showPass dflags "RuleCheck"
    liftIO $ printDump (ruleCheckProgram current_phase pat rb (mg_binds guts))
    return guts


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

doPass :: ([CoreBind] -> [CoreBind]) -> ModGuts -> CoreM ModGuts
doPass bind_f guts = return $ guts { mg_binds = bind_f (mg_binds guts) }

-- Observer passes just peek; don't modify the bindings at all
observe :: (DynFlags -> [CoreBind] -> IO a) -> ModGuts -> CoreM ModGuts
observe do_pass = doPassM $ \binds -> do
    dflags <- getDynFlags
    _ <- liftIO $ do_pass dflags binds
    return binds
\end{code}


%************************************************************************
%*									*
	Gentle simplification
%*									*
%************************************************************************

\begin{code}
simplifyExpr :: DynFlags -- includes spec of what core-to-core passes to do
	     -> CoreExpr
	     -> IO CoreExpr
-- simplifyExpr is called by the driver to simplify an
-- expression typed in at the interactive prompt
--
-- Also used by Template Haskell
simplifyExpr dflags expr
  = do	{
	; Err.showPass dflags "Simplify"

	; us <-  mkSplitUniqSupply 's'

	; let (expr', _counts) = initSmpl dflags emptyRuleBase emptyFamInstEnvs us $
				 simplExprGently simplEnvForGHCi expr

	; Err.dumpIfSet_dyn dflags Opt_D_dump_simpl "Simplified expression"
			(pprCoreExpr expr')

	; return expr'
	}

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
simplifyPgm :: CoreToDo -> ModGuts -> CoreM ModGuts
simplifyPgm pass guts
  = do { hsc_env <- getHscEnv
       ; us <- getUniqueSupplyM
       ; rb <- getRuleBase
       ; liftIOWithCount $  
       	 simplifyPgmIO pass hsc_env us rb guts }

simplifyPgmIO :: CoreToDo
	      -> HscEnv
	      -> UniqSupply
	      -> RuleBase
	      -> ModGuts
	      -> IO (SimplCount, ModGuts)  -- New bindings

simplifyPgmIO pass@(CoreDoSimplify max_iterations mode)
              hsc_env us hpt_rule_base 
              guts@(ModGuts { mg_binds = binds, mg_rules = rules
                            , mg_fam_inst_env = fam_inst_env })
  = do { (termination_msg, it_count, counts_out, guts') 
	   <- do_iteration us 1 [] binds rules 

	; Err.dumpIfSet (dump_phase && dopt Opt_D_dump_simpl_stats dflags)
		  "Simplifier statistics for following pass"
		  (vcat [text termination_msg <+> text "after" <+> ppr it_count <+> text "iterations",
			 blankLine,
			 pprSimplCount counts_out])

	; return (counts_out, guts')
    }
  where
    dflags      = hsc_dflags hsc_env
    dump_phase  = dumpSimplPhase dflags mode
    simpl_env   = mkSimplEnv mode
    active_rule = activeRule dflags simpl_env

    do_iteration :: UniqSupply
                 -> Int		 -- Counts iterations
		 -> [SimplCount] -- Counts from earlier iterations, reversed
		 -> [CoreBind]	 -- Bindings in
		 -> [CoreRule]	 -- and orphan rules
		 -> IO (String, Int, SimplCount, ModGuts)

    do_iteration us iteration_no counts_so_far binds rules
	-- iteration_no is the number of the iteration we are
	-- about to begin, with '1' for the first
      | iteration_no > max_iterations	-- Stop if we've run out of iterations
      = WARN( debugIsOn && (max_iterations > 2)
            , ptext (sLit "Simplifier baling out after") <+> int max_iterations
              <+> ptext (sLit "iterations") 
              <+> (brackets $ hsep $ punctuate comma $ 
                   map (int . simplCountN) (reverse counts_so_far))
              <+> ptext (sLit "Size =") <+> int (coreBindsSize binds) )

		-- Subtract 1 from iteration_no to get the
		-- number of iterations we actually completed
	return ( "Simplifier baled out", iteration_no - 1 
               , totalise counts_so_far
               , guts { mg_binds = binds, mg_rules = rules } )

      -- Try and force thunks off the binds; significantly reduces
      -- space usage, especially with -O.  JRS, 000620.
      | let sz = coreBindsSize binds in sz == sz
      = do {
		-- Occurrence analysis
	   let { tagged_binds = {-# SCC "OccAnal" #-} 
                     occurAnalysePgm active_rule rules binds } ;
	   Err.dumpIfSet_dyn dflags Opt_D_dump_occur_anal "Occurrence analysis"
		     (pprCoreBindings tagged_binds);

	   	-- Get any new rules, and extend the rule base
		-- See Note [Overall plumbing for rules] in Rules.lhs
		-- We need to do this regularly, because simplification can
		-- poke on IdInfo thunks, which in turn brings in new rules
		-- behind the scenes.  Otherwise there's a danger we'll simply
		-- miss the rules for Ids hidden inside imported inlinings
	   eps <- hscEPS hsc_env ;
	   let	{ rule_base1 = unionRuleBase hpt_rule_base (eps_rule_base eps)
	        ; rule_base2 = extendRuleBaseList rule_base1 rules
		; simpl_binds = {-# SCC "SimplTopBinds" #-} 
				simplTopBinds simpl_env tagged_binds
		; fam_envs = (eps_fam_inst_env eps, fam_inst_env) } ;
	   
		-- Simplify the program
		-- We do this with a *case* not a *let* because lazy pattern
		-- matching bit us with bad space leak!
		-- With a let, we ended up with
		--   let
		--	t = initSmpl ...
		--	counts1 = snd t
		--   in
		-- 	case t of {(_,counts1) -> if counts1=0 then ... }
		-- So the conditional didn't force counts1, because the
		-- selection got duplicated.  Sigh!
	   case initSmpl dflags rule_base2 fam_envs us1 simpl_binds of {
	  	(env1, counts1) -> do {

	   let	{ binds1 = getFloats env1
                ; rules1 = substRulesForImportedIds (mkCoreSubst (text "imp-rules") env1) rules
	        } ;

		-- Stop if nothing happened; don't dump output
	   if isZeroSimplCount counts1 then
		return ( "Simplifier reached fixed point", iteration_no
                       , totalise (counts1 : counts_so_far)  -- Include "free" ticks	
		       , guts { mg_binds = binds1, mg_rules = rules1 } )
	   else do {
		-- Short out indirections
		-- We do this *after* at least one run of the simplifier 
		-- because indirection-shorting uses the export flag on *occurrences*
		-- and that isn't guaranteed to be ok until after the first run propagates
		-- stuff from the binding site to its occurrences
		--
		-- ToDo: alas, this means that indirection-shorting does not happen at all
		--	 if the simplifier does nothing (not common, I know, but unsavoury)
	   let { binds2 = {-# SCC "ZapInd" #-} shortOutIndirections binds1 } ;

		-- Dump the result of this iteration
	   end_iteration dflags pass iteration_no counts1 binds2 rules1 ;

		-- Loop
  	   do_iteration us2 (iteration_no + 1) (counts1:counts_so_far) binds2 rules1
           } } } }
      | otherwise = panic "do_iteration"
      where
  	(us1, us2) = splitUniqSupply us

	-- Remember the counts_so_far are reversed
        totalise :: [SimplCount] -> SimplCount
        totalise = foldr (\c acc -> acc `plusSimplCount` c) 
                         (zeroSimplCount dflags) 

simplifyPgmIO _ _ _ _ _ = panic "simplifyPgmIO"

-------------------
end_iteration :: DynFlags -> CoreToDo -> Int 
             -> SimplCount -> [CoreBind] -> [CoreRule] -> IO ()
-- Same as endIteration but with simplifier counts
end_iteration dflags pass iteration_no counts binds rules
  = do { dumpIfSet (dopt Opt_D_dump_simpl_iterations dflags)
                   pass (ptext (sLit "Simplifier counts"))
		   (pprSimplCount counts)

       ; endIteration dflags pass iteration_no binds rules }
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


Note [Messing up the exported Id's RULES]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must be careful about discarding (obviously) or even merging the
RULES on the exported Id. The example that went bad on me at one stage
was this one:
	
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
    add_pair _ env = env
			
-----------------
shortMeOut :: IndEnv -> Id -> Id -> Bool
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
    transfer exp_info = exp_info `setStrictnessInfo` strictnessInfo local_info
				 `setUnfoldingInfo`     unfoldingInfo local_info
				 `setInlinePragInfo`	inlinePragInfo local_info
				 `setSpecInfo`	        addSpecInfo (specInfo exp_info) new_info
    new_info = setSpecInfoHead (idName exported_id) 
			       (specInfo local_info)
	-- Remember to set the function-name field of the
	-- rules as we transfer them from one function to another
\end{code}
