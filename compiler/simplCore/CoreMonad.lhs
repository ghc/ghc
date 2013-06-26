%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[CoreMonad]{The core pipeline monad}

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

{-# LANGUAGE UndecidableInstances #-}

module CoreMonad (
    -- * Configuration of the core-to-core passes
    CoreToDo(..), runWhen, runMaybe,
    SimplifierMode(..),
    FloatOutSwitches(..),
    dumpSimplPhase, pprPassDetails, 

    -- * Plugins
    PluginPass, Plugin(..), CommandLineOption, 
    defaultPlugin, bindsOnlyPass,

    -- * Counting
    SimplCount, doSimplTick, doFreeSimplTick, simplCountN,
    pprSimplCount, plusSimplCount, zeroSimplCount, 
    isZeroSimplCount, hasDetailedCounts, Tick(..),

    -- * The monad
    CoreM, runCoreM,
    
    -- ** Reading from the monad
    getHscEnv, getRuleBase, getModule,
    getDynFlags, getOrigNameCache,
    
    -- ** Writing to the monad
    addSimplCount,
    
    -- ** Lifting into the monad
    liftIO, liftIOWithCount,
    liftIO1, liftIO2, liftIO3, liftIO4,
    
    -- ** Global initialization
    reinitializeGlobals, bracketGlobals,
    
    -- ** Dealing with annotations
    getAnnotations, getFirstAnnotations,
    
    -- ** Debug output
    showPass, endPass, dumpPassResult, lintPassResult, dumpIfSet,

    -- ** Screen output
    putMsg, putMsgS, errorMsg, errorMsgS, 
    fatalErrorMsg, fatalErrorMsgS, 
    debugTraceMsg, debugTraceMsgS,
    dumpIfSet_dyn, 

#ifdef GHCI
    -- * Getting 'Name's
    thNameToGhcName
#endif
  ) where

#ifdef GHCI
import Name( Name )
#endif
import CoreSyn
import PprCore
import CoreUtils
import CoreLint		( lintCoreBindings )
import HscTypes
import Module
import DynFlags
import StaticFlags	
import Rules            ( RuleBase )
import BasicTypes       ( CompilerPhase(..) )
import Annotations
import Id		( Id )

import IOEnv hiding     ( liftIO, failM, failWithM )
import qualified IOEnv  ( liftIO )
import TcEnv            ( tcLookupGlobal )
import TcRnMonad        ( initTcForLookup )

import Outputable
import FastString
import qualified ErrUtils as Err
import Bag
import Maybes
import SrcLoc
import UniqSupply
import UniqFM       ( UniqFM, mapUFM, filterUFM )
import MonadUtils

import Util ( split )
import ListSetOps	( runs )
import Data.List
import Data.Ord
import Data.Dynamic
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word
import Control.Monad

import Prelude hiding   ( read )

#ifdef GHCI
import Control.Concurrent.MVar (MVar)
import Linker ( PersistentLinkerState, saveLinkerGlobals, restoreLinkerGlobals )
import {-# SOURCE #-} TcSplice ( lookupThName_maybe )
import qualified Language.Haskell.TH as TH
#else
saveLinkerGlobals :: IO ()
saveLinkerGlobals = return ()

restoreLinkerGlobals :: () -> IO ()
restoreLinkerGlobals () = return ()
#endif
\end{code}

%************************************************************************
%*									*
                       Debug output
%*									*
%************************************************************************

These functions are not CoreM monad stuff, but they probably ought to
be, and it makes a conveneint place.  place for them.  They print out
stuff before and after core passes, and do Core Lint when necessary.

\begin{code}
showPass :: DynFlags -> CoreToDo -> IO ()
showPass dflags pass = Err.showPass dflags (showPpr dflags pass)

endPass :: DynFlags -> CoreToDo -> CoreProgram -> [CoreRule] -> IO ()
endPass dflags pass binds rules
  = do { dumpPassResult dflags mb_flag (ppr pass) (pprPassDetails pass) binds rules
       ; lintPassResult dflags pass binds }      
  where
    mb_flag = case coreDumpFlag pass of
                Just flag | dopt flag dflags                    -> Just flag
                          | dopt Opt_D_verbose_core2core dflags -> Just flag
                _ -> Nothing

dumpIfSet :: DynFlags -> Bool -> CoreToDo -> SDoc -> SDoc -> IO ()
dumpIfSet dflags dump_me pass extra_info doc
  = Err.dumpIfSet dflags dump_me (showSDoc dflags (ppr pass <+> extra_info)) doc

dumpPassResult :: DynFlags 
               -> Maybe DumpFlag		-- Just df => show details in a file whose
	       	  			--            name is specified by df
               -> SDoc 			-- Header
               -> SDoc 			-- Extra info to appear after header
               -> CoreProgram -> [CoreRule] 
               -> IO ()
dumpPassResult dflags mb_flag hdr extra_info binds rules
  | Just flag <- mb_flag
  = Err.dumpSDoc dflags flag (showSDoc dflags hdr) dump_doc

  | otherwise
  = Err.debugTraceMsg dflags 2 size_doc
          -- Report result size 
	  -- This has the side effect of forcing the intermediate to be evaluated

  where
    size_doc = sep [text "Result size of" <+> hdr, nest 2 (equals <+> ppr (coreBindsStats binds))]

    dump_doc  = vcat [ nest 2 extra_info
		     , size_doc
                     , blankLine
                     , pprCoreBindings binds 
                     , ppUnless (null rules) pp_rules ]
    pp_rules = vcat [ blankLine
                    , ptext (sLit "------ Local rules for imported ids --------")
                    , pprRules rules ]

lintPassResult :: DynFlags -> CoreToDo -> CoreProgram -> IO ()
lintPassResult dflags pass binds
  = when (gopt Opt_DoCoreLinting dflags) $
    do { let (warns, errs) = lintCoreBindings binds
       ; Err.showPass dflags ("Core Linted result of " ++ showPpr dflags pass)
       ; displayLintResults dflags pass warns errs binds  }

displayLintResults :: DynFlags -> CoreToDo
                   -> Bag Err.MsgDoc -> Bag Err.MsgDoc -> CoreProgram
                   -> IO ()
displayLintResults dflags pass warns errs binds
  | not (isEmptyBag errs)
  = do { log_action dflags dflags Err.SevDump noSrcSpan defaultDumpStyle
           (vcat [ banner "errors", Err.pprMessageBag errs
                 , ptext (sLit "*** Offending Program ***")
                 , pprCoreBindings binds
                 , ptext (sLit "*** End of Offense ***") ])
       ; Err.ghcExit dflags 1 }

  | not (isEmptyBag warns)
  , not (case pass of { CoreDesugar -> True; _ -> False })
    	-- Suppress warnings after desugaring pass because some
	-- are legitimate. Notably, the desugarer generates instance
	-- methods with INLINE pragmas that form a mutually recursive
	-- group.  Only afer a round of simplification are they unravelled.
  , not opt_NoDebugOutput
  , showLintWarnings pass
  = log_action dflags dflags Err.SevDump noSrcSpan defaultDumpStyle
        (banner "warnings" $$ Err.pprMessageBag warns)

  | otherwise = return ()
  where
    banner string = ptext (sLit "*** Core Lint")      <+> text string 
                    <+> ptext (sLit ": in result of") <+> ppr pass
                    <+> ptext (sLit "***")

showLintWarnings :: CoreToDo -> Bool
-- Disable Lint warnings on the first simplifier pass, because
-- there may be some INLINE knots still tied, which is tiresomely noisy
showLintWarnings (CoreDoSimplify _ (SimplMode { sm_phase = InitialPhase })) = False
showLintWarnings _ = True
\end{code}


%************************************************************************
%*									*
              The CoreToDo type and related types
	  Abstraction of core-to-core passes to run.
%*									*
%************************************************************************

\begin{code}

data CoreToDo           -- These are diff core-to-core passes,
                        -- which may be invoked in any order,
                        -- as many times as you like.

  = CoreDoSimplify      -- The core-to-core simplifier.
        Int                    -- Max iterations
        SimplifierMode
  | CoreDoPluginPass String PluginPass
  | CoreDoFloatInwards
  | CoreDoFloatOutwards FloatOutSwitches
  | CoreLiberateCase
  | CoreDoPrintCore
  | CoreDoStaticArgs
  | CoreDoStrictness
  | CoreDoWorkerWrapper
  | CoreDoSpecialising
  | CoreDoSpecConstr
  | CoreCSE
  | CoreDoRuleCheck CompilerPhase String   -- Check for non-application of rules
                                           -- matching this string
  | CoreDoVectorisation
  | CoreDoNothing                -- Useful when building up
  | CoreDoPasses [CoreToDo]      -- lists of these things

  | CoreDesugar    -- Right after desugaring, no simple optimisation yet!
  | CoreDesugarOpt -- CoreDesugarXXX: Not strictly a core-to-core pass, but produces
                       --                 Core output, and hence useful to pass to endPass

  | CoreTidy
  | CorePrep

\end{code}

\begin{code}
coreDumpFlag :: CoreToDo -> Maybe DumpFlag
coreDumpFlag (CoreDoSimplify {})      = Just Opt_D_dump_simpl_phases
coreDumpFlag (CoreDoPluginPass {})    = Just Opt_D_dump_core_pipeline
coreDumpFlag CoreDoFloatInwards       = Just Opt_D_verbose_core2core
coreDumpFlag (CoreDoFloatOutwards {}) = Just Opt_D_verbose_core2core
coreDumpFlag CoreLiberateCase         = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoStaticArgs 	      = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoStrictness 	      = Just Opt_D_dump_stranal
coreDumpFlag CoreDoWorkerWrapper      = Just Opt_D_dump_worker_wrapper
coreDumpFlag CoreDoSpecialising       = Just Opt_D_dump_spec
coreDumpFlag CoreDoSpecConstr         = Just Opt_D_dump_spec
coreDumpFlag CoreCSE                  = Just Opt_D_dump_cse 
coreDumpFlag CoreDoVectorisation      = Just Opt_D_dump_vect
coreDumpFlag CoreDesugar              = Just Opt_D_dump_ds 
coreDumpFlag CoreDesugarOpt           = Just Opt_D_dump_ds 
coreDumpFlag CoreTidy                 = Just Opt_D_dump_simpl
coreDumpFlag CorePrep                 = Just Opt_D_dump_prep

coreDumpFlag CoreDoPrintCore         = Nothing
coreDumpFlag (CoreDoRuleCheck {})    = Nothing
coreDumpFlag CoreDoNothing           = Nothing
coreDumpFlag (CoreDoPasses {})       = Nothing

instance Outputable CoreToDo where
  ppr (CoreDoSimplify _ _)     = ptext (sLit "Simplifier")
  ppr (CoreDoPluginPass s _)   = ptext (sLit "Core plugin: ") <+> text s
  ppr CoreDoFloatInwards       = ptext (sLit "Float inwards")
  ppr (CoreDoFloatOutwards f)  = ptext (sLit "Float out") <> parens (ppr f)
  ppr CoreLiberateCase         = ptext (sLit "Liberate case")
  ppr CoreDoStaticArgs 	       = ptext (sLit "Static argument")
  ppr CoreDoStrictness 	       = ptext (sLit "Demand analysis")
  ppr CoreDoWorkerWrapper      = ptext (sLit "Worker Wrapper binds")
  ppr CoreDoSpecialising       = ptext (sLit "Specialise")
  ppr CoreDoSpecConstr         = ptext (sLit "SpecConstr")
  ppr CoreCSE                  = ptext (sLit "Common sub-expression")
  ppr CoreDoVectorisation      = ptext (sLit "Vectorisation")
  ppr CoreDesugar              = ptext (sLit "Desugar (before optimization)")
  ppr CoreDesugarOpt           = ptext (sLit "Desugar (after optimization)")
  ppr CoreTidy                 = ptext (sLit "Tidy Core")
  ppr CorePrep 		       = ptext (sLit "CorePrep")
  ppr CoreDoPrintCore          = ptext (sLit "Print core")
  ppr (CoreDoRuleCheck {})     = ptext (sLit "Rule check")
  ppr CoreDoNothing            = ptext (sLit "CoreDoNothing")
  ppr (CoreDoPasses {})        = ptext (sLit "CoreDoPasses")

pprPassDetails :: CoreToDo -> SDoc
pprPassDetails (CoreDoSimplify n md) = vcat [ ptext (sLit "Max iterations =") <+> int n 
                                            , ppr md ]
pprPassDetails _ = empty
\end{code}

\begin{code}
data SimplifierMode             -- See comments in SimplMonad
  = SimplMode
        { sm_names      :: [String] -- Name(s) of the phase
        , sm_phase      :: CompilerPhase
        , sm_rules      :: Bool     -- Whether RULES are enabled
        , sm_inline     :: Bool     -- Whether inlining is enabled
        , sm_case_case  :: Bool     -- Whether case-of-case is enabled
        , sm_eta_expand :: Bool     -- Whether eta-expansion is enabled
        }

instance Outputable SimplifierMode where
    ppr (SimplMode { sm_phase = p, sm_names = ss
                   , sm_rules = r, sm_inline = i
                   , sm_eta_expand = eta, sm_case_case = cc })
       = ptext (sLit "SimplMode") <+> braces (
         sep [ ptext (sLit "Phase =") <+> ppr p <+>
               brackets (text (concat $ intersperse "," ss)) <> comma
             , pp_flag i   (sLit "inline") <> comma
             , pp_flag r   (sLit "rules") <> comma
             , pp_flag eta (sLit "eta-expand") <> comma
             , pp_flag cc  (sLit "case-of-case") ])
	 where
           pp_flag f s = ppUnless f (ptext (sLit "no")) <+> ptext s
\end{code}


\begin{code}
data FloatOutSwitches = FloatOutSwitches {
  floatOutLambdas   :: Maybe Int,  -- ^ Just n <=> float lambdas to top level, if
                                   -- doing so will abstract over n or fewer 
                                   -- value variables
				   -- Nothing <=> float all lambdas to top level,
                                   --             regardless of how many free variables
                                   -- Just 0 is the vanilla case: float a lambda
                                   --    iff it has no free vars

  floatOutConstants :: Bool,       -- ^ True <=> float constants to top level,
                                   --            even if they do not escape a lambda
  floatOutPartialApplications :: Bool -- ^ True <=> float out partial applications
                                            --            based on arity information.
  }
instance Outputable FloatOutSwitches where
    ppr = pprFloatOutSwitches

pprFloatOutSwitches :: FloatOutSwitches -> SDoc
pprFloatOutSwitches sw 
  = ptext (sLit "FOS") <+> (braces $
     sep $ punctuate comma $ 
     [ ptext (sLit "Lam =")    <+> ppr (floatOutLambdas sw)
     , ptext (sLit "Consts =") <+> ppr (floatOutConstants sw)
     , ptext (sLit "PAPs =")   <+> ppr (floatOutPartialApplications sw) ])

-- The core-to-core pass ordering is derived from the DynFlags:
runWhen :: Bool -> CoreToDo -> CoreToDo
runWhen True  do_this = do_this
runWhen False _       = CoreDoNothing

runMaybe :: Maybe a -> (a -> CoreToDo) -> CoreToDo
runMaybe (Just x) f = f x
runMaybe Nothing  _ = CoreDoNothing


dumpSimplPhase :: DynFlags -> SimplifierMode -> Bool
dumpSimplPhase dflags mode
   | Just spec_string <- shouldDumpSimplPhase dflags
   = match_spec spec_string
   | otherwise
   = dopt Opt_D_verbose_core2core dflags

  where
    match_spec :: String -> Bool
    match_spec spec_string 
      = or $ map (and . map match . split ':') 
           $ split ',' spec_string

    match :: String -> Bool
    match "" = True
    match s  = case reads s of
                [(n,"")] -> phase_num  n
                _        -> phase_name s

    phase_num :: Int -> Bool
    phase_num n = case sm_phase mode of
                    Phase k -> n == k
                    _       -> False

    phase_name :: String -> Bool
    phase_name s = s `elem` sm_names mode
\end{code}


Note [RULEs enabled in SimplGently]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RULES are enabled when doing "gentle" simplification.  Two reasons:

  * We really want the class-op cancellation to happen:
        op (df d1 d2) --> $cop3 d1 d2
    because this breaks the mutual recursion between 'op' and 'df'

  * I wanted the RULE
        lift String ===> ...
    to work in Template Haskell when simplifying
    splices, so we get simpler code for literal strings

But watch out: list fusion can prevent floating.  So use phase control
to switch off those rules until after floating.


%************************************************************************
%*									*
             Types for Plugins
%*									*
%************************************************************************

\begin{code}
-- | Command line options gathered from the -PModule.Name:stuff syntax are given to you as this type
type CommandLineOption = String

-- | 'Plugin' is the core compiler plugin data type. Try to avoid
-- constructing one of these directly, and just modify some fields of
-- 'defaultPlugin' instead: this is to try and preserve source-code
-- compatability when we add fields to this.
--
-- Nonetheless, this API is preliminary and highly likely to change in the future.
data Plugin = Plugin { 
        installCoreToDos :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
                -- ^ Modify the Core pipeline that will be used for compilation. 
                -- This is called as the Core pipeline is built for every module
                --  being compiled, and plugins get the opportunity to modify 
                -- the pipeline in a nondeterministic order.
     }

-- | Default plugin: does nothing at all! For compatability reasons you should base all your
-- plugin definitions on this default value.
defaultPlugin :: Plugin
defaultPlugin = Plugin {
        installCoreToDos = const return
    }

-- | A description of the plugin pass itself
type PluginPass = ModGuts -> CoreM ModGuts

bindsOnlyPass :: (CoreProgram -> CoreM CoreProgram) -> ModGuts -> CoreM ModGuts
bindsOnlyPass pass guts
  = do { binds' <- pass (mg_binds guts)
       ; return (guts { mg_binds = binds' }) }
\end{code}


%************************************************************************
%*									*
             Counting and logging
%*									*
%************************************************************************

\begin{code}
verboseSimplStats :: Bool
verboseSimplStats = opt_PprStyle_Debug		-- For now, anyway

zeroSimplCount	   :: DynFlags -> SimplCount
isZeroSimplCount   :: SimplCount -> Bool
hasDetailedCounts  :: SimplCount -> Bool
pprSimplCount	   :: SimplCount -> SDoc
doSimplTick        :: DynFlags -> Tick -> SimplCount -> SimplCount
doFreeSimplTick    ::             Tick -> SimplCount -> SimplCount
plusSimplCount     :: SimplCount -> SimplCount -> SimplCount
\end{code}

\begin{code}
data SimplCount 
   = VerySimplCount !Int	-- Used when don't want detailed stats

   | SimplCount	{
	ticks   :: !Int,	-- Total ticks
	details :: !TickCounts,	-- How many of each type

	n_log	:: !Int,	-- N
	log1	:: [Tick],	-- Last N events; <= opt_HistorySize, 
		   		--   most recent first
	log2	:: [Tick]	-- Last opt_HistorySize events before that
		   		-- Having log1, log2 lets us accumulate the
				-- recent history reasonably efficiently
     }

type TickCounts = Map Tick Int

simplCountN :: SimplCount -> Int
simplCountN (VerySimplCount n)         = n
simplCountN (SimplCount { ticks = n }) = n

zeroSimplCount dflags
		-- This is where we decide whether to do
		-- the VerySimpl version or the full-stats version
  | dopt Opt_D_dump_simpl_stats dflags
  = SimplCount {ticks = 0, details = Map.empty,
                n_log = 0, log1 = [], log2 = []}
  | otherwise
  = VerySimplCount 0

isZeroSimplCount (VerySimplCount n)    	    = n==0
isZeroSimplCount (SimplCount { ticks = n }) = n==0

hasDetailedCounts (VerySimplCount {}) = False
hasDetailedCounts (SimplCount {})     = True

doFreeSimplTick tick sc@SimplCount { details = dts } 
  = sc { details = dts `addTick` tick }
doFreeSimplTick _ sc = sc 

doSimplTick dflags tick
    sc@(SimplCount { ticks = tks, details = dts, n_log = nl, log1 = l1 })
  | nl >= historySize dflags = sc1 { n_log = 1, log1 = [tick], log2 = l1 }
  | otherwise                = sc1 { n_log = nl+1, log1 = tick : l1 }
  where
    sc1 = sc { ticks = tks+1, details = dts `addTick` tick }

doSimplTick _ _ (VerySimplCount n) = VerySimplCount (n+1)


-- Don't use Map.unionWith because that's lazy, and we want to 
-- be pretty strict here!
addTick :: TickCounts -> Tick -> TickCounts
addTick fm tick = case Map.lookup tick fm of
			Nothing -> Map.insert tick 1 fm
			Just n  -> n1 `seq` Map.insert tick n1 fm
				where
				   n1 = n+1


plusSimplCount sc1@(SimplCount { ticks = tks1, details = dts1 })
	       sc2@(SimplCount { ticks = tks2, details = dts2 })
  = log_base { ticks = tks1 + tks2, details = Map.unionWith (+) dts1 dts2 }
  where
	-- A hackish way of getting recent log info
    log_base | null (log1 sc2) = sc1	-- Nothing at all in sc2
	     | null (log2 sc2) = sc2 { log2 = log1 sc1 }
	     | otherwise       = sc2

plusSimplCount (VerySimplCount n) (VerySimplCount m) = VerySimplCount (n+m)
plusSimplCount _                  _                  = panic "plusSimplCount"
       -- We use one or the other consistently

pprSimplCount (VerySimplCount n) = ptext (sLit "Total ticks:") <+> int n
pprSimplCount (SimplCount { ticks = tks, details = dts, log1 = l1, log2 = l2 })
  = vcat [ptext (sLit "Total ticks:    ") <+> int tks,
	  blankLine,
	  pprTickCounts dts,
	  if verboseSimplStats then
		vcat [blankLine,
		      ptext (sLit "Log (most recent first)"),
		      nest 4 (vcat (map ppr l1) $$ vcat (map ppr l2))]
	  else empty
    ]

pprTickCounts :: Map Tick Int -> SDoc
pprTickCounts counts
  = vcat (map pprTickGroup groups)
  where
    groups :: [[(Tick,Int)]]	-- Each group shares a comon tag
    	      			-- toList returns common tags adjacent
    groups = runs same_tag (Map.toList counts)
    same_tag (tick1,_) (tick2,_) = tickToTag tick1 == tickToTag tick2

pprTickGroup :: [(Tick, Int)] -> SDoc
pprTickGroup group@((tick1,_):_)
  = hang (int (sum [n | (_,n) <- group]) <+> text (tickString tick1))
       2 (vcat [ int n <+> pprTickCts tick  
                                    -- flip as we want largest first
               | (tick,n) <- sortBy (flip (comparing snd)) group])
pprTickGroup [] = panic "pprTickGroup"
\end{code}


\begin{code}
data Tick
  = PreInlineUnconditionally	Id
  | PostInlineUnconditionally	Id

  | UnfoldingDone    		Id
  | RuleFired			FastString	-- Rule name

  | LetFloatFromLet
  | EtaExpansion		Id	-- LHS binder
  | EtaReduction		Id	-- Binder on outer lambda
  | BetaReduction		Id	-- Lambda binder


  | CaseOfCase			Id	-- Bndr on *inner* case
  | KnownBranch			Id	-- Case binder
  | CaseMerge			Id	-- Binder on outer case
  | AltMerge			Id	-- Case binder
  | CaseElim			Id	-- Case binder
  | CaseIdentity		Id	-- Case binder
  | FillInCaseDefault		Id	-- Case binder

  | BottomFound		
  | SimplifierDone		-- Ticked at each iteration of the simplifier

instance Outputable Tick where
  ppr tick = text (tickString tick) <+> pprTickCts tick

instance Eq Tick where
  a == b = case a `cmpTick` b of
           EQ -> True
           _ -> False

instance Ord Tick where
  compare = cmpTick

tickToTag :: Tick -> Int
tickToTag (PreInlineUnconditionally _)	= 0
tickToTag (PostInlineUnconditionally _)	= 1
tickToTag (UnfoldingDone _)		= 2
tickToTag (RuleFired _)			= 3
tickToTag LetFloatFromLet		= 4
tickToTag (EtaExpansion _)		= 5
tickToTag (EtaReduction _)		= 6
tickToTag (BetaReduction _)		= 7
tickToTag (CaseOfCase _)		= 8
tickToTag (KnownBranch _)		= 9
tickToTag (CaseMerge _)			= 10
tickToTag (CaseElim _)			= 11
tickToTag (CaseIdentity _)		= 12
tickToTag (FillInCaseDefault _)		= 13
tickToTag BottomFound			= 14
tickToTag SimplifierDone		= 16
tickToTag (AltMerge _)			= 17

tickString :: Tick -> String
tickString (PreInlineUnconditionally _)	= "PreInlineUnconditionally"
tickString (PostInlineUnconditionally _)= "PostInlineUnconditionally"
tickString (UnfoldingDone _)		= "UnfoldingDone"
tickString (RuleFired _)		= "RuleFired"
tickString LetFloatFromLet		= "LetFloatFromLet"
tickString (EtaExpansion _)		= "EtaExpansion"
tickString (EtaReduction _)		= "EtaReduction"
tickString (BetaReduction _)		= "BetaReduction"
tickString (CaseOfCase _)		= "CaseOfCase"
tickString (KnownBranch _)		= "KnownBranch"
tickString (CaseMerge _)		= "CaseMerge"
tickString (AltMerge _)			= "AltMerge"
tickString (CaseElim _)			= "CaseElim"
tickString (CaseIdentity _)		= "CaseIdentity"
tickString (FillInCaseDefault _)	= "FillInCaseDefault"
tickString BottomFound			= "BottomFound"
tickString SimplifierDone		= "SimplifierDone"

pprTickCts :: Tick -> SDoc
pprTickCts (PreInlineUnconditionally v)	= ppr v
pprTickCts (PostInlineUnconditionally v)= ppr v
pprTickCts (UnfoldingDone v)		= ppr v
pprTickCts (RuleFired v)		= ppr v
pprTickCts LetFloatFromLet		= empty
pprTickCts (EtaExpansion v)		= ppr v
pprTickCts (EtaReduction v)		= ppr v
pprTickCts (BetaReduction v)		= ppr v
pprTickCts (CaseOfCase v)		= ppr v
pprTickCts (KnownBranch v)		= ppr v
pprTickCts (CaseMerge v)		= ppr v
pprTickCts (AltMerge v)			= ppr v
pprTickCts (CaseElim v)			= ppr v
pprTickCts (CaseIdentity v)		= ppr v
pprTickCts (FillInCaseDefault v)	= ppr v
pprTickCts _    			= empty

cmpTick :: Tick -> Tick -> Ordering
cmpTick a b = case (tickToTag a `compare` tickToTag b) of
		GT -> GT
		EQ -> cmpEqTick a b
		LT -> LT

cmpEqTick :: Tick -> Tick -> Ordering
cmpEqTick (PreInlineUnconditionally a)	(PreInlineUnconditionally b)	= a `compare` b
cmpEqTick (PostInlineUnconditionally a)	(PostInlineUnconditionally b)	= a `compare` b
cmpEqTick (UnfoldingDone a)		(UnfoldingDone b)		= a `compare` b
cmpEqTick (RuleFired a)			(RuleFired b)			= a `compare` b
cmpEqTick (EtaExpansion a)		(EtaExpansion b)		= a `compare` b
cmpEqTick (EtaReduction a)		(EtaReduction b)		= a `compare` b
cmpEqTick (BetaReduction a)		(BetaReduction b)		= a `compare` b
cmpEqTick (CaseOfCase a)		(CaseOfCase b)			= a `compare` b
cmpEqTick (KnownBranch a)		(KnownBranch b)			= a `compare` b
cmpEqTick (CaseMerge a)			(CaseMerge b)			= a `compare` b
cmpEqTick (AltMerge a)			(AltMerge b)			= a `compare` b
cmpEqTick (CaseElim a)			(CaseElim b)			= a `compare` b
cmpEqTick (CaseIdentity a)		(CaseIdentity b)		= a `compare` b
cmpEqTick (FillInCaseDefault a)		(FillInCaseDefault b)		= a `compare` b
cmpEqTick _     			_     				= EQ
\end{code}


%************************************************************************
%*									*
             Monad and carried data structure definitions
%*									*
%************************************************************************

\begin{code}
newtype CoreState = CoreState {
        cs_uniq_supply :: UniqSupply
}

data CoreReader = CoreReader {
        cr_hsc_env :: HscEnv,
        cr_rule_base :: RuleBase,
        cr_module :: Module,
        cr_globals :: (,,)    (Bool, [String]) -- from StaticFlags
                              FastStringTable  -- from FastString
#ifdef GHCI
                              (MVar PersistentLinkerState, Bool) -- from Linker
#else
                              ()
#endif
}

data CoreWriter = CoreWriter {
        cw_simpl_count :: !SimplCount  
        -- Making this strict fixes a nasty space leak
        -- See Trac #7702
}

emptyWriter :: DynFlags -> CoreWriter
emptyWriter dflags = CoreWriter {
        cw_simpl_count = zeroSimplCount dflags
    }

plusWriter :: CoreWriter -> CoreWriter -> CoreWriter
plusWriter w1 w2 = CoreWriter {
        cw_simpl_count = (cw_simpl_count w1) `plusSimplCount` (cw_simpl_count w2)
    }

type CoreIOEnv = IOEnv CoreReader

-- | The monad used by Core-to-Core passes to access common state, register simplification
-- statistics and so on
newtype CoreM a = CoreM { unCoreM :: CoreState -> CoreIOEnv (a, CoreState, CoreWriter) }

instance Functor CoreM where
    fmap f ma = do
        a <- ma
        return (f a)

instance Monad CoreM where
    return x = CoreM (\s -> nop s x)
    mx >>= f = CoreM $ \s -> do
            (x, s', w1) <- unCoreM mx s
            (y, s'', w2) <- unCoreM (f x) s'
            let w = w1 `plusWriter` w2 -- forcing w before returning avoids a space leak (Trac #7702)
            return $ seq w (y, s'', w)

instance Applicative CoreM where
    pure = return
    (<*>) = ap

-- For use if the user has imported Control.Monad.Error from MTL
-- Requires UndecidableInstances
instance MonadPlus IO => MonadPlus CoreM where
    mzero = CoreM (const mzero)
    m `mplus` n = CoreM (\rs -> unCoreM m rs `mplus` unCoreM n rs)

instance MonadUnique CoreM where
    getUniqueSupplyM = do
        us <- getS cs_uniq_supply
        let (us1, us2) = splitUniqSupply us
        modifyS (\s -> s { cs_uniq_supply = us2 })
        return us1

    getUniqueM = do
        us <- getS cs_uniq_supply
        let (u,us') = takeUniqFromSupply us
        modifyS (\s -> s { cs_uniq_supply = us' })
        return u

runCoreM :: HscEnv
         -> RuleBase
         -> UniqSupply
         -> Module
         -> CoreM a
         -> IO (a, SimplCount)
runCoreM hsc_env rule_base us mod m = do
        glbls <- liftM3 (,,) saveStaticFlagGlobals saveFSTable saveLinkerGlobals
        liftM extract $ runIOEnv (reader glbls) $ unCoreM m state
  where
    reader glbls = CoreReader {
            cr_hsc_env = hsc_env,
            cr_rule_base = rule_base,
            cr_module = mod,
            cr_globals = glbls
        }
    state = CoreState { 
            cs_uniq_supply = us
        }

    extract :: (a, CoreState, CoreWriter) -> (a, SimplCount)
    extract (value, _, writer) = (value, cw_simpl_count writer)

\end{code}


%************************************************************************
%*									*
             Core combinators, not exported
%*									*
%************************************************************************

\begin{code}

nop :: CoreState -> a -> CoreIOEnv (a, CoreState, CoreWriter)
nop s x = do
    r <- getEnv
    return (x, s, emptyWriter $ (hsc_dflags . cr_hsc_env) r)

read :: (CoreReader -> a) -> CoreM a
read f = CoreM (\s -> getEnv >>= (\r -> nop s (f r)))

getS :: (CoreState -> a) -> CoreM a
getS f = CoreM (\s -> nop s (f s))

modifyS :: (CoreState -> CoreState) -> CoreM ()
modifyS f = CoreM (\s -> nop (f s) ())

write :: CoreWriter -> CoreM ()
write w = CoreM (\s -> return ((), s, w))

\end{code}

\subsection{Lifting IO into the monad}

\begin{code}

-- | Lift an 'IOEnv' operation into 'CoreM'
liftIOEnv :: CoreIOEnv a -> CoreM a
liftIOEnv mx = CoreM (\s -> mx >>= (\x -> nop s x))

instance MonadIO CoreM where
    liftIO = liftIOEnv . IOEnv.liftIO

-- | Lift an 'IO' operation into 'CoreM' while consuming its 'SimplCount'
liftIOWithCount :: IO (SimplCount, a) -> CoreM a
liftIOWithCount what = liftIO what >>= (\(count, x) -> addSimplCount count >> return x)

\end{code}


%************************************************************************
%*									*
             Reader, writer and state accessors
%*									*
%************************************************************************

\begin{code}
getHscEnv :: CoreM HscEnv
getHscEnv = read cr_hsc_env

getRuleBase :: CoreM RuleBase
getRuleBase = read cr_rule_base

addSimplCount :: SimplCount -> CoreM ()
addSimplCount count = write (CoreWriter { cw_simpl_count = count })

-- Convenience accessors for useful fields of HscEnv

instance HasDynFlags CoreM where
    getDynFlags = fmap hsc_dflags getHscEnv

instance HasModule CoreM where
    getModule = read cr_module

-- | The original name cache is the current mapping from 'Module' and
-- 'OccName' to a compiler-wide unique 'Name'
getOrigNameCache :: CoreM OrigNameCache
getOrigNameCache = do
    nameCacheRef <- fmap hsc_NC getHscEnv
    liftIO $ fmap nsNames $ readIORef nameCacheRef
\end{code}

%************************************************************************
%*									*
             Initializing globals
%*									*
%************************************************************************

Note [Initializing globals]

This is a rather annoying function. When a plugin is loaded, it currently
gets linked against a *newly loaded* copy of the GHC package. This would
not be a problem, except that the new copy has its own mutable state
that is not shared with that state that has already been initialized by
the original GHC package.

This leads to loaded plugins calling GHC code which pokes the static flags,
and then dying with a panic because the static flags *it* sees are uninitialized.

There are two possible solutions:
  1. Export the symbols from the GHC executable from the GHC library and link
     against this existing copy rather than a new copy of the GHC library
  2. Carefully ensure that the global state in the two copies of the GHC
     library matches

I tried 1. and it *almost* works (and speeds up plugin load times!) except
on Windows. On Windows the GHC library tends to export more than 65536 symbols
(see #5292) which overflows the limit of what we can export from the EXE and
causes breakage.

(Note that if the GHC exeecutable was dynamically linked this wouldn't be a problem,
because we could share the GHC library it links to.)

We are going to try 2. instead. Unfortunately, this means that every plugin
will have to say `reinitializeGlobals` before it does anything, but never mind.

I've threaded the cr_globals through CoreM rather than giving them as an
argument to the plugin function so that we can turn this function into
(return ()) without breaking any plugins when we eventually get 1. working.

-----

We include the FastString table in this mechanism, because we'd like
FastStrings created by the plugin to have the same uniques as similar strings
created by the host compiler itself.  For example, this allows plugins to
lookup known names (eg `mkTcOcc "MySpecialType"`) in the GlobalRdrEnv or even
re-invoke the parser.

In particular, the following little sanity test was failing in a plugin
prototyping safe newtype-coercions.

   let rdrName = mkModuleName "GHC.NT.Type" `mkRdrQual` mkTcOcc "NT"
   putMsgS $ showSDoc dflags $ ppr $ lookupGRE_RdrName rdrName $ mg_rdr_env guts

`mkTcOcc` involves the lookup (or creation) of a FastString.  Since the
plugin's FastString.string_table is empty, constructing the RdrName also
allocates new uniques for the FastStrings "GHC.NT.Type" and "NT".  These
uniques are almost certainly unequal to the ones that the host compiler
originally assigned to those FastStrings.  Thus the lookup fails since the
domain of the GlobalRdrEnv is affected by the RdrName's OccName's FastString's
unique.

\begin{code}
-- called by plugin
reinitializeGlobals :: CoreM ()
reinitializeGlobals = do
    (sf_globals, fs_table, linker_globals) <- read cr_globals
    hsc_env <- getHscEnv
    let dflags = hsc_dflags hsc_env
    liftIO $ restoreStaticFlagGlobals sf_globals
    liftIO $ restoreFSTable fs_table
    liftIO $ restoreLinkerGlobals linker_globals
    liftIO $ setUnsafeGlobalDynFlags dflags

-- called by host compiler, assuming argument is code from a plugin
bracketGlobals :: CoreM a -> CoreM a
bracketGlobals (CoreM f) = do
  tbl <- liftIO saveFSTable
  let upd e = e {cr_globals=(x,tbl,z)}
        where (x,_,z) = cr_globals e
  x <- CoreM (\s -> updEnv upd (f s))
  liftIO unsaveFSTable
  return x
\end{code}

%************************************************************************
%*									*
             Dealing with annotations
%*									*
%************************************************************************

\begin{code}
-- | Get all annotations of a given type. This happens lazily, that is
-- no deserialization will take place until the [a] is actually demanded and
-- the [a] can also be empty (the UniqFM is not filtered).
--
-- This should be done once at the start of a Core-to-Core pass that uses
-- annotations.
--
-- See Note [Annotations]
getAnnotations :: Typeable a => ([Word8] -> a) -> ModGuts -> CoreM (UniqFM [a])
getAnnotations deserialize guts = do
     hsc_env <- getHscEnv
     ann_env <- liftIO $ prepareAnnotations hsc_env (Just guts)
     return (deserializeAnns deserialize ann_env)

-- | Get at most one annotation of a given type per Unique.
getFirstAnnotations :: Typeable a => ([Word8] -> a) -> ModGuts -> CoreM (UniqFM a)
getFirstAnnotations deserialize guts
  = liftM (mapUFM head . filterUFM (not . null))
  $ getAnnotations deserialize guts
  
\end{code}

Note [Annotations]
~~~~~~~~~~~~~~~~~~
A Core-to-Core pass that wants to make use of annotations calls
getAnnotations or getFirstAnnotations at the beginning to obtain a UniqFM with
annotations of a specific type. This produces all annotations from interface
files read so far. However, annotations from interface files read during the
pass will not be visible until getAnnotations is called again. This is similar
to how rules work and probably isn't too bad.

The current implementation could be optimised a bit: when looking up
annotations for a thing from the HomePackageTable, we could search directly in
the module where the thing is defined rather than building one UniqFM which
contains all annotations we know of. This would work because annotations can
only be given to things defined in the same module. However, since we would
only want to deserialise every annotation once, we would have to build a cache
for every module in the HTP. In the end, it's probably not worth it as long as
we aren't using annotations heavily.

%************************************************************************
%*									*
                Direct screen output
%*									*
%************************************************************************

\begin{code}

msg :: (DynFlags -> SDoc -> IO ()) -> SDoc -> CoreM ()
msg how doc = do
        dflags <- getDynFlags
        liftIO $ how dflags doc

-- | Output a String message to the screen
putMsgS :: String -> CoreM ()
putMsgS = putMsg . text

-- | Output a message to the screen
putMsg :: SDoc -> CoreM ()
putMsg = msg Err.putMsg

-- | Output a string error to the screen
errorMsgS :: String -> CoreM ()
errorMsgS = errorMsg . text

-- | Output an error to the screen
errorMsg :: SDoc -> CoreM ()
errorMsg = msg Err.errorMsg

-- | Output a fatal string error to the screen. Note this does not by itself cause the compiler to die
fatalErrorMsgS :: String -> CoreM ()
fatalErrorMsgS = fatalErrorMsg . text

-- | Output a fatal error to the screen. Note this does not by itself cause the compiler to die
fatalErrorMsg :: SDoc -> CoreM ()
fatalErrorMsg = msg Err.fatalErrorMsg

-- | Output a string debugging message at verbosity level of @-v@ or higher
debugTraceMsgS :: String -> CoreM ()
debugTraceMsgS = debugTraceMsg . text

-- | Outputs a debugging message at verbosity level of @-v@ or higher
debugTraceMsg :: SDoc -> CoreM ()
debugTraceMsg = msg (flip Err.debugTraceMsg 3)

-- | Show some labelled 'SDoc' if a particular flag is set or at a verbosity level of @-v -ddump-most@ or higher
dumpIfSet_dyn :: DumpFlag -> String -> SDoc -> CoreM ()
dumpIfSet_dyn flag str = msg (\dflags -> Err.dumpIfSet_dyn dflags flag str)
\end{code}


%************************************************************************
%*									*
               Finding TyThings
%*									*
%************************************************************************

\begin{code}
instance MonadThings CoreM where
    lookupThing name = do
        hsc_env <- getHscEnv
        liftIO $ initTcForLookup hsc_env (tcLookupGlobal name)
\end{code}

%************************************************************************
%*									*
               Template Haskell interoperability
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI
-- | Attempt to convert a Template Haskell name to one that GHC can
-- understand. Original TH names such as those you get when you use
-- the @'foo@ syntax will be translated to their equivalent GHC name
-- exactly. Qualified or unqualifed TH names will be dynamically bound
-- to names in the module being compiled, if possible. Exact TH names
-- will be bound to the name they represent, exactly.
thNameToGhcName :: TH.Name -> CoreM (Maybe Name)
thNameToGhcName th_name = do
    hsc_env <- getHscEnv
    liftIO $ initTcForLookup hsc_env (lookupThName_maybe th_name)
#endif
\end{code}
