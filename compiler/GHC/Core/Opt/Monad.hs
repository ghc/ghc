{-
(c) The AQUA Project, Glasgow University, 1993-1998

-}


{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Core.Opt.Monad (
    -- * Configuration of the core-to-core passes
    CoreToDo(..), runWhen, runMaybe,
    SimplMode(..),
    FloatOutSwitches(..),
    pprPassDetails,

    -- * Plugins
    CorePluginPass, bindsOnlyPass,

    -- * Counting
    SimplCount, doSimplTick, doFreeSimplTick, simplCountN,
    pprSimplCount, plusSimplCount, zeroSimplCount,
    isZeroSimplCount, hasDetailedCounts, Tick(..),

    -- * The monad
    CoreM, runCoreM,

    -- ** Reading from the monad
    getHscEnv, getRuleBase, getModule,
    getDynFlags, getPackageFamInstEnv,
    getVisibleOrphanMods, getUniqMask,
    getPrintUnqualified, getSrcSpanM,

    -- ** Writing to the monad
    addSimplCount,

    -- ** Lifting into the monad
    liftIO, liftIOWithCount,

    -- ** Dealing with annotations
    getAnnotations, getFirstAnnotations,

    -- ** Screen output
    putMsg, putMsgS, errorMsg, errorMsgS, msg,
    fatalErrorMsg, fatalErrorMsgS,
    debugTraceMsg, debugTraceMsgS,
  ) where

import GHC.Prelude hiding ( read )

import GHC.Driver.Session
import GHC.Driver.Env

import GHC.Core
import GHC.Core.Unfold

import GHC.Types.Basic  ( CompilerPhase(..) )
import GHC.Types.Annotations
import GHC.Types.Var
import GHC.Types.Unique.Supply
import GHC.Types.Name.Env
import GHC.Types.SrcLoc
import GHC.Types.Error

import GHC.Utils.Error ( errorDiagnostic )
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Logger
import GHC.Utils.Monad

import GHC.Data.FastString
import GHC.Data.IOEnv hiding     ( liftIO, failM, failWithM )
import qualified GHC.Data.IOEnv  as IOEnv

import GHC.Unit.Module
import GHC.Unit.Module.ModGuts
import GHC.Unit.External

import Data.Bifunctor ( bimap )
import Data.List (intersperse, groupBy, sortBy)
import Data.Ord
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapStrict
import Data.Word
import Control.Monad
import Control.Applicative ( Alternative(..) )
import GHC.Utils.Panic (throwGhcException, GhcException(..), panic)

{-
************************************************************************
*                                                                      *
              The CoreToDo type and related types
          Abstraction of core-to-core passes to run.
*                                                                      *
************************************************************************
-}

data CoreToDo           -- These are diff core-to-core passes,
                        -- which may be invoked in any order,
                        -- as many times as you like.

  = CoreDoSimplify      -- The core-to-core simplifier.
        Int                    -- Max iterations
        SimplMode
  | CoreDoPluginPass String CorePluginPass
  | CoreDoFloatInwards
  | CoreDoFloatOutwards FloatOutSwitches
  | CoreLiberateCase
  | CoreDoPrintCore
  | CoreDoStaticArgs
  | CoreDoCallArity
  | CoreDoExitify
  | CoreDoDemand
  | CoreDoCpr
  | CoreDoWorkerWrapper
  | CoreDoSpecialising
  | CoreDoSpecConstr
  | CoreCSE
  | CoreDoRuleCheck CompilerPhase String   -- Check for non-application of rules
                                           -- matching this string
  | CoreDoNothing                -- Useful when building up
  | CoreDoPasses [CoreToDo]      -- lists of these things

  | CoreDesugar    -- Right after desugaring, no simple optimisation yet!
  | CoreDesugarOpt -- CoreDesugarXXX: Not strictly a core-to-core pass, but produces
                       --                 Core output, and hence useful to pass to endPass

  | CoreTidy
  | CorePrep
  | CoreAddCallerCcs
  | CoreOccurAnal
  | CoreFreshenUniques

instance Outputable CoreToDo where
  ppr (CoreDoSimplify _ _)     = text "Simplifier"
  ppr (CoreDoPluginPass s _)   = text "Core plugin: " <+> text s
  ppr CoreDoFloatInwards       = text "Float inwards"
  ppr (CoreDoFloatOutwards f)  = text "Float out" <> parens (ppr f)
  ppr CoreLiberateCase         = text "Liberate case"
  ppr CoreDoStaticArgs         = text "Static argument"
  ppr CoreDoCallArity          = text "Called arity analysis"
  ppr CoreDoExitify            = text "Exitification transformation"
  ppr CoreDoDemand             = text "Demand analysis"
  ppr CoreDoCpr                = text "Constructed Product Result analysis"
  ppr CoreDoWorkerWrapper      = text "Worker Wrapper binds"
  ppr CoreDoSpecialising       = text "Specialise"
  ppr CoreDoSpecConstr         = text "SpecConstr"
  ppr CoreCSE                  = text "Common sub-expression"
  ppr CoreDesugar              = text "Desugar (before optimization)"
  ppr CoreDesugarOpt           = text "Desugar (after optimization)"
  ppr CoreTidy                 = text "Tidy Core"
  ppr CoreAddCallerCcs         = text "Add caller cost-centres"
  ppr CorePrep                 = text "CorePrep"
  ppr CoreOccurAnal            = text "Occurrence analysis"
  ppr CoreDoPrintCore          = text "Print core"
  ppr (CoreDoRuleCheck {})     = text "Rule check"
  ppr CoreDoNothing            = text "CoreDoNothing"
  ppr (CoreDoPasses passes)    = text "CoreDoPasses" <+> ppr passes
  ppr CoreFreshenUniques       = text "CoreFreshenUniques"

pprPassDetails :: CoreToDo -> SDoc
pprPassDetails (CoreDoSimplify n md) = vcat [ text "Max iterations =" <+> int n
                                            , ppr md ]
pprPassDetails _ = Outputable.empty

data SimplMode             -- See comments in GHC.Core.Opt.Simplify.Monad
  = SimplMode
        { sm_names      :: [String]       -- ^ Name(s) of the phase
        , sm_phase      :: CompilerPhase
        , sm_uf_opts    :: !UnfoldingOpts -- ^ Unfolding options
        , sm_rules      :: !Bool          -- ^ Whether RULES are enabled
        , sm_inline     :: !Bool          -- ^ Whether inlining is enabled
        , sm_case_case  :: !Bool          -- ^ Whether case-of-case is enabled
        , sm_eta_expand :: !Bool          -- ^ Whether eta-expansion is enabled
        , sm_pre_inline :: !Bool          -- ^ Whether pre-inlining is enabled
        , sm_logger     :: !Logger
        , sm_dflags     :: DynFlags
            -- Just for convenient non-monadic access; we don't override these.
            --
            -- Used for:
            --    - target platform (for `exprIsDupable` and `mkDupableAlt`)
            --    - Opt_DictsCheap and Opt_PedanticBottoms general flags
            --    - rules options (initRuleOpts)
            --    - inlineCheck
        }

instance Outputable SimplMode where
    ppr (SimplMode { sm_phase = p, sm_names = ss
                   , sm_rules = r, sm_inline = i
                   , sm_eta_expand = eta, sm_case_case = cc })
       = text "SimplMode" <+> braces (
         sep [ text "Phase =" <+> ppr p <+>
               brackets (text (concat $ intersperse "," ss)) <> comma
             , pp_flag i   (text "inline") <> comma
             , pp_flag r   (text "rules") <> comma
             , pp_flag eta (text "eta-expand") <> comma
             , pp_flag cc  (text "case-of-case") ])
         where
           pp_flag f s = ppUnless f (text "no") <+> s

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
  floatOutOverSatApps :: Bool,
                             -- ^ True <=> float out over-saturated applications
                             --            based on arity information.
                             -- See Note [Floating over-saturated applications]
                             -- in GHC.Core.Opt.SetLevels
  floatToTopLevelOnly :: Bool      -- ^ Allow floating to the top level only.
  }
instance Outputable FloatOutSwitches where
    ppr = pprFloatOutSwitches

pprFloatOutSwitches :: FloatOutSwitches -> SDoc
pprFloatOutSwitches sw
  = text "FOS" <+> (braces $
     sep $ punctuate comma $
     [ text "Lam ="    <+> ppr (floatOutLambdas sw)
     , text "Consts =" <+> ppr (floatOutConstants sw)
     , text "OverSatApps ="   <+> ppr (floatOutOverSatApps sw) ])

-- The core-to-core pass ordering is derived from the DynFlags:
runWhen :: Bool -> CoreToDo -> CoreToDo
runWhen True  do_this = do_this
runWhen False _       = CoreDoNothing

runMaybe :: Maybe a -> (a -> CoreToDo) -> CoreToDo
runMaybe (Just x) f = f x
runMaybe Nothing  _ = CoreDoNothing

{-

************************************************************************
*                                                                      *
             Types for Plugins
*                                                                      *
************************************************************************
-}

-- | A description of the plugin pass itself
type CorePluginPass = ModGuts -> CoreM ModGuts

bindsOnlyPass :: (CoreProgram -> CoreM CoreProgram) -> ModGuts -> CoreM ModGuts
bindsOnlyPass pass guts
  = do { binds' <- pass (mg_binds guts)
       ; return (guts { mg_binds = binds' }) }

{-
************************************************************************
*                                                                      *
             Counting and logging
*                                                                      *
************************************************************************
-}

getVerboseSimplStats :: (Bool -> SDoc) -> SDoc
getVerboseSimplStats = getPprDebug          -- For now, anyway

zeroSimplCount     :: DynFlags -> SimplCount
isZeroSimplCount   :: SimplCount -> Bool
hasDetailedCounts  :: SimplCount -> Bool
pprSimplCount      :: SimplCount -> SDoc
doSimplTick        :: DynFlags -> Tick -> SimplCount -> SimplCount
doFreeSimplTick    ::             Tick -> SimplCount -> SimplCount
plusSimplCount     :: SimplCount -> SimplCount -> SimplCount

data SimplCount
   = VerySimplCount !Int        -- Used when don't want detailed stats

   | SimplCount {
        ticks   :: !Int,        -- Total ticks
        details :: !TickCounts, -- How many of each type

        n_log   :: !Int,        -- N
        log1    :: [Tick],      -- Last N events; <= opt_HistorySize,
                                --   most recent first
        log2    :: [Tick]       -- Last opt_HistorySize events before that
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

isZeroSimplCount (VerySimplCount n)         = n==0
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


addTick :: TickCounts -> Tick -> TickCounts
addTick fm tick = MapStrict.insertWith (+) tick 1 fm

plusSimplCount sc1@(SimplCount { ticks = tks1, details = dts1 })
               sc2@(SimplCount { ticks = tks2, details = dts2 })
  = log_base { ticks = tks1 + tks2
             , details = MapStrict.unionWith (+) dts1 dts2 }
  where
        -- A hackish way of getting recent log info
    log_base | null (log1 sc2) = sc1    -- Nothing at all in sc2
             | null (log2 sc2) = sc2 { log2 = log1 sc1 }
             | otherwise       = sc2

plusSimplCount (VerySimplCount n) (VerySimplCount m) = VerySimplCount (n+m)
plusSimplCount lhs                rhs                =
  throwGhcException . PprProgramError "plusSimplCount" $ vcat
    [ text "lhs"
    , pprSimplCount lhs
    , text "rhs"
    , pprSimplCount rhs
    ]
       -- We use one or the other consistently

pprSimplCount (VerySimplCount n) = text "Total ticks:" <+> int n
pprSimplCount (SimplCount { ticks = tks, details = dts, log1 = l1, log2 = l2 })
  = vcat [text "Total ticks:    " <+> int tks,
          blankLine,
          pprTickCounts dts,
          getVerboseSimplStats $ \dbg -> if dbg
          then
                vcat [blankLine,
                      text "Log (most recent first)",
                      nest 4 (vcat (map ppr l1) $$ vcat (map ppr l2))]
          else Outputable.empty
    ]

{- Note [Which transformations are innocuous]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At one point (Jun 18) I wondered if some transformations (ticks)
might be  "innocuous", in the sense that they do not unlock a later
transformation that does not occur in the same pass.  If so, we could
refrain from bumping the overall tick-count for such innocuous
transformations, and perhaps terminate the simplifier one pass
earlier.

But alas I found that virtually nothing was innocuous!  This Note
just records what I learned, in case anyone wants to try again.

These transformations are not innocuous:

*** NB: I think these ones could be made innocuous
          EtaExpansion
          LetFloatFromLet

LetFloatFromLet
    x = K (let z = e2 in Just z)
  prepareRhs transforms to
    x2 = let z=e2 in Just z
    x  = K xs
  And now more let-floating can happen in the
  next pass, on x2

PreInlineUnconditionally
  Example in spectral/cichelli/Auxil
     hinsert = ...let lo = e in
                  let j = ...lo... in
                  case x of
                    False -> ()
                    True -> case lo of I# lo' ->
                              ...j...
  When we PreInlineUnconditionally j, lo's occ-info changes to once,
  so it can be PreInlineUnconditionally in the next pass, and a
  cascade of further things can happen.

PostInlineUnconditionally
  let x = e in
  let y = ...x.. in
  case .. of { A -> ...x...y...
               B -> ...x...y... }
  Current postinlineUnconditinaly will inline y, and then x; sigh.

  But PostInlineUnconditionally might also unlock subsequent
  transformations for the same reason as PreInlineUnconditionally,
  so it's probably not innocuous anyway.

KnownBranch, BetaReduction:
  May drop chunks of code, and thereby enable PreInlineUnconditionally
  for some let-binding which now occurs once

EtaExpansion:
  Example in imaginary/digits-of-e1
    fail = \void. e          where e :: IO ()
  --> etaExpandRhs
    fail = \void. (\s. (e |> g) s) |> sym g      where g :: IO () ~ S -> (S,())
  --> Next iteration of simplify
    fail1 = \void. \s. (e |> g) s
    fail = fail1 |> Void# -> sym g
  And now inline 'fail'

CaseMerge:
  case x of y {
    DEFAULT -> case y of z { pi -> ei }
    alts2 }
  ---> CaseMerge
    case x of { pi -> let z = y in ei
              ; alts2 }
  The "let z=y" case-binder-swap gets dealt with in the next pass
-}

pprTickCounts :: Map Tick Int -> SDoc
pprTickCounts counts
  = vcat (map pprTickGroup groups)
  where
    groups :: [[(Tick,Int)]]    -- Each group shares a common tag
                                -- toList returns common tags adjacent
    groups = groupBy same_tag (Map.toList counts)
    same_tag (tick1,_) (tick2,_) = tickToTag tick1 == tickToTag tick2

pprTickGroup :: [(Tick, Int)] -> SDoc
pprTickGroup group@((tick1,_):_)
  = hang (int (sum [n | (_,n) <- group]) <+> text (tickString tick1))
       2 (vcat [ int n <+> pprTickCts tick
                                    -- flip as we want largest first
               | (tick,n) <- sortBy (flip (comparing snd)) group])
pprTickGroup [] = panic "pprTickGroup"

data Tick  -- See Note [Which transformations are innocuous]
  = PreInlineUnconditionally    Id
  | PostInlineUnconditionally   Id

  | UnfoldingDone               Id
  | RuleFired                   FastString      -- Rule name

  | LetFloatFromLet
  | EtaExpansion                Id      -- LHS binder
  | EtaReduction                Id      -- Binder on outer lambda
  | BetaReduction               Id      -- Lambda binder


  | CaseOfCase                  Id      -- Bndr on *inner* case
  | KnownBranch                 Id      -- Case binder
  | CaseMerge                   Id      -- Binder on outer case
  | AltMerge                    Id      -- Case binder
  | CaseElim                    Id      -- Case binder
  | CaseIdentity                Id      -- Case binder
  | FillInCaseDefault           Id      -- Case binder

  | SimplifierDone              -- Ticked at each iteration of the simplifier

instance Outputable Tick where
  ppr tick = text (tickString tick) <+> pprTickCts tick

instance Eq Tick where
  a == b = case a `cmpTick` b of
           EQ -> True
           _ -> False

instance Ord Tick where
  compare = cmpTick

tickToTag :: Tick -> Int
tickToTag (PreInlineUnconditionally _)  = 0
tickToTag (PostInlineUnconditionally _) = 1
tickToTag (UnfoldingDone _)             = 2
tickToTag (RuleFired _)                 = 3
tickToTag LetFloatFromLet               = 4
tickToTag (EtaExpansion _)              = 5
tickToTag (EtaReduction _)              = 6
tickToTag (BetaReduction _)             = 7
tickToTag (CaseOfCase _)                = 8
tickToTag (KnownBranch _)               = 9
tickToTag (CaseMerge _)                 = 10
tickToTag (CaseElim _)                  = 11
tickToTag (CaseIdentity _)              = 12
tickToTag (FillInCaseDefault _)         = 13
tickToTag SimplifierDone                = 16
tickToTag (AltMerge _)                  = 17

tickString :: Tick -> String
tickString (PreInlineUnconditionally _) = "PreInlineUnconditionally"
tickString (PostInlineUnconditionally _)= "PostInlineUnconditionally"
tickString (UnfoldingDone _)            = "UnfoldingDone"
tickString (RuleFired _)                = "RuleFired"
tickString LetFloatFromLet              = "LetFloatFromLet"
tickString (EtaExpansion _)             = "EtaExpansion"
tickString (EtaReduction _)             = "EtaReduction"
tickString (BetaReduction _)            = "BetaReduction"
tickString (CaseOfCase _)               = "CaseOfCase"
tickString (KnownBranch _)              = "KnownBranch"
tickString (CaseMerge _)                = "CaseMerge"
tickString (AltMerge _)                 = "AltMerge"
tickString (CaseElim _)                 = "CaseElim"
tickString (CaseIdentity _)             = "CaseIdentity"
tickString (FillInCaseDefault _)        = "FillInCaseDefault"
tickString SimplifierDone               = "SimplifierDone"

pprTickCts :: Tick -> SDoc
pprTickCts (PreInlineUnconditionally v) = ppr v
pprTickCts (PostInlineUnconditionally v)= ppr v
pprTickCts (UnfoldingDone v)            = ppr v
pprTickCts (RuleFired v)                = ppr v
pprTickCts LetFloatFromLet              = Outputable.empty
pprTickCts (EtaExpansion v)             = ppr v
pprTickCts (EtaReduction v)             = ppr v
pprTickCts (BetaReduction v)            = ppr v
pprTickCts (CaseOfCase v)               = ppr v
pprTickCts (KnownBranch v)              = ppr v
pprTickCts (CaseMerge v)                = ppr v
pprTickCts (AltMerge v)                 = ppr v
pprTickCts (CaseElim v)                 = ppr v
pprTickCts (CaseIdentity v)             = ppr v
pprTickCts (FillInCaseDefault v)        = ppr v
pprTickCts _                            = Outputable.empty

cmpTick :: Tick -> Tick -> Ordering
cmpTick a b = case (tickToTag a `compare` tickToTag b) of
                GT -> GT
                EQ -> cmpEqTick a b
                LT -> LT

cmpEqTick :: Tick -> Tick -> Ordering
cmpEqTick (PreInlineUnconditionally a)  (PreInlineUnconditionally b)    = a `compare` b
cmpEqTick (PostInlineUnconditionally a) (PostInlineUnconditionally b)   = a `compare` b
cmpEqTick (UnfoldingDone a)             (UnfoldingDone b)               = a `compare` b
cmpEqTick (RuleFired a)                 (RuleFired b)                   = a `uniqCompareFS` b
cmpEqTick (EtaExpansion a)              (EtaExpansion b)                = a `compare` b
cmpEqTick (EtaReduction a)              (EtaReduction b)                = a `compare` b
cmpEqTick (BetaReduction a)             (BetaReduction b)               = a `compare` b
cmpEqTick (CaseOfCase a)                (CaseOfCase b)                  = a `compare` b
cmpEqTick (KnownBranch a)               (KnownBranch b)                 = a `compare` b
cmpEqTick (CaseMerge a)                 (CaseMerge b)                   = a `compare` b
cmpEqTick (AltMerge a)                  (AltMerge b)                    = a `compare` b
cmpEqTick (CaseElim a)                  (CaseElim b)                    = a `compare` b
cmpEqTick (CaseIdentity a)              (CaseIdentity b)                = a `compare` b
cmpEqTick (FillInCaseDefault a)         (FillInCaseDefault b)           = a `compare` b
cmpEqTick _                             _                               = EQ

{-
************************************************************************
*                                                                      *
             Monad and carried data structure definitions
*                                                                      *
************************************************************************
-}

data CoreReader = CoreReader {
        cr_hsc_env             :: HscEnv,
        cr_rule_base           :: RuleBase,
        cr_module              :: Module,
        cr_print_unqual        :: PrintUnqualified,
        cr_loc                 :: SrcSpan,   -- Use this for log/error messages so they
                                             -- are at least tagged with the right source file
        cr_visible_orphan_mods :: !ModuleSet,
        cr_uniq_mask           :: !Char      -- Mask for creating unique values
}

-- Note: CoreWriter used to be defined with data, rather than newtype.  If it
-- is defined that way again, the cw_simpl_count field, at least, must be
-- strict to avoid a space leak (#7702).
newtype CoreWriter = CoreWriter {
        cw_simpl_count :: SimplCount
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

-- | The monad used by Core-to-Core passes to register simplification statistics.
--  Also used to have common state (in the form of UniqueSupply) for generating Uniques.
newtype CoreM a = CoreM { unCoreM :: CoreIOEnv (a, CoreWriter) }
    deriving (Functor)

instance Monad CoreM where
    mx >>= f = CoreM $ do
            (x, w1) <- unCoreM mx
            (y, w2) <- unCoreM (f x)
            let w = w1 `plusWriter` w2
            return $ seq w (y, w)
            -- forcing w before building the tuple avoids a space leak
            -- (#7702)

instance Applicative CoreM where
    pure x = CoreM $ nop x
    (<*>) = ap
    m *> k = m >>= \_ -> k

instance Alternative CoreM where
    empty   = CoreM Control.Applicative.empty
    m <|> n = CoreM (unCoreM m <|> unCoreM n)

instance MonadPlus CoreM

instance MonadUnique CoreM where
    getUniqueSupplyM = do
        mask <- read cr_uniq_mask
        liftIO $! mkSplitUniqSupply mask

    getUniqueM = do
        mask <- read cr_uniq_mask
        liftIO $! uniqFromMask mask

runCoreM :: HscEnv
         -> RuleBase
         -> Char -- ^ Mask
         -> Module
         -> ModuleSet
         -> PrintUnqualified
         -> SrcSpan
         -> CoreM a
         -> IO (a, SimplCount)
runCoreM hsc_env rule_base mask mod orph_imps print_unqual loc m
  = liftM extract $ runIOEnv reader $ unCoreM m
  where
    reader = CoreReader {
            cr_hsc_env = hsc_env,
            cr_rule_base = rule_base,
            cr_module = mod,
            cr_visible_orphan_mods = orph_imps,
            cr_print_unqual = print_unqual,
            cr_loc = loc,
            cr_uniq_mask = mask
        }

    extract :: (a, CoreWriter) -> (a, SimplCount)
    extract (value, writer) = (value, cw_simpl_count writer)

{-
************************************************************************
*                                                                      *
             Core combinators, not exported
*                                                                      *
************************************************************************
-}

nop :: a -> CoreIOEnv (a, CoreWriter)
nop x = do
    r <- getEnv
    return (x, emptyWriter $ (hsc_dflags . cr_hsc_env) r)

read :: (CoreReader -> a) -> CoreM a
read f = CoreM $ getEnv >>= (\r -> nop (f r))

write :: CoreWriter -> CoreM ()
write w = CoreM $ return ((), w)

-- \subsection{Lifting IO into the monad}

-- | Lift an 'IOEnv' operation into 'CoreM'
liftIOEnv :: CoreIOEnv a -> CoreM a
liftIOEnv mx = CoreM (mx >>= (\x -> nop x))

instance MonadIO CoreM where
    liftIO = liftIOEnv . IOEnv.liftIO

-- | Lift an 'IO' operation into 'CoreM' while consuming its 'SimplCount'
liftIOWithCount :: IO (SimplCount, a) -> CoreM a
liftIOWithCount what = liftIO what >>= (\(count, x) -> addSimplCount count >> return x)

{-
************************************************************************
*                                                                      *
             Reader, writer and state accessors
*                                                                      *
************************************************************************
-}

getHscEnv :: CoreM HscEnv
getHscEnv = read cr_hsc_env

getRuleBase :: CoreM RuleBase
getRuleBase = read cr_rule_base

getVisibleOrphanMods :: CoreM ModuleSet
getVisibleOrphanMods = read cr_visible_orphan_mods

getPrintUnqualified :: CoreM PrintUnqualified
getPrintUnqualified = read cr_print_unqual

getSrcSpanM :: CoreM SrcSpan
getSrcSpanM = read cr_loc

addSimplCount :: SimplCount -> CoreM ()
addSimplCount count = write (CoreWriter { cw_simpl_count = count })

getUniqMask :: CoreM Char
getUniqMask = read cr_uniq_mask

-- Convenience accessors for useful fields of HscEnv

instance HasDynFlags CoreM where
    getDynFlags = fmap hsc_dflags getHscEnv

instance HasLogger CoreM where
    getLogger = fmap hsc_logger getHscEnv

instance HasModule CoreM where
    getModule = read cr_module

getPackageFamInstEnv :: CoreM PackageFamInstEnv
getPackageFamInstEnv = do
    hsc_env <- getHscEnv
    eps <- liftIO $ hscEPS hsc_env
    return $ eps_fam_inst_env eps

{-
************************************************************************
*                                                                      *
             Dealing with annotations
*                                                                      *
************************************************************************
-}

-- | Get all annotations of a given type. This happens lazily, that is
-- no deserialization will take place until the [a] is actually demanded and
-- the [a] can also be empty (the UniqFM is not filtered).
--
-- This should be done once at the start of a Core-to-Core pass that uses
-- annotations.
--
-- See Note [Annotations]
getAnnotations :: Typeable a => ([Word8] -> a) -> ModGuts -> CoreM (ModuleEnv [a], NameEnv [a])
getAnnotations deserialize guts = do
     hsc_env <- getHscEnv
     ann_env <- liftIO $ prepareAnnotations hsc_env (Just guts)
     return (deserializeAnns deserialize ann_env)

-- | Get at most one annotation of a given type per annotatable item.
getFirstAnnotations :: Typeable a => ([Word8] -> a) -> ModGuts -> CoreM (ModuleEnv a, NameEnv a)
getFirstAnnotations deserialize guts
  = bimap mod name <$> getAnnotations deserialize guts
  where
    mod = mapModuleEnv head . filterModuleEnv (const $ not . null)
    name = mapNameEnv head . filterNameEnv (not . null)

{-
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

************************************************************************
*                                                                      *
                Direct screen output
*                                                                      *
************************************************************************
-}

msg :: MessageClass -> SDoc -> CoreM ()
msg msg_class doc = do
    logger <- getLogger
    loc    <- getSrcSpanM
    unqual <- getPrintUnqualified
    let sty = case msg_class of
                MCDiagnostic _ _ -> err_sty
                MCDump           -> dump_sty
                _                -> user_sty
        err_sty  = mkErrStyle unqual
        user_sty = mkUserStyle unqual AllTheWay
        dump_sty = mkDumpStyle unqual
    liftIO $ logMsg logger msg_class loc (withPprStyle sty doc)

-- | Output a String message to the screen
putMsgS :: String -> CoreM ()
putMsgS = putMsg . text

-- | Output a message to the screen
putMsg :: SDoc -> CoreM ()
putMsg = msg MCInfo

-- | Output an error to the screen. Does not cause the compiler to die.
errorMsgS :: String -> CoreM ()
errorMsgS = errorMsg . text

-- | Output an error to the screen. Does not cause the compiler to die.
errorMsg :: SDoc -> CoreM ()
errorMsg doc = msg errorDiagnostic doc

-- | Output a fatal error to the screen. Does not cause the compiler to die.
fatalErrorMsgS :: String -> CoreM ()
fatalErrorMsgS = fatalErrorMsg . text

-- | Output a fatal error to the screen. Does not cause the compiler to die.
fatalErrorMsg :: SDoc -> CoreM ()
fatalErrorMsg = msg MCFatal

-- | Output a string debugging message at verbosity level of @-v@ or higher
debugTraceMsgS :: String -> CoreM ()
debugTraceMsgS = debugTraceMsg . text

-- | Outputs a debugging message at verbosity level of @-v@ or higher
debugTraceMsg :: SDoc -> CoreM ()
debugTraceMsg = msg MCDump
