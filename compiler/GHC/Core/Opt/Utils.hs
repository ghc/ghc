{-
(c) The AQUA Project, Glasgow University, 1993-1998

-}


{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Core.Opt.Utils (
    -- * Configuration of the core-to-core passes
    SimplMode(..),
    FloatOutSwitches(..),
    FloatEnable(..),
    floatEnable,

    -- * Counting
    SimplCount, doSimplTick, doFreeSimplTick, simplCountN,
    pprSimplCount, plusSimplCount, zeroSimplCount,
    isZeroSimplCount, hasDetailedCounts, Tick(..),

    -- ** Dealing with annotations
    getAnnotationsFromHscEnv, getFirstAnnotationsFromHscEnv,
  ) where

import GHC.Prelude hiding ( read )

import GHC.Driver.Session
import GHC.Driver.Env

import GHC.Core.Unfold

import GHC.Types.Basic  ( CompilerPhase(..) )
import GHC.Types.Annotations
import GHC.Types.Var
import GHC.Types.Name.Env
import GHC.Types.Error

import GHC.Utils.Outputable as Outputable
import GHC.Utils.Logger

import GHC.Data.FastString

import GHC.Unit.Module
import GHC.Unit.Module.ModGuts

import Data.Bifunctor ( bimap )
import Data.List (intersperse, groupBy, sortBy)
import Data.Ord
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapStrict
import Data.Word
import GHC.Utils.Panic (throwGhcException, GhcException(..), panic)

data FloatEnable  -- Controls local let-floating
  = FloatDisabled      -- Do no local let-floating
  | FloatNestedOnly    -- Local let-floating for nested (NotTopLevel) bindings only
  | FloatEnabled       -- Do local let-floating on all bindings

floatEnable :: DynFlags -> FloatEnable
floatEnable dflags =
  case (gopt Opt_LocalFloatOut dflags, gopt Opt_LocalFloatOutTopLevel dflags) of
    (True, True) -> FloatEnabled
    (True, False)-> FloatNestedOnly
    (False, _)   -> FloatDisabled

{-
Note [Local floating]
~~~~~~~~~~~~~~~~~~~~~
The Simplifier can perform local let-floating: it floats let-bindings
out of the RHS of let-bindings.  See
  Let-floating: moving bindings to give faster programs (ICFP'96)
  https://www.microsoft.com/en-us/research/publication/let-floating-moving-bindings-to-give-faster-programs/

Here's an example
   x = let y = v+1 in (y,true)

The RHS of x is a thunk.  Much better to float that y-binding out to give
   y = v+1
   x = (y,true)

Not only have we avoided building a thunk, but any (case x of (p,q) -> ...) in
the scope of the x-binding can now be simplified.

This local let-floating is done in GHC.Core.Opt.Simplify.prepareBinding,
controlled by the predicate GHC.Core.Opt.Simplify.Env.doFloatFromRhs.

The `FloatEnable` data type controls where local let-floating takes place;
it allows you to specify that it should be done only for nested bindings;
or for top-level bindings as well; or not at all.

Note that all of this is quite separate from the global FloatOut pass;
see GHC.Core.Opt.FloatOut.

-}

data SimplMode             -- See comments in GHC.Core.Opt.Simplify.Monad
  = SimplMode
        { sm_names        :: [String]       -- ^ Name(s) of the phase
        , sm_phase        :: CompilerPhase
        , sm_uf_opts      :: !UnfoldingOpts -- ^ Unfolding options
        , sm_rules        :: !Bool          -- ^ Whether RULES are enabled
        , sm_inline       :: !Bool          -- ^ Whether inlining is enabled
        , sm_case_case    :: !Bool          -- ^ Whether case-of-case is enabled
        , sm_eta_expand   :: !Bool          -- ^ Whether eta-expansion is enabled
        , sm_cast_swizzle :: !Bool          -- ^ Do we swizzle casts past lambdas?
        , sm_pre_inline   :: !Bool          -- ^ Whether pre-inlining is enabled
        , sm_float_enable :: !FloatEnable   -- ^ Whether to enable floating out
        , sm_logger       :: !Logger
        , sm_dflags       :: DynFlags
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
                   , sm_cast_swizzle = cs
                   , sm_eta_expand = eta, sm_case_case = cc })
       = text "SimplMode" <+> braces (
         sep [ text "Phase =" <+> ppr p <+>
               brackets (text (concat $ intersperse "," ss)) <> comma
             , pp_flag i   (text "inline") <> comma
             , pp_flag r   (text "rules") <> comma
             , pp_flag eta (text "eta-expand") <> comma
             , pp_flag cs (text "cast-swizzle") <> comma
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
getAnnotationsFromHscEnv :: Typeable a => HscEnv -> ([Word8] -> a) -> ModGuts -> IO (ModuleEnv [a], NameEnv [a])
getAnnotationsFromHscEnv hsc_env deserialize guts = do
   ann_env <- prepareAnnotations hsc_env (Just guts)
   return (deserializeAnns deserialize ann_env)

-- | Get at most one annotation of a given type per annotatable item.
getFirstAnnotationsFromHscEnv :: Typeable a => HscEnv -> ([Word8] -> a) -> ModGuts -> IO (ModuleEnv a, NameEnv a)
getFirstAnnotationsFromHscEnv hsc_env deserialize guts
  = bimap mod name <$> getAnnotationsFromHscEnv hsc_env deserialize guts
  where
    mod = mapModuleEnv head . filterModuleEnv (const $ not . null)
    name = mapNameEnv head . filterNameEnv (not . null)

{-
Note [Annotations]
~~~~~~~~~~~~~~~~~~
A Core-to-Core pass that wants to make use of annotations calls
getAnnotationsFromHscEnv or getFirstAnnotationsFromHscEnv at the beginning to
obtain a UniqFM with annotations of a specific type. This produces all
annotations from interface files read so far. However, annotations from
interface files read during the pass will not be visible until
getAnnotationsFromHscEnv is called again. This is similar to how rules work and
probably isn't too bad.

The current implementation could be optimised a bit: when looking up
annotations for a thing from the HomePackageTable, we could search directly in
the module where the thing is defined rather than building one UniqFM which
contains all annotations we know of. This would work because annotations can
only be given to things defined in the same module. However, since we would
only want to deserialise every annotation once, we would have to build a cache
for every module in the HTP. In the end, it's probably not worth it as long as
we aren't using annotations heavily.
-}
