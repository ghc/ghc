{-
(c) The AQUA Project, Glasgow University, 1993-1998

-}

{-# LANGUAGE DerivingVia #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Core.Opt.Stats (
    SimplCount, doSimplTick, doFreeSimplTick, simplCountN,
    pprSimplCount, plusSimplCount, zeroSimplCount,
    isZeroSimplCount, hasDetailedCounts, Tick(..)
  ) where

import GHC.Prelude

import GHC.Types.Var
import GHC.Types.Error

import GHC.Utils.Outputable as Outputable

import GHC.Data.FastString

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapStrict
import GHC.Utils.Panic (throwGhcException, GhcException(..))

getVerboseSimplStats :: (Bool -> SDoc) -> SDoc
getVerboseSimplStats = getPprDebug          -- For now, anyway

zeroSimplCount     :: Bool -- ^ -ddump-simpl-stats
                   -> SimplCount
isZeroSimplCount   :: SimplCount -> Bool
hasDetailedCounts  :: SimplCount -> Bool
pprSimplCount      :: SimplCount -> SDoc
doSimplTick        :: Int -- ^ History size of the elaborate counter
                   -> Tick -> SimplCount -> SimplCount
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

zeroSimplCount dump_simpl_stats
                -- This is where we decide whether to do
                -- the VerySimpl version or the full-stats version
  | dump_simpl_stats
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

doSimplTick history_size tick
    sc@(SimplCount { ticks = tks, details = dts, n_log = nl, log1 = l1 })
  | nl >= history_size = sc1 { n_log = 1, log1 = [tick], log2 = l1 }
  | otherwise          = sc1 { n_log = nl+1, log1 = tick : l1 }
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
    groups :: [NonEmpty (Tick, Int)] -- Each group shares a common tag
                                     -- toList returns common tags adjacent
    groups = NE.groupWith (tickToTag . fst) (Map.toList counts)

pprTickGroup :: NonEmpty (Tick, Int) -> SDoc
pprTickGroup group@((tick1,_) :| _)
  = hang (int (sum (fmap snd group)) <+> pprTickType tick1)
       2 (vcat [ int n <+> pprTickCts tick
                                    -- flip as we want largest first
               | (tick,n) <- sortOn (Down . snd) (NE.toList group)])

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
  ppr tick = pprTickType tick <+> pprTickCts tick

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

pprTickType :: Tick -> SDoc
pprTickType (PreInlineUnconditionally _) = text "PreInlineUnconditionally"
pprTickType (PostInlineUnconditionally _)= text "PostInlineUnconditionally"
pprTickType (UnfoldingDone _)            = text "UnfoldingDone"
pprTickType (RuleFired _)                = text "RuleFired"
pprTickType LetFloatFromLet              = text "LetFloatFromLet"
pprTickType (EtaExpansion _)             = text "EtaExpansion"
pprTickType (EtaReduction _)             = text "EtaReduction"
pprTickType (BetaReduction _)            = text "BetaReduction"
pprTickType (CaseOfCase _)               = text "CaseOfCase"
pprTickType (KnownBranch _)              = text "KnownBranch"
pprTickType (CaseMerge _)                = text "CaseMerge"
pprTickType (AltMerge _)                 = text "AltMerge"
pprTickType (CaseElim _)                 = text "CaseElim"
pprTickType (CaseIdentity _)             = text "CaseIdentity"
pprTickType (FillInCaseDefault _)        = text "FillInCaseDefault"
pprTickType SimplifierDone               = text "SimplifierDone"

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
