
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Utility module for the pattern-match coverage checker.
module GHC.HsToCore.Pmc.Utils (

        tracePm, traceWhenFailPm, mkPmId,
        allPmCheckWarnings, overlapping, exhaustive, redundantBang,
        exhaustiveWarningFlag,
        isMatchContextPmChecked, isMatchContextPmChecked_SinglePat,
        needToRunPmCheck

    ) where

import GHC.Prelude

import GHC.Types.Basic (Origin(..), requiresPMC)
import GHC.Driver.DynFlags
import GHC.Hs
import GHC.Core.Type
import GHC.Data.FastString
import GHC.Data.IOEnv
import GHC.Data.Maybe
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Unique.Supply
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Logger
import GHC.HsToCore.Monad

import Control.Monad

tracePm :: String -> SDoc -> DsM ()
tracePm herald doc = do
  logger  <- getLogger
  name_ppr_ctx <- mkNamePprCtxDs
  liftIO $ putDumpFileMaybe' logger name_ppr_ctx
            Opt_D_dump_ec_trace "" FormatText (text herald $$ (nest 2 doc))
{-# INLINE tracePm #-}  -- see Note [INLINE conditional tracing utilities]

traceWhenFailPm :: String -> SDoc -> MaybeT DsM a -> MaybeT DsM a
traceWhenFailPm herald doc act = MaybeT $ do
  mb_a <- runMaybeT act
  when (isNothing mb_a) $ tracePm herald doc
  pure mb_a
{-# INLINE traceWhenFailPm #-}  -- see Note [INLINE conditional tracing utilities]

-- | Generate a fresh `Id` of a given type
mkPmId :: Type -> DsM Id
mkPmId ty = getUniqueM >>= \unique ->
  let occname = mkVarOccFS $ fsLit "pm"
  in  return (mkUserLocal occname unique ManyTy ty noSrcSpan)
{-# NOINLINE mkPmId #-} -- We'll CPR deeply, that should be enough

-- | All warning flags that need to run the pattern match checker.
allPmCheckWarnings :: [WarningFlag]
allPmCheckWarnings =
  [ Opt_WarnIncompletePatterns
  , Opt_WarnIncompleteUniPatterns
  , Opt_WarnIncompletePatternsRecUpd
  , Opt_WarnOverlappingPatterns
  , Opt_WarnIncompleteRecordSelectors
  ]

-- | Check whether the redundancy checker should run (redundancy only)
overlapping :: DynFlags -> HsMatchContext fn -> Bool
-- See Note [Inaccessible warnings for record updates]
overlapping _      RecUpd = False
overlapping dflags _      = wopt Opt_WarnOverlappingPatterns dflags

-- | Check whether the exhaustiveness checker should run (exhaustiveness only)
exhaustive :: DynFlags -> HsMatchContext fn -> Bool
exhaustive  dflags = maybe False (`wopt` dflags) . exhaustiveWarningFlag

-- | Check whether unnecessary bangs should be warned about
redundantBang :: DynFlags -> Bool
redundantBang dflags = wopt Opt_WarnRedundantBangPatterns dflags

-- | Denotes whether an exhaustiveness check is supported, and if so,
-- via which 'WarningFlag' it's controlled.
-- Returns 'Nothing' if check is not supported.
exhaustiveWarningFlag :: HsMatchContext fn -> Maybe WarningFlag
exhaustiveWarningFlag FunRhs{}           = Just Opt_WarnIncompletePatterns
exhaustiveWarningFlag CaseAlt            = Just Opt_WarnIncompletePatterns
exhaustiveWarningFlag IfAlt              = Just Opt_WarnIncompletePatterns
exhaustiveWarningFlag (LamAlt LamSingle) = Just Opt_WarnIncompleteUniPatterns
exhaustiveWarningFlag (LamAlt _case)     = Just Opt_WarnIncompletePatterns
exhaustiveWarningFlag PatBindRhs         = Just Opt_WarnIncompleteUniPatterns
exhaustiveWarningFlag PatBindGuards      = Just Opt_WarnIncompletePatterns
exhaustiveWarningFlag (ArrowMatchCtxt c) = arrowMatchContextExhaustiveWarningFlag c
exhaustiveWarningFlag RecUpd             = Just Opt_WarnIncompletePatternsRecUpd
exhaustiveWarningFlag LazyPatCtx         = Just Opt_WarnIncompleteUniPatterns
exhaustiveWarningFlag ThPatSplice        = Nothing
exhaustiveWarningFlag PatSyn             = Nothing
exhaustiveWarningFlag ThPatQuote         = Nothing
-- Don't warn about incomplete patterns in list comprehensions, pattern guards
-- etc. They are often *supposed* to be incomplete
exhaustiveWarningFlag StmtCtxt{}         = Nothing

arrowMatchContextExhaustiveWarningFlag :: HsArrowMatchContext -> Maybe WarningFlag
arrowMatchContextExhaustiveWarningFlag = \ case
  ProcExpr              -> Just Opt_WarnIncompleteUniPatterns
  ArrowCaseAlt          -> Just Opt_WarnIncompletePatterns
  ArrowLamAlt LamSingle -> Just Opt_WarnIncompleteUniPatterns
  ArrowLamAlt _         -> Just Opt_WarnIncompletePatterns

-- | Check whether any part of pattern match checking is enabled for this
-- 'HsMatchContext' (does not matter whether it is the redundancy check or the
-- exhaustiveness check).
isMatchContextPmChecked :: DynFlags -> Origin -> HsMatchContext fn -> Bool
isMatchContextPmChecked dflags origin ctxt
  =  requiresPMC origin
  && (overlapping dflags ctxt || exhaustive dflags ctxt)

-- | Check whether exhaustivity checks are enabled for this 'HsMatchContext',
-- when dealing with a single pattern (using the 'matchSinglePatVar' function).
isMatchContextPmChecked_SinglePat :: DynFlags -> Origin -> HsMatchContext fn -> LPat GhcTc -> Bool
isMatchContextPmChecked_SinglePat dflags origin ctxt pat
  | not (needToRunPmCheck dflags origin)
  = False
  | StmtCtxt {} <- ctxt
  -- For @StmtCtxt@, we are interested in propagating pattern-match information
  -- but not in the actual outcome of pattern-match checking, so we skip
  -- if the pattern is "boring" (gives rise to no long-distance information).
  -- (This is done purely for runtime performance.)
  = not (isBoringHsPat pat) -- See Note [Boring patterns] in GHC.Hs.Pat.
  | otherwise
  = overlapping dflags ctxt || exhaustive dflags ctxt

-- | Return True when any of the pattern match warnings ('allPmCheckWarnings')
-- are enabled, in which case we need to run the pattern match checker.
needToRunPmCheck :: DynFlags -> Origin -> Bool
needToRunPmCheck dflags origin
  =  requiresPMC origin
  && any (`wopt` dflags) allPmCheckWarnings

{- Note [Inaccessible warnings for record updates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#12957):

  data T a where
    T1 :: { x :: Int } -> T Bool
    T2 :: { x :: Int } -> T a
    T3 :: T a

  f :: T Char -> T a
  f r = r { x = 3 }

In GHC.Tc.Gen.Expr.desugarRecordUpd, we will conservatively generate a case
for T1 even though it's impossible:

  f r = case r of
          T1 x -> T1 3   -- Inaccessible branch
          T2 x -> T2 3
          _    -> error "Missing"

We don't want to warn about the inaccessible branch because the programmer
didn't put it there!  So we filter out the warning here.
The test case T12957a checks this.

The same can happen for long distance term constraints instead of type
constraints (#17783):

  data T = A { x :: Int } | B
  f r@A{} = r { x = 3 }
  f _     = B

Here, the long distance info from the FunRhs match (@r ~ A x@) will make the
clause matching on @B@ of the desugaring to @case@ redundant. It's generated
code that we don't want to warn about.
-}
