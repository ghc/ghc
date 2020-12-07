{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Utility module for the pattern-match coverage checker.
module GHC.HsToCore.Pmc.Utils (

        tracePm, mkPmId,
        allPmCheckWarnings, overlapping, exhaustive, redundantBang,
        exhaustiveWarningFlag,
        isMatchContextPmChecked, needToRunPmCheck

    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Types.Basic (Origin(..), isGenerated)
import GHC.Driver.Session
import GHC.Hs
import GHC.Core.Type
import GHC.Data.EnumSet (EnumSet)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString
import GHC.Data.IOEnv
import qualified GHC.LanguageExtensions as LangExt
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Unique.Supply
import GHC.Types.SrcLoc
import GHC.Utils.Error
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.HsToCore.Monad

tracePm :: String -> SDoc -> DsM ()
tracePm herald doc = do
  dflags <- getDynFlags
  printer <- mkPrintUnqualifiedDs
  liftIO $ dumpIfSet_dyn_printer printer dflags
            Opt_D_dump_ec_trace "" FormatText (text herald $$ (nest 2 doc))
{-# INLINE tracePm #-}  -- see Note [INLINE conditional tracing utilities]

-- | Generate a fresh `Id` of a given type
mkPmId :: Type -> DsM Id
mkPmId ty = getUniqueM >>= \unique ->
  let occname = mkVarOccFS $ fsLit "pm"
      name    = mkInternalName unique occname noSrcSpan
  in  return (mkLocalIdOrCoVar name Many ty)

-- | All warning flags that need to run the pattern match checker.
allPmCheckWarnings :: [WarningFlag]
allPmCheckWarnings =
  [ Opt_WarnIncompletePatterns
  , Opt_WarnIncompleteUniPatterns
  , Opt_WarnIncompletePatternsRecUpd
  , Opt_WarnOverlappingPatterns
  ]

-- | Check whether the redundancy checker should run (redundancy only)
overlapping :: DynFlags -> HsMatchContext id -> Bool
-- See Note [Inaccessible warnings for record updates]
overlapping _      RecUpd = False
overlapping dflags _      = wopt Opt_WarnOverlappingPatterns dflags

-- | Check whether the exhaustiveness checker should run (exhaustiveness only)
exhaustive :: DynFlags -> HsMatchContext id -> Bool
exhaustive  dflags = maybe False (`wopt` dflags) . exhaustiveWarningFlag (extensionFlags dflags)

-- | Check whether unnecessary bangs should be warned about
redundantBang :: DynFlags -> Bool
redundantBang dflags = wopt Opt_WarnRedundantBangPatterns dflags

-- | Denotes whether an exhaustiveness check is supported, and if so,
-- via which 'WarningFlag' it's controlled.
-- Returns 'Nothing' if check is not supported.
exhaustiveWarningFlag :: EnumSet LangExt.Extension -> HsMatchContext id -> Maybe WarningFlag
exhaustiveWarningFlag _ (FunRhs {})   = Just Opt_WarnIncompletePatterns
exhaustiveWarningFlag _ CaseAlt       = Just Opt_WarnIncompletePatterns
exhaustiveWarningFlag _ IfAlt         = Just Opt_WarnIncompletePatterns
exhaustiveWarningFlag _ LambdaExpr    = Just Opt_WarnIncompleteUniPatterns
exhaustiveWarningFlag _ PatBindRhs    = Just Opt_WarnIncompleteUniPatterns
exhaustiveWarningFlag _ PatBindGuards = Just Opt_WarnIncompletePatterns
exhaustiveWarningFlag _ ProcExpr      = Just Opt_WarnIncompleteUniPatterns
exhaustiveWarningFlag _ RecUpd        = Just Opt_WarnIncompletePatternsRecUpd
exhaustiveWarningFlag _ ThPatSplice   = Nothing
exhaustiveWarningFlag _ PatSyn        = Nothing
exhaustiveWarningFlag _ ThPatQuote    = Nothing
exhaustiveWarningFlag exts (StmtCtxt DoExpr {}) | not (EnumSet.member LangExt.FallibleDo exts) =
                        Just Opt_WarnIncompleteUniPatterns
exhaustiveWarningFlag exts (StmtCtxt MDoExpr {}) | not (EnumSet.member LangExt.FallibleDo exts) =
                        Just Opt_WarnIncompleteUniPatterns
-- Don't warn about incomplete patterns in list comprehensions, pattern guards
-- etc. They are often *supposed* to be incomplete
exhaustiveWarningFlag _ (StmtCtxt {}) = Nothing

-- | Check whether any part of pattern match checking is enabled for this
-- 'HsMatchContext' (does not matter whether it is the redundancy check or the
-- exhaustiveness check).
isMatchContextPmChecked :: DynFlags -> Origin -> HsMatchContext id -> Bool
isMatchContextPmChecked dflags origin kind
  | isGenerated origin
  = False
  | otherwise
  = overlapping dflags kind || exhaustive dflags kind

-- | Return True when any of the pattern match warnings ('allPmCheckWarnings')
-- are enabled, in which case we need to run the pattern match checker.
needToRunPmCheck :: DynFlags -> Origin -> Bool
needToRunPmCheck dflags origin
  | isGenerated origin
  = False
  | otherwise
  = notNull (filter (`wopt` dflags) allPmCheckWarnings)

{- Note [Inaccessible warnings for record updates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#12957)
  data T a where
    T1 :: { x :: Int } -> T Bool
    T2 :: { x :: Int } -> T a
    T3 :: T a

  f :: T Char -> T a
  f r = r { x = 3 }

The desugarer will conservatively generate a case for T1 even though
it's impossible:
  f r = case r of
          T1 x -> T1 3   -- Inaccessible branch
          T2 x -> T2 3
          _    -> error "Missing"

We don't want to warn about the inaccessible branch because the programmer
didn't put it there!  So we filter out the warning here.

The same can happen for long distance term constraints instead of type
constraints (#17783):

  data T = A { x :: Int } | B { x :: Int }
  f r@A{} = r { x = 3 }
  f _     = B 0

Here, the long distance info from the FunRhs match (@r ~ A x@) will make the
clause matching on @B@ of the desugaring to @case@ redundant. It's generated
code that we don't want to warn about.
-}
