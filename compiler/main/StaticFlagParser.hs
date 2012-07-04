-----------------------------------------------------------------------------
--
-- Static flags
--
-- Static flags can only be set once, on the command-line.  Inside GHC,
-- each static flag corresponds to a top-level value, usually of type Bool.
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module StaticFlagParser (
        parseStaticFlags,
        parseStaticFlagsFull,
        flagsStatic
    ) where

#include "HsVersions.h"

import qualified StaticFlags as SF
import StaticFlags ( v_opt_C_ready, getWayFlags, tablesNextToCode, WayName(..)
                   , opt_SimplExcessPrecision )
import CmdLineParser
import Config
import SrcLoc
import Util
import Panic

import Control.Monad
import Data.Char
import Data.IORef
import Data.List

-----------------------------------------------------------------------------
-- Static flags

-- | Parses GHC's static flags from a list of command line arguments.
--
-- These flags are static in the sense that they can be set only once and they
-- are global, meaning that they affect every instance of GHC running;
-- multiple GHC threads will use the same flags.
--
-- This function must be called before any session is started, i.e., before
-- the first call to 'GHC.withGhc'.
--
-- Static flags are more of a hack and are static for more or less historical
-- reasons.  In the long run, most static flags should eventually become
-- dynamic flags.
--
-- XXX: can we add an auto-generated list of static flags here?
--
parseStaticFlags :: [Located String] -> IO ([Located String], [Located String])
parseStaticFlags = parseStaticFlagsFull flagsStatic

-- | Parse GHC's static flags as @parseStaticFlags@ does. However it also
-- takes a list of available static flags, such that certain flags can be
-- enabled or disabled through this argument.
parseStaticFlagsFull :: [Flag IO] -> [Located String]
                     -> IO ([Located String], [Located String])
parseStaticFlagsFull flagsAvailable args = do
  ready <- readIORef v_opt_C_ready
  when ready $ ghcError (ProgramError "Too late for parseStaticFlags: call it before newSession")

  (leftover, errs, warns1) <- processArgs flagsAvailable args
  when (not (null errs)) $ ghcError $ errorsToGhcException errs

    -- deal with the way flags: the way (eg. prof) gives rise to
    -- further flags, some of which might be static.
  way_flags <- getWayFlags
  let way_flags' = map (mkGeneralLocated "in way flags") way_flags

    -- if we're unregisterised, add some more flags
  let unreg_flags | cGhcUnregisterised == "YES" = unregFlags
                  | otherwise = []

    -- as these are GHC generated flags, we parse them with all static flags
    -- in scope, regardless of what availableFlags are passed in.
  (more_leftover, errs, warns2) <-
      processArgs flagsStatic (unreg_flags ++ way_flags')

    -- see sanity code in staticOpts
  writeIORef v_opt_C_ready True

    -- TABLES_NEXT_TO_CODE affects the info table layout.
    -- Be careful to do this *after* all processArgs,
    -- because evaluating tablesNextToCode involves looking at the global
    -- static flags.  Those pesky global variables...
  let cg_flags | tablesNextToCode = map (mkGeneralLocated "in cg_flags")
                                        ["-optc-DTABLES_NEXT_TO_CODE"]
               | otherwise        = []

    -- HACK: -fexcess-precision is both a static and a dynamic flag.  If
    -- the static flag parser has slurped it, we must return it as a
    -- leftover too.  ToDo: make -fexcess-precision dynamic only.
  let excess_prec
       | opt_SimplExcessPrecision = map (mkGeneralLocated "in excess_prec")
                                        ["-fexcess-precision"]
       | otherwise                = []

  when (not (null errs)) $ ghcError $ errorsToGhcException errs
  return (excess_prec ++ cg_flags ++ more_leftover ++ leftover,
          warns1 ++ warns2)

flagsStatic :: [Flag IO]
-- All the static flags should appear in this list.  It describes how each
-- static flag should be processed.  Two main purposes:
-- (a) if a command-line flag doesn't appear in the list, GHC can complain
-- (b) a command-line flag may remove, or add, other flags; e.g. the "-fno-X" things
--
-- The common (PassFlag addOpt) action puts the static flag into the bunch of
-- things that are searched up by the top-level definitions like
--      opt_foo = lookUp (fsLit "-dfoo")

-- Note that ordering is important in the following list: any flag which
-- is a prefix flag (i.e. HasArg, Prefix, OptPrefix, AnySuffix) will override
-- flags further down the list with the same prefix.

flagsStatic = [
        ------- ways --------------------------------------------------------
    Flag "prof"           (NoArg (addWay WayProf))
  , Flag "eventlog"       (NoArg (addWay WayEventLog))
  , Flag "parallel"       (NoArg (addWay WayPar))
  , Flag "gransim"        (NoArg (addWay WayGran))
  , Flag "smp"            (NoArg (addWay WayThreaded >> deprecate "Use -threaded instead"))
  , Flag "debug"          (NoArg (addWay WayDebug))
  , Flag "ndp"            (NoArg (addWay WayNDP))
  , Flag "threaded"       (NoArg (addWay WayThreaded))

  , Flag "ticky"          (PassFlag (\f -> do addOpt f; addWay WayDebug))
    -- -ticky enables ticky-ticky code generation, and also implies -debug which
    -- is required to get the RTS ticky support.

        ------ Debugging ----------------------------------------------------
  , Flag "dppr-debug"                  (PassFlag addOpt)
  , Flag "dsuppress-all"               (PassFlag addOpt)
  , Flag "dsuppress-uniques"           (PassFlag addOpt)
  , Flag "dsuppress-coercions"         (PassFlag addOpt)
  , Flag "dsuppress-module-prefixes"   (PassFlag addOpt)
  , Flag "dsuppress-type-applications" (PassFlag addOpt)
  , Flag "dsuppress-idinfo"            (PassFlag addOpt)
  , Flag "dsuppress-var-kinds"         (PassFlag addOpt)
  , Flag "dsuppress-type-signatures"   (PassFlag addOpt)
  , Flag "dopt-fuel"                   (AnySuffix addOpt)
  , Flag "dno-debug-output"            (PassFlag addOpt)
  , Flag "dstub-dead-values"           (PassFlag addOpt)
      -- rest of the debugging flags are dynamic

        ----- Linker --------------------------------------------------------
  , Flag "static"         (PassFlag addOpt)
  , Flag "dynamic"        (NoArg (removeOpt "-static" >> addWay WayDyn))
    -- ignored for compat w/ gcc:
  , Flag "rdynamic"       (NoArg (return ()))

        ----- RTS opts ------------------------------------------------------
  , Flag "H"              (HasArg (\s -> liftEwM (setHeapSize (fromIntegral (decodeSize s)))))

  , Flag "Rghc-timing"    (NoArg (liftEwM enableTimingStats))

        ------ Compiler flags -----------------------------------------------

        -- -fPIC requires extra checking: only the NCG supports it.
        -- See also DynFlags.parseDynamicFlags.
  , Flag "fPIC" (PassFlag setPIC)

        -- All other "-fno-<blah>" options cancel out "-f<blah>" on the hsc cmdline
  , Flag "fno-"
         (PrefixPred (\s -> isStaticFlag ("f"++s)) (\s -> removeOpt ("-f"++s)))


        -- Pass all remaining "-f<blah>" options to hsc
  , Flag "f" (AnySuffixPred isStaticFlag addOpt)
  ]

setPIC :: String -> StaticP ()
setPIC | cGhcWithNativeCodeGen == "YES" || cGhcUnregisterised == "YES"
       = addOpt
       | otherwise
       = ghcError $ CmdLineError "-fPIC is not supported on this platform"

isStaticFlag :: String -> Bool
isStaticFlag f =
  f `elem` [
    "fscc-profiling",
    "fdicts-strict",
    "fspec-inline-join-points",
    "fno-hi-version-check",
    "dno-black-holing",
    "fno-state-hack",
    "fsimple-list-literals",
    "fruntime-types",
    "fno-pre-inlining",
    "fno-opt-coercion",
    "fexcess-precision",
    "static",
    "fhardwire-lib-paths",
    "funregisterised",
    "fcpr-off",
    "ferror-spans",
    "fPIC",
    "fhpc"
    ]
  || any (`isPrefixOf` f) [
    "fliberate-case-threshold",
    "fmax-worker-args",
    "fhistory-size",
    "funfolding-creation-threshold",
    "funfolding-dict-threshold",
    "funfolding-use-threshold",
    "funfolding-fun-discount",
    "funfolding-keeness-factor"
     ]

unregFlags :: [Located String]
unregFlags = map (mkGeneralLocated "in unregFlags")
   [ "-optc-DNO_REGS"
   , "-optc-DUSE_MINIINTERPRETER"
   , "-funregisterised" ]

-----------------------------------------------------------------------------
-- convert sizes like "3.5M" into integers

decodeSize :: String -> Integer
decodeSize str
  | c == ""      = truncate n
  | c == "K" || c == "k" = truncate (n * 1000)
  | c == "M" || c == "m" = truncate (n * 1000 * 1000)
  | c == "G" || c == "g" = truncate (n * 1000 * 1000 * 1000)
  | otherwise            = ghcError (CmdLineError ("can't decode size: " ++ str))
  where (m, c) = span pred str
        n      = readRational m
        pred c = isDigit c || c == '.'


type StaticP = EwM IO

addOpt :: String -> StaticP ()
addOpt = liftEwM . SF.addOpt

addWay :: WayName -> StaticP ()
addWay = liftEwM . SF.addWay

removeOpt :: String -> StaticP ()
removeOpt = liftEwM . SF.removeOpt

-----------------------------------------------------------------------------
-- RTS Hooks

foreign import ccall unsafe "setHeapSize"       setHeapSize       :: Int -> IO ()
foreign import ccall unsafe "enableTimingStats" enableTimingStats :: IO ()

