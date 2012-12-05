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
import StaticFlags ( v_opt_C_ready )
import CmdLineParser
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
  when ready $ throwGhcException (ProgramError "Too late for parseStaticFlags: call it before newSession")

  (leftover, errs, warns) <- processArgs flagsAvailable args
  when (not (null errs)) $ throwGhcException $ errorsToGhcException errs

    -- see sanity code in staticOpts
  writeIORef v_opt_C_ready True

  return (leftover, warns)

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
        ------ Debugging ----------------------------------------------------
    Flag "dppr-debug"                  (PassFlag addOpt)
  , Flag "dno-debug-output"            (PassFlag addOpt)
      -- rest of the debugging flags are dynamic

        ----- RTS opts ------------------------------------------------------
  , Flag "H"              (HasArg (\s -> liftEwM (setHeapSize (fromIntegral (decodeSize s)))))

  , Flag "Rghc-timing"    (NoArg (liftEwM enableTimingStats))

        ------ Compiler flags -----------------------------------------------

        -- All other "-fno-<blah>" options cancel out "-f<blah>" on the hsc cmdline
  , Flag "fno-"
         (PrefixPred (\s -> isStaticFlag ("f"++s)) (\s -> removeOpt ("-f"++s)))


        -- Pass all remaining "-f<blah>" options to hsc
  , Flag "f" (AnySuffixPred isStaticFlag addOpt)
  ]

isStaticFlag :: String -> Bool
isStaticFlag f =
  f `elem` [
    "fdicts-strict",
    "fspec-inline-join-points",
    "fno-hi-version-check",
    "dno-black-holing",
    "fno-state-hack",
    "fruntime-types",
    "fno-opt-coercion",
    "fno-flat-cache",
    "fhardwire-lib-paths",
    "fcpr-off"
    ]
  || any (`isPrefixOf` f) [
     ]

-----------------------------------------------------------------------------
-- convert sizes like "3.5M" into integers

decodeSize :: String -> Integer
decodeSize str
  | c == ""      = truncate n
  | c == "K" || c == "k" = truncate (n * 1000)
  | c == "M" || c == "m" = truncate (n * 1000 * 1000)
  | c == "G" || c == "g" = truncate (n * 1000 * 1000 * 1000)
  | otherwise            = throwGhcException (CmdLineError ("can't decode size: " ++ str))
  where (m, c) = span pred str
        n      = readRational m
        pred c = isDigit c || c == '.'


type StaticP = EwM IO

addOpt :: String -> StaticP ()
addOpt = liftEwM . SF.addOpt

removeOpt :: String -> StaticP ()
removeOpt = liftEwM . SF.removeOpt

-----------------------------------------------------------------------------
-- RTS Hooks

foreign import ccall unsafe "setHeapSize"       setHeapSize       :: Int -> IO ()
foreign import ccall unsafe "enableTimingStats" enableTimingStats :: IO ()

