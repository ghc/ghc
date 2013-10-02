{-# OPTIONS -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

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

module StaticFlags (
        -- entry point
        parseStaticFlags,

        staticFlags,
        initStaticOpts,

        -- Output style options
        opt_PprStyle_Debug,
        opt_NoDebugOutput,

        -- optimisation opts
        opt_NoStateHack,
        opt_CprOff,
        opt_NoOptCoercion,

        -- For the parser
        addOpt, removeOpt, v_opt_C_ready,

        -- Saving/restoring globals
        saveStaticFlagGlobals, restoreStaticFlagGlobals,

        -- For options autocompletion
        flagsStatic, flagsStaticNames
  ) where

#include "HsVersions.h"

import CmdLineParser
import FastString
import SrcLoc
import Util
-- import Maybes                ( firstJusts )
import Panic

import Control.Monad
import Data.Char
import Data.IORef
import System.IO.Unsafe ( unsafePerformIO )


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
  when ready $ throwGhcExceptionIO (ProgramError "Too late for parseStaticFlags: call it before runGhc or runGhcT")

  (leftover, errs, warns) <- processArgs flagsAvailable args
  when (not (null errs)) $ throwGhcExceptionIO $ errorsToGhcException errs

    -- see sanity code in staticOpts
  writeIORef v_opt_C_ready True
  return (leftover, warns)

-- holds the static opts while they're being collected, before
-- being unsafely read by unpacked_static_opts below.
GLOBAL_VAR(v_opt_C, [], [String])
GLOBAL_VAR(v_opt_C_ready, False, Bool)


staticFlags :: [String]
staticFlags = unsafePerformIO $ do
  ready <- readIORef v_opt_C_ready
  if (not ready)
        then panic "Static flags have not been initialised!\n        Please call GHC.parseStaticFlags early enough."
        else readIORef v_opt_C

-- All the static flags should appear in this list.  It describes how each
-- static flag should be processed.  Two main purposes:
-- (a) if a command-line flag doesn't appear in the list, GHC can complain
-- (b) a command-line flag may remove, or add, other flags; e.g. the "-fno-X"
--     things
--
-- The common (PassFlag addOpt) action puts the static flag into the bunch of
-- things that are searched up by the top-level definitions like
--      opt_foo = lookUp (fsLit "-dfoo")

-- Note that ordering is important in the following list: any flag which
-- is a prefix flag (i.e. HasArg, Prefix, OptPrefix, AnySuffix) will override
-- flags further down the list with the same prefix.

flagsStatic :: [Flag IO]
flagsStatic = [
  ------ Debugging ----------------------------------------------------
    Flag "dppr-debug"       (PassFlag addOptEwM)
  , Flag "dno-debug-output" (PassFlag addOptEwM)
  -- rest of the debugging flags are dynamic

  ----- RTS opts ------------------------------------------------------
  , Flag "H"           (HasArg (\s -> liftEwM (setHeapSize (fromIntegral (decodeSize s)))))

  , Flag "Rghc-timing" (NoArg (liftEwM enableTimingStats))

  ------ Compiler flags -----------------------------------------------
  -- All other "-fno-<blah>" options cancel out "-f<blah>" on the hsc cmdline
  , Flag "fno-"
         (PrefixPred (\s -> isStaticFlag ("f"++s)) (\s -> removeOptEwM ("-f"++s)))

  -- Pass all remaining "-f<blah>" options to hsc
  , Flag "f" (AnySuffixPred isStaticFlag addOptEwM)
  ]



isStaticFlag :: String -> Bool
isStaticFlag f = f `elem` flagsStaticNames


flagsStaticNames :: [String]
flagsStaticNames = [
    "fno-state-hack",
    "fno-opt-coercion",
    "fcpr-off"
    ]


initStaticOpts :: IO ()
initStaticOpts = writeIORef v_opt_C_ready True

addOpt :: String -> IO ()
addOpt = consIORef v_opt_C

removeOpt :: String -> IO ()
removeOpt f = do
  fs <- readIORef v_opt_C
  writeIORef v_opt_C $! filter (/= f) fs

type StaticP = EwM IO

addOptEwM :: String -> StaticP ()
addOptEwM = liftEwM . addOpt

removeOptEwM :: String -> StaticP ()
removeOptEwM = liftEwM . removeOpt

packed_static_opts :: [FastString]
packed_static_opts   = map mkFastString staticFlags

lookUp :: FastString -> Bool
lookUp sw = sw `elem` packed_static_opts

-- debugging options

opt_PprStyle_Debug :: Bool
opt_PprStyle_Debug = lookUp  (fsLit "-dppr-debug")

opt_NoDebugOutput  :: Bool
opt_NoDebugOutput  = lookUp  (fsLit "-dno-debug-output")

opt_NoStateHack    :: Bool
opt_NoStateHack    = lookUp  (fsLit "-fno-state-hack")

-- Switch off CPR analysis in the new demand analyser
opt_CprOff         :: Bool
opt_CprOff         = lookUp  (fsLit "-fcpr-off")

opt_NoOptCoercion  :: Bool
opt_NoOptCoercion  = lookUp  (fsLit "-fno-opt-coercion")


-----------------------------------------------------------------------------
-- Convert sizes like "3.5M" into integers

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


-----------------------------------------------------------------------------
-- Tunneling our global variables into a new instance of the GHC library

saveStaticFlagGlobals :: IO (Bool, [String])
saveStaticFlagGlobals = liftM2 (,) (readIORef v_opt_C_ready) (readIORef v_opt_C)

restoreStaticFlagGlobals :: (Bool, [String]) -> IO ()
restoreStaticFlagGlobals (c_ready, c) = do
    writeIORef v_opt_C_ready c_ready
    writeIORef v_opt_C c


-----------------------------------------------------------------------------
-- RTS Hooks

foreign import ccall unsafe "setHeapSize"       setHeapSize       :: Int -> IO ()
foreign import ccall unsafe "enableTimingStats" enableTimingStats :: IO ()


{-
-- (lookup_str "foo") looks for the flag -foo=X or -fooX,
-- and returns the string X
lookup_str       :: String -> Maybe String
lookup_str sw
   = case firstJusts (map (stripPrefix sw) staticFlags) of
        Just ('=' : str) -> Just str
        Just str         -> Just str
        Nothing          -> Nothing

lookup_def_int   :: String -> Int -> Int
lookup_def_int sw def = case (lookup_str sw) of
                            Nothing -> def              -- Use default
                            Just xx -> try_read sw xx

lookup_def_float :: String -> Float -> Float
lookup_def_float sw def = case (lookup_str sw) of
                            Nothing -> def              -- Use default
                            Just xx -> try_read sw xx

try_read :: Read a => String -> String -> a
-- (try_read sw str) tries to read s; if it fails, it
-- bleats about flag sw
try_read sw str
  = case reads str of
        ((x,_):_) -> x  -- Be forgiving: ignore trailing goop, and alternative parses
        []        -> throwGhcException (UsageError ("Malformed argument " ++ str ++ " for flag " ++ sw))
                        -- ToDo: hack alert. We should really parse the arguments
                        --       and announce errors in a more civilised way.
-}

