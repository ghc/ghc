{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

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
        unsafeGlobalDynFlags, setUnsafeGlobalDynFlags,

	staticFlags,
        initStaticOpts,

	-- Output style options
	opt_PprStyle_Debug,
        opt_NoDebugOutput,

	-- language opts
	opt_DictsStrict,

	-- optimisation opts
	opt_NoStateHack,
	opt_CprOff,
	opt_NoOptCoercion,
        opt_NoFlatCache,

    -- For the parser
    addOpt, removeOpt, v_opt_C_ready,

    -- Saving/restoring globals
    saveStaticFlagGlobals, restoreStaticFlagGlobals
  ) where

#include "HsVersions.h"

import {-# SOURCE #-} DynFlags (DynFlags)

import FastString
import Util
-- import Maybes		( firstJusts )
import Panic

import Control.Monad
import Data.IORef
import System.IO.Unsafe	( unsafePerformIO )
-- import Data.List

--------------------------------------------------------------------------
-- Do not use unsafeGlobalDynFlags!
--
-- unsafeGlobalDynFlags is a hack, necessary because we need to be able
-- to show SDocs when tracing, but we don't always have DynFlags
-- available.
--
-- Do not use it if you can help it. You may get the wrong value!

GLOBAL_VAR(v_unsafeGlobalDynFlags, panic "v_unsafeGlobalDynFlags: not initialised", DynFlags)

unsafeGlobalDynFlags :: DynFlags
unsafeGlobalDynFlags = unsafePerformIO $ readIORef v_unsafeGlobalDynFlags

setUnsafeGlobalDynFlags :: DynFlags -> IO ()
setUnsafeGlobalDynFlags = writeIORef v_unsafeGlobalDynFlags

-----------------------------------------------------------------------------
-- Static flags

initStaticOpts :: IO ()
initStaticOpts = writeIORef v_opt_C_ready True

addOpt :: String -> IO ()
addOpt = consIORef v_opt_C

removeOpt :: String -> IO ()
removeOpt f = do
  fs <- readIORef v_opt_C
  writeIORef v_opt_C $! filter (/= f) fs

lookUp	       	 :: FastString -> Bool

-- holds the static opts while they're being collected, before
-- being unsafely read by unpacked_static_opts below.
GLOBAL_VAR(v_opt_C, [], [String])
GLOBAL_VAR(v_opt_C_ready, False, Bool)

staticFlags :: [String]
staticFlags = unsafePerformIO $ do
  ready <- readIORef v_opt_C_ready
  if (not ready)
        then panic "Static flags have not been initialised!\n        Please call GHC.newSession or GHC.parseStaticFlags early enough."
        else readIORef v_opt_C

packed_static_opts :: [FastString]
packed_static_opts   = map mkFastString staticFlags

lookUp     sw = sw `elem` packed_static_opts

{-
-- (lookup_str "foo") looks for the flag -foo=X or -fooX,
-- and returns the string X
lookup_str       :: String -> Maybe String
lookup_str sw
   = case firstJusts (map (stripPrefix sw) staticFlags) of
	Just ('=' : str) -> Just str
	Just str         -> Just str
	Nothing		 -> Nothing

lookup_def_int   :: String -> Int -> Int
lookup_def_int sw def = case (lookup_str sw) of
			    Nothing -> def		-- Use default
		  	    Just xx -> try_read sw xx

lookup_def_float :: String -> Float -> Float
lookup_def_float sw def = case (lookup_str sw) of
			    Nothing -> def		-- Use default
		  	    Just xx -> try_read sw xx

try_read :: Read a => String -> String -> a
-- (try_read sw str) tries to read s; if it fails, it
-- bleats about flag sw
try_read sw str
  = case reads str of
	((x,_):_) -> x	-- Be forgiving: ignore trailing goop, and alternative parses
	[]	  -> throwGhcException (UsageError ("Malformed argument " ++ str ++ " for flag " ++ sw))
			-- ToDo: hack alert. We should really parse the arguments
			-- 	 and announce errors in a more civilised way.
-}


{-
 Putting the compiler options into temporary at-files
 may turn out to be necessary later on if we turn hsc into
 a pure Win32 application where I think there's a command-line
 length limit of 255. unpacked_opts understands the @ option.

unpacked_opts :: [String]
unpacked_opts =
  concat $
  map (expandAts) $
  map unpackFS argv  -- NOT ARGV any more: v_Static_hsc_opts
  where
   expandAts ('@':fname) = words (unsafePerformIO (readFile fname))
   expandAts l = [l]
-}

-- debugging options

opt_PprStyle_Debug  :: Bool
opt_PprStyle_Debug              = lookUp  (fsLit "-dppr-debug")

opt_NoDebugOutput   :: Bool
opt_NoDebugOutput               = lookUp  (fsLit "-dno-debug-output")

-- language opts
opt_DictsStrict :: Bool
opt_DictsStrict			= lookUp  (fsLit "-fdicts-strict")

opt_NoStateHack :: Bool
opt_NoStateHack			= lookUp  (fsLit "-fno-state-hack")

opt_CprOff :: Bool
opt_CprOff			= lookUp  (fsLit "-fcpr-off")
	-- Switch off CPR analysis in the new demand analyser

opt_NoOptCoercion :: Bool
opt_NoOptCoercion    	        = lookUp  (fsLit "-fno-opt-coercion")

opt_NoFlatCache :: Bool
opt_NoFlatCache    	        = lookUp  (fsLit "-fno-flat-cache")

-----------------------------------------------------------------------------
-- Tunneling our global variables into a new instance of the GHC library

saveStaticFlagGlobals :: IO (Bool, [String])
saveStaticFlagGlobals = liftM2 (,) (readIORef v_opt_C_ready) (readIORef v_opt_C)

restoreStaticFlagGlobals :: (Bool, [String]) -> IO ()
restoreStaticFlagGlobals (c_ready, c) = do
    writeIORef v_opt_C_ready c_ready
    writeIORef v_opt_C c

