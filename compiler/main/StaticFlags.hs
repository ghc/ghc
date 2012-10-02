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
	staticFlags,
        initStaticOpts,

	-- Output style options
	opt_PprStyle_Debug,
        opt_NoDebugOutput,

	-- Suppressing boring aspects of core dumps
	opt_SuppressAll,
	opt_SuppressUniques,
        opt_SuppressCoercions,
	opt_SuppressModulePrefixes,
	opt_SuppressTypeApplications,
	opt_SuppressIdInfo,
	opt_SuppressTypeSignatures,
        opt_SuppressVarKinds,

	-- language opts
	opt_DictsStrict,

	-- optimisation opts
	opt_NoStateHack,
        opt_SimpleListLiterals,
	opt_CprOff,
	opt_SimplNoPreInlining,
	opt_SimplExcessPrecision,
	opt_NoOptCoercion,
	opt_MaxWorkerArgs,
        opt_NoFlatCache,

	-- Unfolding control
	opt_UF_CreationThreshold,
	opt_UF_UseThreshold,
	opt_UF_FunAppDiscount,
	opt_UF_DictDiscount,
	opt_UF_KeenessFactor,
	opt_UF_DearOp,

	-- misc opts
	opt_ErrorSpans,

    -- For the parser
    addOpt, removeOpt, v_opt_C_ready,

    -- Saving/restoring globals
    saveStaticFlagGlobals, restoreStaticFlagGlobals
  ) where

#include "HsVersions.h"

import FastString
import Util
import Maybes		( firstJusts )
import Panic

import Control.Monad
import Data.IORef
import System.IO.Unsafe	( unsafePerformIO )
import Data.List

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
lookup_def_int   :: String -> Int -> Int
lookup_def_float :: String -> Float -> Float
lookup_str       :: String -> Maybe String

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

-- (lookup_str "foo") looks for the flag -foo=X or -fooX,
-- and returns the string X
lookup_str sw
   = case firstJusts (map (stripPrefix sw) staticFlags) of
	Just ('=' : str) -> Just str
	Just str         -> Just str
	Nothing		 -> Nothing

lookup_def_int sw def = case (lookup_str sw) of
			    Nothing -> def		-- Use default
		  	    Just xx -> try_read sw xx

lookup_def_float sw def = case (lookup_str sw) of
			    Nothing -> def		-- Use default
		  	    Just xx -> try_read sw xx


try_read :: Read a => String -> String -> a
-- (try_read sw str) tries to read s; if it fails, it
-- bleats about flag sw
try_read sw str
  = case reads str of
	((x,_):_) -> x	-- Be forgiving: ignore trailing goop, and alternative parses
	[]	  -> ghcError (UsageError ("Malformed argument " ++ str ++ " for flag " ++ sw))
			-- ToDo: hack alert. We should really parse the arguments
			-- 	 and announce errors in a more civilised way.


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
-- | Suppress all that is suppressable in core dumps.
--   Except for uniques, as some simplifier phases introduce new varibles that
--   have otherwise identical names.
opt_SuppressAll :: Bool
opt_SuppressAll
	= lookUp  (fsLit "-dsuppress-all")

-- | Suppress all coercions, them replacing with '...'
opt_SuppressCoercions :: Bool
opt_SuppressCoercions
	=  lookUp  (fsLit "-dsuppress-all")
	|| lookUp  (fsLit "-dsuppress-coercions")

opt_SuppressVarKinds :: Bool
opt_SuppressVarKinds
	=  lookUp  (fsLit "-dsuppress-all")
	|| lookUp  (fsLit "-dsuppress-var-kinds")

-- | Suppress module id prefixes on variables.
opt_SuppressModulePrefixes :: Bool
opt_SuppressModulePrefixes
	=  lookUp  (fsLit "-dsuppress-all")
	|| lookUp  (fsLit "-dsuppress-module-prefixes")

-- | Suppress type applications.
opt_SuppressTypeApplications :: Bool
opt_SuppressTypeApplications
	=  lookUp  (fsLit "-dsuppress-all")
	|| lookUp  (fsLit "-dsuppress-type-applications")

-- | Suppress info such as arity and unfoldings on identifiers.
opt_SuppressIdInfo :: Bool
opt_SuppressIdInfo
	=  lookUp  (fsLit "-dsuppress-all")
	|| lookUp  (fsLit "-dsuppress-idinfo")

-- | Suppress separate type signatures in core, but leave types on lambda bound vars
opt_SuppressTypeSignatures :: Bool
opt_SuppressTypeSignatures
	=  lookUp  (fsLit "-dsuppress-all")
	|| lookUp  (fsLit "-dsuppress-type-signatures")

-- | Suppress unique ids on variables.
--   Except for uniques, as some simplifier phases introduce new variables that
--   have otherwise identical names.
opt_SuppressUniques :: Bool
opt_SuppressUniques
	=  lookUp  (fsLit "-dsuppress-uniques")


opt_PprStyle_Debug  :: Bool
opt_PprStyle_Debug              = lookUp  (fsLit "-dppr-debug")

opt_NoDebugOutput   :: Bool
opt_NoDebugOutput               = lookUp  (fsLit "-dno-debug-output")

-- language opts
opt_DictsStrict :: Bool
opt_DictsStrict			= lookUp  (fsLit "-fdicts-strict")

opt_SimpleListLiterals :: Bool
opt_SimpleListLiterals	        = lookUp  (fsLit "-fsimple-list-literals")

opt_NoStateHack :: Bool
opt_NoStateHack			= lookUp  (fsLit "-fno-state-hack")

opt_CprOff :: Bool
opt_CprOff			= lookUp  (fsLit "-fcpr-off")
	-- Switch off CPR analysis in the new demand analyser
opt_MaxWorkerArgs :: Int
opt_MaxWorkerArgs		= lookup_def_int "-fmax-worker-args" (10::Int)

-- Simplifier switches
opt_SimplNoPreInlining :: Bool
opt_SimplNoPreInlining		= lookUp  (fsLit "-fno-pre-inlining")
	-- NoPreInlining is there just to see how bad things
	-- get if you don't do it!
opt_SimplExcessPrecision :: Bool
opt_SimplExcessPrecision	= lookUp  (fsLit "-fexcess-precision")

opt_NoOptCoercion :: Bool
opt_NoOptCoercion    	        = lookUp  (fsLit "-fno-opt-coercion")

opt_NoFlatCache :: Bool
opt_NoFlatCache    	        = lookUp  (fsLit "-fno-flat-cache")

-- Unfolding control
-- See Note [Discounts and thresholds] in CoreUnfold

opt_UF_CreationThreshold, opt_UF_UseThreshold :: Int
opt_UF_DearOp, opt_UF_FunAppDiscount, opt_UF_DictDiscount :: Int
opt_UF_KeenessFactor :: Float

opt_UF_CreationThreshold = lookup_def_int "-funfolding-creation-threshold" (750::Int)
  -- This threshold must be reasonably high to take 
  -- account of possible discounts.  
  -- E.g. 450 is not enough in 'fulsom' for Interval.sqr to inline into Csg.calc
  --      (The unfolding for sqr never makes it into the interface file.)

opt_UF_UseThreshold      = lookup_def_int "-funfolding-use-threshold"      (60::Int)
opt_UF_FunAppDiscount    = lookup_def_int "-funfolding-fun-discount"       (60::Int)

opt_UF_DictDiscount      = lookup_def_int "-funfolding-dict-discount"      (30::Int)
   -- Be fairly keen to inline a fuction if that means
   -- we'll be able to pick the right method from a dictionary

opt_UF_KeenessFactor	 = lookup_def_float "-funfolding-keeness-factor"   (1.5::Float)
opt_UF_DearOp            = ( 40 :: Int)


-- Include full span info in error messages, instead of just the start position.
opt_ErrorSpans :: Bool
opt_ErrorSpans			= lookUp (fsLit "-ferror-spans")

-----------------------------------------------------------------------------
-- Tunneling our global variables into a new instance of the GHC library

saveStaticFlagGlobals :: IO (Bool, [String])
saveStaticFlagGlobals = liftM2 (,) (readIORef v_opt_C_ready) (readIORef v_opt_C)

restoreStaticFlagGlobals :: (Bool, [String]) -> IO ()
restoreStaticFlagGlobals (c_ready, c) = do
    writeIORef v_opt_C_ready c_ready
    writeIORef v_opt_C c

