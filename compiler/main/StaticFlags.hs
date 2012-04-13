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

	-- Ways
	WayName(..), Way(..), v_Ways, isRTSWay, mkBuildTag,

	-- Output style options
	opt_PprUserLength,
	opt_PprCols,
	opt_PprCaseAsLet,
	opt_PprStyle_Debug, opt_TraceLevel,
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

	-- profiling opts
	opt_SccProfilingOn,

        -- Hpc opts
	opt_Hpc,

	-- language opts
	opt_DictsStrict,
	opt_IrrefutableTuples,
	opt_Parallel,

	-- optimisation opts
	opt_NoStateHack,
        opt_SimpleListLiterals,
	opt_CprOff,
	opt_SimplNoPreInlining,
	opt_SimplExcessPrecision,
	opt_NoOptCoercion,
	opt_MaxWorkerArgs,

	-- Unfolding control
	opt_UF_CreationThreshold,
	opt_UF_UseThreshold,
	opt_UF_FunAppDiscount,
	opt_UF_DictDiscount,
	opt_UF_KeenessFactor,
	opt_UF_DearOp,

	-- Optimization fuel controls
	opt_Fuel,

	-- Related to linking
	opt_PIC,
	opt_Static,

	-- misc opts
	opt_IgnoreDotGhci,
	opt_ErrorSpans,
	opt_GranMacros,
	opt_HiVersion,
	opt_HistorySize,
        opt_Unregisterised,
	v_Ld_inputs,
	tablesNextToCode,
        opt_StubDeadValues,
        opt_Ticky,

    -- For the parser
    addOpt, removeOpt, addWay, getWayFlags, v_opt_C_ready,

    -- Saving/restoring globals
    saveStaticFlagGlobals, restoreStaticFlagGlobals
  ) where

#include "HsVersions.h"

import Config
import FastString
import Util
import Maybes		( firstJusts )
import Panic

import Control.Monad    ( liftM3 )
import Data.Maybe       ( listToMaybe )
import Data.IORef
import System.IO.Unsafe	( unsafePerformIO )
import Data.List

-----------------------------------------------------------------------------
-- Static flags

initStaticOpts :: IO ()
initStaticOpts = writeIORef v_opt_C_ready True

addOpt :: String -> IO ()
addOpt = consIORef v_opt_C

addWay :: WayName -> IO ()
addWay = consIORef v_Ways . lkupWay

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
GLOBAL_VAR(v_opt_C, defaultStaticOpts, [String])
GLOBAL_VAR(v_opt_C_ready, False, Bool)

staticFlags :: [String]
staticFlags = unsafePerformIO $ do
  ready <- readIORef v_opt_C_ready
  if (not ready)
        then panic "Static flags have not been initialised!\n        Please call GHC.newSession or GHC.parseStaticFlags early enough."
        else readIORef v_opt_C

-- -static is the default
defaultStaticOpts :: [String]
defaultStaticOpts = ["-static"]

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

opt_IgnoreDotGhci :: Bool
opt_IgnoreDotGhci		= lookUp (fsLit "-ignore-dot-ghci")

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

-- | Display case expressions with a single alternative as strict let bindings
opt_PprCaseAsLet :: Bool
opt_PprCaseAsLet	= lookUp   (fsLit "-dppr-case-as-let")

-- | Set the maximum width of the dumps
--   If GHC's command line options are bad then the options parser uses the
--   pretty printer display the error message. In this case the staticFlags
--   won't be initialized yet, so we must check for this case explicitly
--   and return the default value.
opt_PprCols :: Int
opt_PprCols
 = unsafePerformIO
 $ do	ready <- readIORef v_opt_C_ready
	if (not ready)
		then return 100
		else return $ lookup_def_int "-dppr-cols" 100


opt_PprStyle_Debug  :: Bool
opt_PprStyle_Debug              = lookUp  (fsLit "-dppr-debug")

opt_TraceLevel :: Int
opt_TraceLevel = lookup_def_int "-dtrace-level" 1  	-- Standard level is 1
	       	 			    	        -- Less verbose is 0

opt_PprUserLength   :: Int
opt_PprUserLength	        = lookup_def_int "-dppr-user-length" 5 --ToDo: give this a name

opt_Fuel            :: Int
opt_Fuel                        = lookup_def_int "-dopt-fuel" maxBound

opt_NoDebugOutput   :: Bool
opt_NoDebugOutput               = lookUp  (fsLit "-dno-debug-output")

-- profiling opts
opt_SccProfilingOn :: Bool
opt_SccProfilingOn		= lookUp  (fsLit "-fscc-profiling")

-- Hpc opts
opt_Hpc :: Bool
opt_Hpc				= lookUp (fsLit "-fhpc")

-- language opts
opt_DictsStrict :: Bool
opt_DictsStrict			= lookUp  (fsLit "-fdicts-strict")

opt_IrrefutableTuples :: Bool
opt_IrrefutableTuples		= lookUp  (fsLit "-firrefutable-tuples")

opt_Parallel :: Bool
opt_Parallel			= lookUp  (fsLit "-fparallel")

opt_SimpleListLiterals :: Bool
opt_SimpleListLiterals	        = lookUp  (fsLit "-fsimple-list-literals")

opt_NoStateHack :: Bool
opt_NoStateHack			= lookUp  (fsLit "-fno-state-hack")

opt_CprOff :: Bool
opt_CprOff			= lookUp  (fsLit "-fcpr-off")
	-- Switch off CPR analysis in the new demand analyser
opt_MaxWorkerArgs :: Int
opt_MaxWorkerArgs		= lookup_def_int "-fmax-worker-args" (10::Int)

opt_GranMacros :: Bool
opt_GranMacros			= lookUp  (fsLit "-fgransim")

opt_HiVersion :: Integer
opt_HiVersion			= read (cProjectVersionInt ++ cProjectPatchLevel) :: Integer

opt_HistorySize :: Int
opt_HistorySize			= lookup_def_int "-fhistory-size" 20

opt_StubDeadValues  :: Bool
opt_StubDeadValues		= lookUp  (fsLit "-dstub-dead-values")

-- Simplifier switches
opt_SimplNoPreInlining :: Bool
opt_SimplNoPreInlining		= lookUp  (fsLit "-fno-pre-inlining")
	-- NoPreInlining is there just to see how bad things
	-- get if you don't do it!
opt_SimplExcessPrecision :: Bool
opt_SimplExcessPrecision	= lookUp  (fsLit "-fexcess-precision")

opt_NoOptCoercion :: Bool
opt_NoOptCoercion    	        = lookUp  (fsLit "-fno-opt-coercion")

-- Unfolding control
-- See Note [Discounts and thresholds] in CoreUnfold

opt_UF_CreationThreshold, opt_UF_UseThreshold :: Int
opt_UF_DearOp, opt_UF_FunAppDiscount, opt_UF_DictDiscount :: Int
opt_UF_KeenessFactor :: Float

opt_UF_CreationThreshold = lookup_def_int "-funfolding-creation-threshold" (450::Int)
opt_UF_UseThreshold      = lookup_def_int "-funfolding-use-threshold"      (60::Int)
opt_UF_FunAppDiscount    = lookup_def_int "-funfolding-fun-discount"       (60::Int)

opt_UF_DictDiscount      = lookup_def_int "-funfolding-dict-discount"      (30::Int)
   -- Be fairly keen to inline a fuction if that means
   -- we'll be able to pick the right method from a dictionary

opt_UF_KeenessFactor	 = lookup_def_float "-funfolding-keeness-factor"   (1.5::Float)
opt_UF_DearOp            = ( 40 :: Int)


-- Related to linking
opt_PIC :: Bool
#if darwin_TARGET_OS && x86_64_TARGET_ARCH
opt_PIC                         = True
#elif darwin_TARGET_OS
opt_PIC                         = lookUp (fsLit "-fPIC") || not opt_Static
#else
opt_PIC                         = lookUp (fsLit "-fPIC")
#endif
opt_Static :: Bool
opt_Static			= lookUp  (fsLit "-static")
opt_Unregisterised :: Bool
opt_Unregisterised		= lookUp  (fsLit "-funregisterised")

-- Derived, not a real option.  Determines whether we will be compiling
-- info tables that reside just before the entry code, or with an
-- indirection to the entry code.  See TABLES_NEXT_TO_CODE in
-- includes/rts/storage/InfoTables.h.
tablesNextToCode :: Bool
tablesNextToCode 		= not opt_Unregisterised
		 		  && cGhcEnableTablesNextToCode == "YES"

-- Include full span info in error messages, instead of just the start position.
opt_ErrorSpans :: Bool
opt_ErrorSpans			= lookUp (fsLit "-ferror-spans")

opt_Ticky :: Bool
opt_Ticky                       = lookUp (fsLit "-ticky")

-- object files and libraries to be linked in are collected here.
-- ToDo: perhaps this could be done without a global, it wasn't obvious
-- how to do it though --SDM.
GLOBAL_VAR(v_Ld_inputs,	[],      [String])

-----------------------------------------------------------------------------
-- Ways

-- The central concept of a "way" is that all objects in a given
-- program must be compiled in the same "way".  Certain options change
-- parameters of the virtual machine, eg. profiling adds an extra word
-- to the object header, so profiling objects cannot be linked with
-- non-profiling objects.

-- After parsing the command-line options, we determine which "way" we
-- are building - this might be a combination way, eg. profiling+threaded.

-- We then find the "build-tag" associated with this way, and this
-- becomes the suffix used to find .hi files and libraries used in
-- this compilation.

data WayName
  = WayThreaded
  | WayDebug
  | WayProf
  | WayEventLog
  | WayPar
  | WayGran
  | WayNDP
  | WayDyn
  deriving (Eq,Ord)

GLOBAL_VAR(v_Ways, [] ,[Way])

allowed_combination :: [WayName] -> Bool
allowed_combination way = and [ x `allowedWith` y
			      | x <- way, y <- way, x < y ]
  where
	-- Note ordering in these tests: the left argument is
	-- <= the right argument, according to the Ord instance
	-- on Way above.

	-- dyn is allowed with everything
	_ `allowedWith` WayDyn  		= True
	WayDyn `allowedWith` _		        = True

	-- debug is allowed with everything
	_ `allowedWith` WayDebug		= True
	WayDebug `allowedWith` _		= True

	WayProf `allowedWith` WayNDP		= True
	WayThreaded `allowedWith` WayProf	= True
	WayThreaded `allowedWith` WayEventLog	= True
	_ `allowedWith` _ 			= False


getWayFlags :: IO [String]  -- new options
getWayFlags = do
  unsorted <- readIORef v_Ways
  let ways = sortBy (compare `on` wayName) $
             nubBy  ((==) `on` wayName) $ unsorted
  writeIORef v_Ways ways

  if not (allowed_combination (map wayName ways))
      then ghcError (CmdLineError $
      		    "combination not supported: "  ++
      		    foldr1 (\a b -> a ++ '/':b)
      		    (map wayDesc ways))
      else
      	   return (concatMap wayOpts ways)

mkBuildTag :: [Way] -> String
mkBuildTag ways = concat (intersperse "_" (map wayTag ways))

lkupWay :: WayName -> Way
lkupWay w =
   case listToMaybe (filter ((==) w . wayName) way_details) of
	Nothing -> error "findBuildTag"
	Just details -> details

isRTSWay :: WayName -> Bool
isRTSWay = wayRTSOnly . lkupWay

data Way = Way {
  wayName    :: WayName,
  wayTag     :: String,
  wayRTSOnly :: Bool,
  wayDesc    :: String,
  wayOpts    :: [String]
  }

way_details :: [ Way ]
way_details =
  [ Way WayThreaded "thr" True "Threaded" [
#if defined(freebsd_TARGET_OS)
--	  "-optc-pthread"
--      , "-optl-pthread"
	-- FreeBSD's default threading library is the KSE-based M:N libpthread,
	-- which GHC has some problems with.  It's currently not clear whether
	-- the problems are our fault or theirs, but it seems that using the
	-- alternative 1:1 threading library libthr works around it:
	  "-optl-lthr"
#elif defined(openbsd_TARGET_OS) || defined(netbsd_TARGET_OS)
	  "-optc-pthread"
	, "-optl-pthread"
#elif defined(solaris2_TARGET_OS)
          "-optl-lrt"
#endif
	],

    Way WayDebug "debug" True "Debug" [],

    Way WayDyn "dyn" False "Dynamic"
	[ "-DDYNAMIC"
	, "-optc-DDYNAMIC"
#if defined(mingw32_TARGET_OS)
	-- On Windows, code that is to be linked into a dynamic library must be compiled
	--	with -fPIC. Labels not in the current package are assumed to be in a DLL
	--	different from the current one.
	, "-fPIC"
#elif defined(openbsd_TARGET_OS) || defined(netbsd_TARGET_OS)
	-- Without this, linking the shared libHSffi fails because
	-- it uses pthread mutexes.
	, "-optl-pthread"
#endif
	],

    Way WayProf "p" False "Profiling"
	[ "-fscc-profiling"
	, "-DPROFILING"
	, "-optc-DPROFILING" ],

    Way WayEventLog "l" True "RTS Event Logging"
	[ "-DTRACING"
	, "-optc-DTRACING" ],

    Way WayPar "mp" False "Parallel"
	[ "-fparallel"
	, "-D__PARALLEL_HASKELL__"
	, "-optc-DPAR"
	, "-package concurrent"
        , "-optc-w"
        , "-optl-L${PVM_ROOT}/lib/${PVM_ARCH}"
        , "-optl-lpvm3"
        , "-optl-lgpvm3" ],

    -- at the moment we only change the RTS and could share compiler and libs!
    Way WayPar "mt" False "Parallel ticky profiling"
	[ "-fparallel"
	, "-D__PARALLEL_HASKELL__"
	, "-optc-DPAR"
	, "-optc-DPAR_TICKY"
	, "-package concurrent"
        , "-optc-w"
        , "-optl-L${PVM_ROOT}/lib/${PVM_ARCH}"
        , "-optl-lpvm3"
        , "-optl-lgpvm3" ],

    Way WayPar "md" False "Distributed"
	[ "-fparallel"
	, "-D__PARALLEL_HASKELL__"
	, "-D__DISTRIBUTED_HASKELL__"
	, "-optc-DPAR"
	, "-optc-DDIST"
	, "-package concurrent"
        , "-optc-w"
        , "-optl-L${PVM_ROOT}/lib/${PVM_ARCH}"
        , "-optl-lpvm3"
        , "-optl-lgpvm3" ],

    Way WayGran "mg" False "GranSim"
	[ "-fgransim"
	, "-D__GRANSIM__"
	, "-optc-DGRAN"
	, "-package concurrent" ],

    Way WayNDP "ndp" False "Nested data parallelism"
	[ "-XParr"
	, "-fvectorise"]
  ]

-----------------------------------------------------------------------------
-- Tunneling our global variables into a new instance of the GHC library

-- Ignore the v_Ld_inputs global because:
--  a) It is mutated even once GHC has been initialised, which means that I'd
--     have to add another layer of indirection to truly share the value
--  b) We can get away without sharing it because it only affects the link,
--     and is mutated by the GHC exe. Users who load up a new copy of the GHC
--     library while another is running almost certainly won't actually access it.
saveStaticFlagGlobals :: IO (Bool, [String], [Way])
saveStaticFlagGlobals = liftM3 (,,) (readIORef v_opt_C_ready) (readIORef v_opt_C) (readIORef v_Ways)

restoreStaticFlagGlobals :: (Bool, [String], [Way]) -> IO ()
restoreStaticFlagGlobals (c_ready, c, ways) = do
    writeIORef v_opt_C_ready c_ready
    writeIORef v_opt_C c
    writeIORef v_Ways ways

