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
	WayName(..), v_Ways, v_Build_tag, v_RTS_Build_tag, isRTSWay,

	-- Output style options
	opt_PprUserLength,
	opt_SuppressUniques,
	opt_PprStyle_Debug,
        opt_NoDebugOutput,

	-- profiling opts
	opt_AutoSccsOnAllToplevs,
	opt_AutoSccsOnExportedToplevs,
	opt_AutoSccsOnIndividualCafs,
	opt_SccProfilingOn,
	opt_DoTickyProfiling,

        -- Hpc opts
	opt_Hpc,

	-- language opts
	opt_DictsStrict,
	opt_IrrefutableTuples,
	opt_Parallel,

	-- optimisation opts
	opt_DsMultiTyVar,
	opt_NoStateHack,
	opt_SpecInlineJoinPoints,
	opt_CprOff,
	opt_SimplNoPreInlining,
	opt_SimplExcessPrecision,
	opt_MaxWorkerArgs,

	-- Unfolding control
	opt_UF_CreationThreshold,
	opt_UF_UseThreshold,
	opt_UF_FunAppDiscount,
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
	opt_OmitBlackHoling,
	opt_Unregisterised,
	opt_EmitExternalCore,
	v_Ld_inputs,
	tablesNextToCode,

    -- For the parser
    addOpt, removeOpt, addWay, findBuildTag, v_opt_C_ready
  ) where

#include "HsVersions.h"

import Config
import FastString
import Util
import Maybes		( firstJust )
import Panic

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
addWay = consIORef v_Ways

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
   = case firstJust (map (maybePrefixMatch sw) staticFlags) of
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
			-- ToDo: hack alert. We should really parse the arugments
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

-- debugging opts
opt_SuppressUniques :: Bool
opt_SuppressUniques		= lookUp  (fsLit "-dsuppress-uniques")
opt_PprStyle_Debug  :: Bool
opt_PprStyle_Debug		= lookUp  (fsLit "-dppr-debug")
opt_PprUserLength   :: Int
opt_PprUserLength	        = lookup_def_int "-dppr-user-length" 5 --ToDo: give this a name
opt_Fuel            :: Int
opt_Fuel                        = lookup_def_int "-dopt-fuel" maxBound
opt_NoDebugOutput   :: Bool
opt_NoDebugOutput               = lookUp  (fsLit "-dno-debug-output")


-- profiling opts
opt_AutoSccsOnAllToplevs :: Bool
opt_AutoSccsOnAllToplevs	= lookUp  (fsLit "-fauto-sccs-on-all-toplevs")
opt_AutoSccsOnExportedToplevs :: Bool
opt_AutoSccsOnExportedToplevs	= lookUp  (fsLit "-fauto-sccs-on-exported-toplevs")
opt_AutoSccsOnIndividualCafs :: Bool
opt_AutoSccsOnIndividualCafs	= lookUp  (fsLit "-fauto-sccs-on-individual-cafs")
opt_SccProfilingOn :: Bool
opt_SccProfilingOn		= lookUp  (fsLit "-fscc-profiling")
opt_DoTickyProfiling :: Bool
opt_DoTickyProfiling            = WayTicky `elem` (unsafePerformIO $ readIORef v_Ways)

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

-- optimisation opts
opt_DsMultiTyVar :: Bool
opt_DsMultiTyVar		= not (lookUp (fsLit "-fno-ds-multi-tyvar"))
	-- On by default

opt_SpecInlineJoinPoints :: Bool
opt_SpecInlineJoinPoints	= lookUp  (fsLit "-fspec-inline-join-points")

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
opt_OmitBlackHoling :: Bool
opt_OmitBlackHoling		= lookUp  (fsLit "-dno-black-holing")

-- Simplifier switches
opt_SimplNoPreInlining :: Bool
opt_SimplNoPreInlining		= lookUp  (fsLit "-fno-pre-inlining")
	-- NoPreInlining is there just to see how bad things
	-- get if you don't do it!
opt_SimplExcessPrecision :: Bool
opt_SimplExcessPrecision	= lookUp  (fsLit "-fexcess-precision")

-- Unfolding control
opt_UF_CreationThreshold :: Int
opt_UF_CreationThreshold	= lookup_def_int "-funfolding-creation-threshold"  (45::Int)
opt_UF_UseThreshold :: Int
opt_UF_UseThreshold		= lookup_def_int "-funfolding-use-threshold"	   (8::Int)	-- Discounts can be big
opt_UF_FunAppDiscount :: Int
opt_UF_FunAppDiscount		= lookup_def_int "-funfolding-fun-discount"	   (6::Int)	-- It's great to inline a fn
opt_UF_KeenessFactor :: Float
opt_UF_KeenessFactor		= lookup_def_float "-funfolding-keeness-factor"	   (1.5::Float)

opt_UF_DearOp :: Int
opt_UF_DearOp   = ( 4 :: Int)


-- Related to linking
opt_PIC :: Bool
#if darwin_TARGET_OS && x86_64_TARGET_ARCH
opt_PIC                         = True
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
-- includes/InfoTables.h.
tablesNextToCode :: Bool
tablesNextToCode 		= not opt_Unregisterised
		 		  && cGhcEnableTablesNextToCode == "YES"

opt_EmitExternalCore :: Bool
opt_EmitExternalCore		= lookUp  (fsLit "-fext-core")

-- Include full span info in error messages, instead of just the start position.
opt_ErrorSpans :: Bool
opt_ErrorSpans			= lookUp (fsLit "-ferror-spans")


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
-- are building - this might be a combination way, eg. profiling+ticky-ticky.

-- We then find the "build-tag" associated with this way, and this
-- becomes the suffix used to find .hi files and libraries used in
-- this compilation.

GLOBAL_VAR(v_Build_tag, "", String)

-- The RTS has its own build tag, because there are some ways that
-- affect the RTS only.
GLOBAL_VAR(v_RTS_Build_tag, "", String)

data WayName
  = WayThreaded
  | WayDebug
  | WayProf
  | WayTicky
  | WayPar
  | WayGran
  | WayNDP
  | WayUser_a
  | WayUser_b
  | WayUser_c
  | WayUser_d
  | WayUser_e
  | WayUser_f
  | WayUser_g
  | WayUser_h
  | WayUser_i
  | WayUser_j
  | WayUser_k
  | WayUser_l
  | WayUser_m
  | WayUser_n
  | WayUser_o
  | WayUser_A
  | WayUser_B
  deriving (Eq,Ord)

GLOBAL_VAR(v_Ways, [] ,[WayName])

allowed_combination :: [WayName] -> Bool
allowed_combination way = and [ x `allowedWith` y 
			      | x <- way, y <- way, x < y ]
  where
	-- Note ordering in these tests: the left argument is
	-- <= the right argument, according to the Ord instance
	-- on Way above.

	-- debug is allowed with everything
	_ `allowedWith` WayDebug		= True
	WayDebug `allowedWith` _		= True

	WayProf `allowedWith` WayNDP		= True
	WayThreaded `allowedWith` WayProf	= True
	_ `allowedWith` _ 			= False


findBuildTag :: IO [String]  -- new options
findBuildTag = do
  way_names <- readIORef v_Ways
  let ws = sort (nub way_names)

  if not (allowed_combination ws)
      then ghcError (CmdLineError $
      		    "combination not supported: "  ++
      		    foldr1 (\a b -> a ++ '/':b) 
      		    (map (wayName . lkupWay) ws))
      else let ways    = map lkupWay ws
      	       tag     = mkBuildTag (filter (not.wayRTSOnly) ways)
      	       rts_tag = mkBuildTag ways
      	       flags   = map wayOpts ways
      	   in do
      	   writeIORef v_Build_tag tag
      	   writeIORef v_RTS_Build_tag rts_tag
      	   return (concat flags)



mkBuildTag :: [Way] -> String
mkBuildTag ways = concat (intersperse "_" (map wayTag ways))

lkupWay :: WayName -> Way
lkupWay w = 
   case lookup w way_details of
	Nothing -> error "findBuildTag"
	Just details -> details

isRTSWay :: WayName -> Bool
isRTSWay = wayRTSOnly . lkupWay 

data Way = Way {
  wayTag     :: String,
  wayRTSOnly :: Bool,
  wayName    :: String,
  wayOpts    :: [String]
  }

way_details :: [ (WayName, Way) ]
way_details =
  [ (WayThreaded, Way "thr" True "Threaded" [
#if defined(freebsd_TARGET_OS)
--	  "-optc-pthread"
--      , "-optl-pthread"
	-- FreeBSD's default threading library is the KSE-based M:N libpthread,
	-- which GHC has some problems with.  It's currently not clear whether
	-- the problems are our fault or theirs, but it seems that using the
	-- alternative 1:1 threading library libthr works around it:
	  "-optl-lthr"
#elif defined(solaris2_TARGET_OS)
          "-optl-lrt"
#endif
	] ),

    (WayDebug, Way "debug" True "Debug" [] ),

    (WayProf, Way  "p" False "Profiling"
	[ "-fscc-profiling"
	, "-DPROFILING"
	, "-optc-DPROFILING" ]),

    (WayTicky, Way  "t" True "Ticky-ticky Profiling"  
	[ "-DTICKY_TICKY"
	, "-optc-DTICKY_TICKY" ]),

    -- optl's below to tell linker where to find the PVM library -- HWL
    (WayPar, Way  "mp" False "Parallel" 
	[ "-fparallel"
	, "-D__PARALLEL_HASKELL__"
	, "-optc-DPAR"
	, "-package concurrent"
        , "-optc-w"
        , "-optl-L${PVM_ROOT}/lib/${PVM_ARCH}"
        , "-optl-lpvm3"
        , "-optl-lgpvm3" ]),

    -- at the moment we only change the RTS and could share compiler and libs!
    (WayPar, Way  "mt" False "Parallel ticky profiling" 
	[ "-fparallel"
	, "-D__PARALLEL_HASKELL__"
	, "-optc-DPAR"
	, "-optc-DPAR_TICKY"
	, "-package concurrent"
        , "-optc-w"
        , "-optl-L${PVM_ROOT}/lib/${PVM_ARCH}"
        , "-optl-lpvm3"
        , "-optl-lgpvm3" ]),

    (WayPar, Way  "md" False "Distributed" 
	[ "-fparallel"
	, "-D__PARALLEL_HASKELL__"
	, "-D__DISTRIBUTED_HASKELL__"
	, "-optc-DPAR"
	, "-optc-DDIST"
	, "-package concurrent"
        , "-optc-w"
        , "-optl-L${PVM_ROOT}/lib/${PVM_ARCH}"
        , "-optl-lpvm3"
        , "-optl-lgpvm3" ]),

    (WayGran, Way  "mg" False "GranSim"
	[ "-fgransim"
	, "-D__GRANSIM__"
	, "-optc-DGRAN"
	, "-package concurrent" ]),

    (WayNDP, Way  "ndp" False "Nested data parallelism"
	[ "-XParr"
	, "-fvectorise"]),

    (WayUser_a,  Way  "a"  False "User way 'a'"  ["$WAY_a_REAL_OPTS"]),	
    (WayUser_b,  Way  "b"  False "User way 'b'"  ["$WAY_b_REAL_OPTS"]),	
    (WayUser_c,  Way  "c"  False "User way 'c'"  ["$WAY_c_REAL_OPTS"]),	
    (WayUser_d,  Way  "d"  False "User way 'd'"  ["$WAY_d_REAL_OPTS"]),	
    (WayUser_e,  Way  "e"  False "User way 'e'"  ["$WAY_e_REAL_OPTS"]),	
    (WayUser_f,  Way  "f"  False "User way 'f'"  ["$WAY_f_REAL_OPTS"]),	
    (WayUser_g,  Way  "g"  False "User way 'g'"  ["$WAY_g_REAL_OPTS"]),	
    (WayUser_h,  Way  "h"  False "User way 'h'"  ["$WAY_h_REAL_OPTS"]),	
    (WayUser_i,  Way  "i"  False "User way 'i'"  ["$WAY_i_REAL_OPTS"]),	
    (WayUser_j,  Way  "j"  False "User way 'j'"  ["$WAY_j_REAL_OPTS"]),	
    (WayUser_k,  Way  "k"  False "User way 'k'"  ["$WAY_k_REAL_OPTS"]),	
    (WayUser_l,  Way  "l"  False "User way 'l'"  ["$WAY_l_REAL_OPTS"]),	
    (WayUser_m,  Way  "m"  False "User way 'm'"  ["$WAY_m_REAL_OPTS"]),	
    (WayUser_n,  Way  "n"  False "User way 'n'"  ["$WAY_n_REAL_OPTS"]),	
    (WayUser_o,  Way  "o"  False "User way 'o'"  ["$WAY_o_REAL_OPTS"]),	
    (WayUser_A,  Way  "A"  False "User way 'A'"  ["$WAY_A_REAL_OPTS"]),	
    (WayUser_B,  Way  "B"  False "User way 'B'"  ["$WAY_B_REAL_OPTS"]) 
  ]

