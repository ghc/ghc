-----------------------------------------------------------------------------
--
-- Settings for the driver
--
-- (c) The University of Glasgow 2002
--
-----------------------------------------------------------------------------

module DriverState where

#include "HsVersions.h"

import CmdLineOpts
import DriverPhases
import DriverUtil
import Util
import Config
import Panic

import DATA_IOREF	( IORef, readIORef, writeIORef )
import EXCEPTION

import List
import Char  
import Monad
import Maybe		( fromJust, isJust )
import Directory	( doesDirectoryExist )

-----------------------------------------------------------------------------
-- non-configured things

cHaskell1Version = "5" -- i.e., Haskell 98

-----------------------------------------------------------------------------
-- GHC modes of operation

data GhcMode
  = DoMkDependHS			-- ghc -M
  | StopBefore Phase			-- ghc -E | -C | -S
					-- StopBefore StopLn is the default
  | DoMake				-- ghc --make
  | DoInteractive			-- ghc --interactive
  | DoEval String			-- ghc -e
  deriving (Show)

data GhcLink	-- What to do in the link step 
  =		-- Only relevant for modes
		-- 	DoMake and StopBefore StopLn
    NoLink		-- Don't link at all
  | StaticLink		-- Ordinary linker [the default]
  | MkDLL		-- Make a DLL

GLOBAL_VAR(v_GhcMode,     StopBefore StopLn, 	GhcMode)
GLOBAL_VAR(v_GhcModeFlag, "",     		String)
GLOBAL_VAR(v_GhcLink, 	  StaticLink,		GhcLink)

setMode :: GhcMode -> String -> IO ()
setMode m flag = do
  old_mode <- readIORef v_GhcMode
  old_flag <- readIORef v_GhcModeFlag
  when (notNull old_flag && flag /= old_flag) $
      throwDyn (UsageError 
          ("cannot use `" ++ old_flag ++ "' with `" ++ flag ++ "'"))
  writeIORef v_GhcMode m
  writeIORef v_GhcModeFlag flag

isInteractiveMode, isInterpretiveMode     :: GhcMode -> Bool
isMakeMode, isLinkMode, isCompManagerMode :: GhcMode -> Bool

isInteractiveMode DoInteractive = True
isInteractiveMode _		= False

-- isInterpretiveMode: byte-code compiler involved
isInterpretiveMode DoInteractive = True
isInterpretiveMode (DoEval _)    = True
isInterpretiveMode _             = False

isMakeMode DoMake = True
isMakeMode _      = False

-- True if we are going to attempt to link in this mode.
-- (we might not actually link, depending on the GhcLink flag)
isLinkMode (StopBefore StopLn) = True
isLinkMode DoMake	       = True
isLinkMode _   		       = False

isCompManagerMode DoMake        = True
isCompManagerMode DoInteractive = True
isCompManagerMode (DoEval _)    = True
isCompManagerMode _             = False

isNoLink :: GhcLink -> Bool
isNoLink NoLink = True
isNoLink other  = False

-----------------------------------------------------------------------------
-- Global compilation flags

-- Default CPP defines in Haskell source
hsSourceCppOpts =
	[ "-D__HASKELL1__="++cHaskell1Version
	, "-D__GLASGOW_HASKELL__="++cProjectVersionInt				
	, "-D__HASKELL98__"
	, "-D__CONCURRENT_HASKELL__"
	]


-- Keep output from intermediate phases
GLOBAL_VAR(v_Keep_hi_diffs, 		False, 		Bool)
GLOBAL_VAR(v_Keep_hc_files,		False,		Bool)
GLOBAL_VAR(v_Keep_s_files,		False,		Bool)
GLOBAL_VAR(v_Keep_raw_s_files,		False,		Bool)
GLOBAL_VAR(v_Keep_tmp_files, 		False, 		Bool)
#ifdef ILX
GLOBAL_VAR(v_Keep_il_files,		False,		Bool)
GLOBAL_VAR(v_Keep_ilx_files,		False,		Bool)
#endif

-- Misc
GLOBAL_VAR(v_Scale_sizes_by,    	1.0,		Double)
GLOBAL_VAR(v_Static, 			True,		Bool)
GLOBAL_VAR(v_NoHsMain, 			False, 		Bool)
GLOBAL_VAR(v_MainModIs,			Nothing,	Maybe String)
GLOBAL_VAR(v_MainFunIs,			Nothing,	Maybe String)
GLOBAL_VAR(v_Collect_ghc_timing, 	False,		Bool)
GLOBAL_VAR(v_Do_asm_mangling,		True,		Bool)
GLOBAL_VAR(v_Excess_precision,		False,		Bool)
GLOBAL_VAR(v_Read_DotGHCi,		True,		Bool)

-- Preprocessor flags
GLOBAL_VAR(v_Hs_source_pp_opts, [], [String])

-----------------------------------------------------------------------------
-- Splitting object files (for libraries)

GLOBAL_VAR(v_Split_object_files,	False,		Bool)
GLOBAL_VAR(v_Split_info,		("",0),		(String,Int))
	-- The split prefix and number of files

	
can_split :: Bool
can_split =  
#if    defined(i386_TARGET_ARCH)     \
    || defined(alpha_TARGET_ARCH)    \
    || defined(hppa_TARGET_ARCH)     \
    || defined(m68k_TARGET_ARCH)     \
    || defined(mips_TARGET_ARCH)     \
    || defined(powerpc_TARGET_ARCH)  \
    || defined(rs6000_TARGET_ARCH)   \
    || defined(sparc_TARGET_ARCH) 
   True
#else
   False
#endif

-----------------------------------------------------------------------------
-- Compiler output options

GLOBAL_VAR(v_Output_dir,  Nothing, Maybe String)
GLOBAL_VAR(v_Output_file, Nothing, Maybe String)
GLOBAL_VAR(v_Output_hi,   Nothing, Maybe String)

-- called to verify that the output files & directories
-- point somewhere valid. 
--
-- The assumption is that the directory portion of these output
-- options will have to exist by the time 'verifyOutputFiles'
-- is invoked.
-- 
verifyOutputFiles :: IO ()
verifyOutputFiles = do
  odir <- readIORef v_Output_dir
  when (isJust odir) $ do
     let dir = fromJust odir
     flg <- doesDirectoryExist dir
     when (not flg) (nonExistentDir "-odir" dir)
  ofile <- readIORef v_Output_file
  when (isJust ofile) $ do
     let fn = fromJust ofile
     flg <- doesDirNameExist fn
     when (not flg) (nonExistentDir "-o" fn)
  ohi <- readIORef v_Output_hi
  when (isJust ohi) $ do
     let hi = fromJust ohi
     flg <- doesDirNameExist hi
     when (not flg) (nonExistentDir "-ohi" hi)
 where
   nonExistentDir flg dir = 
     throwDyn (CmdLineError ("error: directory portion of " ++ 
                             show dir ++ " does not exist (used with " ++ 
			     show flg ++ " option.)"))

GLOBAL_VAR(v_Object_suf,  phaseInputExt StopLn, String)
GLOBAL_VAR(v_HC_suf,  	  phaseInputExt HCc,    String)
GLOBAL_VAR(v_Hi_dir,      Nothing, Maybe String)
GLOBAL_VAR(v_Hi_suf,      "hi",	   String)

GLOBAL_VAR(v_Ld_inputs,	[],      [String])

odir_ify :: String -> IO String
odir_ify f = do
  odir_opt <- readIORef v_Output_dir
  case odir_opt of
	Nothing -> return f
	Just d  -> return (replaceFilenameDirectory f d)

osuf_ify :: String -> IO String
osuf_ify f = do
  osuf <- readIORef v_Object_suf
  return (replaceFilenameSuffix f osuf)

GLOBAL_VAR(v_StgStats,                  False, Bool)

buildStgToDo :: IO [ StgToDo ]
buildStgToDo = do
  stg_stats <- readIORef v_StgStats
  let flags1 | stg_stats = [ D_stg_stats ]
	     | otherwise = [ ]

	-- STG passes
  ways_ <- readIORef v_Ways
  let flags2 | WayProf `elem` ways_ = StgDoMassageForProfiling : flags1
	     | otherwise            = flags1

  return flags2

-----------------------------------------------------------------------------
-- Paths & Libraries

split_marker = ':'   -- not configurable (ToDo)

v_Include_paths, v_Library_paths :: IORef [String]
GLOBAL_VAR(v_Include_paths, [], [String])
GLOBAL_VAR(v_Library_paths, [],	 [String])

#ifdef darwin_TARGET_OS
GLOBAL_VAR(v_Framework_paths, [], [String])
GLOBAL_VAR(v_Cmdline_frameworks, [], [String])
#endif

addToDirList :: IORef [String] -> String -> IO ()
addToDirList ref path
  = do paths           <- readIORef ref
       shiny_new_ones  <- splitPathList path
       writeIORef ref (paths ++ shiny_new_ones)


splitPathList :: String -> IO [String]
splitPathList s = do ps <- splitUp s; return (filter notNull ps)
		-- empty paths are ignored: there might be a trailing
		-- ':' in the initial list, for example.  Empty paths can
		-- cause confusion when they are translated into -I options
		-- for passing to gcc.
  where
#ifdef mingw32_TARGET_OS
     -- 'hybrid' support for DOS-style paths in directory lists.
     -- 
     -- That is, if "foo:bar:baz" is used, this interpreted as
     -- consisting of three entries, 'foo', 'bar', 'baz'.
     -- However, with "c:/foo:c:\\foo;x:/bar", this is interpreted
     -- as four elts, "c:/foo", "c:\\foo", "x", and "/bar" --
     -- *provided* c:/foo exists and x:/bar doesn't.
     --
     -- Notice that no attempt is made to fully replace the 'standard'
     -- split marker ':' with the Windows / DOS one, ';'. The reason being
     -- that this will cause too much breakage for users & ':' will
     -- work fine even with DOS paths, if you're not insisting on being silly.
     -- So, use either.
    splitUp []         = return []
    splitUp (x:':':div:xs) 
      | div `elem` dir_markers = do
          let (p,rs) = findNextPath xs
          ps  <- splitUp rs
           {-
             Consult the file system to check the interpretation
             of (x:':':div:p) -- this is arguably excessive, we
             could skip this test & just say that it is a valid
             dir path.
           -}
          flg <- doesDirectoryExist (x:':':div:p)
          if flg then
             return ((x:':':div:p):ps)
           else
             return ([x]:(div:p):ps)
    splitUp xs = do
      let (p,rs) = findNextPath xs
      ps <- splitUp rs
      return (cons p ps)
    
    cons "" xs = xs
    cons x  xs = x:xs

    -- will be called either when we've consumed nought or the "<Drive>:/" part of
    -- a DOS path, so splitting is just a Q of finding the next split marker.
    findNextPath xs = 
        case break (`elem` split_markers) xs of
	   (p, d:ds) -> (p, ds)
	   (p, xs)   -> (p, xs)

    split_markers :: [Char]
    split_markers = [':', ';']

    dir_markers :: [Char]
    dir_markers = ['/', '\\']

#else
    splitUp xs = return (split split_marker xs)
#endif

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
  | WayUnreg
  | WayTicky
  | WayPar
  | WayGran
  | WaySMP
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

allowed_combination way = and [ x `allowedWith` y 
			      | x <- way, y <- way, x < y ]
  where
	-- Note ordering in these tests: the left argument is
	-- <= the right argument, according to the Ord instance
	-- on Way above.

	-- debug is allowed with everything
	_ `allowedWith` WayDebug		= True
	WayDebug `allowedWith` _		= True

	WayThreaded `allowedWith` WayProf	= True
	WayProf `allowedWith` WayUnreg		= True
	WayProf `allowedWith` WaySMP		= True
	WayProf `allowedWith` WayNDP		= True
	_ `allowedWith` _ 			= False


findBuildTag :: IO [String]  -- new options
findBuildTag = do
  way_names <- readIORef v_Ways
  let ws = sort way_names
  if not (allowed_combination ws)
      then throwDyn (CmdLineError $
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

lkupWay w = 
   case lookup w way_details of
	Nothing -> error "findBuildTag"
	Just details -> details

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
	  "-optc-pthread"
        , "-optl-pthread"
#endif
	] ),

    (WayDebug, Way "debug" True "Debug" [] ),

    (WayProf, Way  "p" False "Profiling"
	[ "-fscc-profiling"
	, "-DPROFILING"
	, "-optc-DPROFILING"
	, "-fvia-C" ]),

    (WayTicky, Way  "t" False "Ticky-ticky Profiling"  
	[ "-fticky-ticky"
	, "-DTICKY_TICKY"
	, "-optc-DTICKY_TICKY"
	, "-fvia-C" ]),

    (WayUnreg, Way  "u" False "Unregisterised" 
	unregFlags ),

    -- optl's below to tell linker where to find the PVM library -- HWL
    (WayPar, Way  "mp" False "Parallel" 
	[ "-fparallel"
	, "-D__PARALLEL_HASKELL__"
	, "-optc-DPAR"
	, "-package concurrent"
        , "-optc-w"
        , "-optl-L${PVM_ROOT}/lib/${PVM_ARCH}"
        , "-optl-lpvm3"
        , "-optl-lgpvm3"
	, "-fvia-C" ]),

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
        , "-optl-lgpvm3"
	, "-fvia-C" ]),

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
        , "-optl-lgpvm3"
	, "-fvia-C" ]),

    (WayGran, Way  "mg" False "GranSim"
	[ "-fgransim"
	, "-D__GRANSIM__"
	, "-optc-DGRAN"
	, "-package concurrent"
	, "-fvia-C" ]),

    (WaySMP, Way  "s" False "SMP"
	[ "-fsmp"
	, "-optc-pthread"
#ifndef freebsd_TARGET_OS
	, "-optl-pthread"
#endif
	, "-optc-DSMP"
	, "-fvia-C" ]),

    (WayNDP, Way  "ndp" False "Nested data parallelism"
	[ "-fparr"
	, "-fflatten"]),

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

unregFlags = 
   [ "-optc-DNO_REGS"
   , "-optc-DUSE_MINIINTERPRETER"
   , "-fno-asm-mangling"
   , "-funregisterised"
   , "-fvia-C" ]

-----------------------------------------------------------------------------
-- Options for particular phases

GLOBAL_VAR(v_Opt_dep,    [], [String])
GLOBAL_VAR(v_Anti_opt_C, [], [String])
GLOBAL_VAR(v_Opt_C,      [], [String])
GLOBAL_VAR(v_Opt_l,      [], [String])
GLOBAL_VAR(v_Opt_dll,    [], [String])

getStaticOpts :: IORef [String] -> IO [String]
getStaticOpts ref = readIORef ref >>= return . reverse
