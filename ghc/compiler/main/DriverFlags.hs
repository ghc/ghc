-----------------------------------------------------------------------------
-- $Id: DriverFlags.hs,v 1.33 2000/12/11 15:26:00 sewardj Exp $
--
-- Driver flags
--
-- (c) Simon Marlow 2000
--
-----------------------------------------------------------------------------

module DriverFlags where

#include "HsVersions.h"

import PackageMaintenance
import DriverState
import DriverUtil
import TmpFiles 	( v_TmpDir )
import CmdLineOpts
import Config
import Util
import TmpFiles	 ( newTempName )
import Directory ( removeFile )
import Exception
import IOExts

import IO
import Maybe
import Monad
import System
import Char

-----------------------------------------------------------------------------
-- Flags

-- Flag parsing is now done in stages:
--
--     * parse the initial list of flags and remove any flags understood
--	 by the driver only.  Determine whether we're in multi-compilation
--	 or single-compilation mode.
--
--     * gather the list of "static" hsc flags, and assign them to the global
--	 static hsc flags variable.
--
--     * build the inital DynFlags from the remaining flags.
--
--     * complain if we've got any flags left over.
--
--     * for each source file: grab the OPTIONS, and build a new DynFlags
--       to pass to the compiler.

-----------------------------------------------------------------------------
-- Process command-line  

data OptKind
	= NoArg (IO ()) 		    -- flag with no argument
	| HasArg (String -> IO ())	    -- flag has an argument (maybe prefix)
	| SepArg (String -> IO ())	    -- flag has a separate argument
	| Prefix (String -> IO ())	    -- flag is a prefix only
	| OptPrefix (String -> IO ())       -- flag may be a prefix
	| AnySuffix (String -> IO ())       -- flag is a prefix, pass whole arg to fn
	| PassFlag  (String -> IO ())       -- flag with no arg, pass flag to fn
	| PrefixPred (String -> Bool) (String -> IO ())
	| AnySuffixPred (String -> Bool) (String -> IO ())

processArgs :: [(String,OptKind)] -> [String] -> [String]
   -> IO [String]  -- returns spare args
processArgs _spec [] spare = return (reverse spare)
processArgs spec args@(('-':arg):args') spare = do
  case findArg spec arg of
    Just (rest,action) -> 
      do args' <- processOneArg action rest args
	 processArgs spec args' spare
    Nothing -> 
      processArgs spec args' (('-':arg):spare)
processArgs spec (arg:args) spare = 
  processArgs spec args (arg:spare)

processOneArg :: OptKind -> String -> [String] -> IO [String]
processOneArg action rest (dash_arg@('-':arg):args) =
  case action of
	NoArg  io -> 
		if rest == ""
			then io >> return args
			else unknownFlagErr dash_arg

	HasArg fio -> 
		if rest /= "" 
			then fio rest >> return args
			else case args of
				[] -> unknownFlagErr dash_arg
				(arg1:args1) -> fio arg1 >> return args1

	SepArg fio -> 
		case args of
			[] -> unknownFlagErr dash_arg
			(arg1:args1) -> fio arg1 >> return args1

	Prefix fio -> 
		if rest /= ""
			then fio rest >> return args
			else unknownFlagErr dash_arg
	
	PrefixPred p fio -> 
		if rest /= ""
			then fio rest >> return args
			else unknownFlagErr dash_arg
	
	OptPrefix fio       -> fio rest >> return args

	AnySuffix fio       -> fio dash_arg >> return args

	AnySuffixPred p fio -> fio dash_arg >> return args

	PassFlag fio  -> 
		if rest /= ""
			then unknownFlagErr dash_arg
			else fio dash_arg >> return args

findArg :: [(String,OptKind)] -> String -> Maybe (String,OptKind)
findArg spec arg
  = case [ (remove_spaces rest, k) 
	 | (pat,k) <- spec, Just rest <- [my_prefix_match pat arg],
	   arg_ok k rest arg ] 
    of
	[]      -> Nothing
	(one:_) -> Just one

arg_ok (NoArg _)            rest arg = null rest
arg_ok (HasArg _)           rest arg = True
arg_ok (SepArg _)           rest arg = null rest
arg_ok (Prefix _)	    rest arg = not (null rest)
arg_ok (PrefixPred p _)     rest arg = not (null rest) && p rest
arg_ok (OptPrefix _)	    rest arg = True
arg_ok (PassFlag _)         rest arg = null rest 
arg_ok (AnySuffix _)        rest arg = True
arg_ok (AnySuffixPred p _)  rest arg = p arg

-----------------------------------------------------------------------------
-- Static flags

-- note that ordering is important in the following list: any flag which
-- is a prefix flag (i.e. HasArg, Prefix, OptPrefix, AnySuffix) will override
-- flags further down the list with the same prefix.

static_flags = 
  [  ------- help -------------------------------------------------------
     ( "?"    		, NoArg long_usage)
  ,  ( "-help"		, NoArg long_usage)
  

      ------- version ----------------------------------------------------
  ,  ( "-version"	 , NoArg (do hPutStrLn stdout (cProjectName
				      ++ ", version " ++ cProjectVersion)
				     exitWith ExitSuccess))
  ,  ( "-numeric-version", NoArg (do hPutStrLn stdout cProjectVersion
				     exitWith ExitSuccess))

      ------- verbosity ----------------------------------------------------
  ,  ( "n"              , NoArg (writeIORef v_Dry_run True) )

	------- recompilation checker --------------------------------------
  ,  ( "recomp"		, NoArg (writeIORef v_Recomp True) )
  ,  ( "no-recomp"  	, NoArg (writeIORef v_Recomp False) )

	------- ways --------------------------------------------------------
  ,  ( "prof"		, NoArg (addNoDups v_Ways	WayProf) )
  ,  ( "unreg"		, NoArg (addNoDups v_Ways	WayUnreg) )
  ,  ( "ticky"		, NoArg (addNoDups v_Ways	WayTicky) )
  ,  ( "parallel"	, NoArg (addNoDups v_Ways	WayPar) )
  ,  ( "gransim"	, NoArg (addNoDups v_Ways	WayGran) )
  ,  ( "smp"		, NoArg (addNoDups v_Ways	WaySMP) )
  ,  ( "debug"		, NoArg (addNoDups v_Ways	WayDebug) )
 	-- ToDo: user ways

	------ Debugging ----------------------------------------------------
  ,  ( "dppr-noprags",     PassFlag (add v_Opt_C) )
  ,  ( "dppr-debug",       PassFlag (add v_Opt_C) )
  ,  ( "dppr-user-length", AnySuffix (add v_Opt_C) )
      -- rest of the debugging flags are dynamic

	------- Interface files ---------------------------------------------
  ,  ( "hi"		, NoArg (writeIORef v_ProduceHi True) )
  ,  ( "nohi"		, NoArg (writeIORef v_ProduceHi False) )

	--------- Profiling --------------------------------------------------
  ,  ( "auto-dicts"	, NoArg (add v_Opt_C "-fauto-sccs-on-dicts") )
  ,  ( "auto-all"	, NoArg (add v_Opt_C "-fauto-sccs-on-all-toplevs") )
  ,  ( "auto"		, NoArg (add v_Opt_C "-fauto-sccs-on-exported-toplevs") )
  ,  ( "caf-all"	, NoArg (add v_Opt_C "-fauto-sccs-on-individual-cafs") )
         -- "ignore-sccs"  doesn't work  (ToDo)

  ,  ( "no-auto-dicts"	, NoArg (add v_Anti_opt_C "-fauto-sccs-on-dicts") )
  ,  ( "no-auto-all"	, NoArg (add v_Anti_opt_C "-fauto-sccs-on-all-toplevs") )
  ,  ( "no-auto"	, NoArg (add v_Anti_opt_C "-fauto-sccs-on-exported-toplevs") )
  ,  ( "no-caf-all"	, NoArg (add v_Anti_opt_C "-fauto-sccs-on-individual-cafs") )

	------- Miscellaneous -----------------------------------------------
  ,  ( "no-link-chk"    , NoArg (return ()) ) -- ignored for backwards compat
  ,  ( "no-hs-main"     , NoArg (writeIORef v_NoHsMain True) )

	------- Output Redirection ------------------------------------------
  ,  ( "odir"		, HasArg (writeIORef v_Output_dir  . Just) )
  ,  ( "o"		, SepArg (writeIORef v_Output_file . Just) )
  ,  ( "osuf"		, HasArg (writeIORef v_Object_suf  . Just) )
  ,  ( "hisuf"		, HasArg (writeIORef v_Hi_suf) )
  ,  ( "tmpdir"		, HasArg (writeIORef v_TmpDir . (++ "/")) )
  ,  ( "ohi"		, HasArg (\s -> case s of 
					  "-" -> writeIORef v_Hi_on_stdout True
					  _   -> writeIORef v_Output_hi (Just s)) )
	-- -odump?

  ,  ( "keep-hc-file"   , AnySuffix (\_ -> writeIORef v_Keep_hc_files True) )
  ,  ( "keep-s-file"    , AnySuffix (\_ -> writeIORef v_Keep_s_files  True) )
  ,  ( "keep-raw-s-file", AnySuffix (\_ -> writeIORef v_Keep_raw_s_files  True) )
  ,  ( "keep-tmp-files" , AnySuffix (\_ -> writeIORef v_Keep_tmp_files True) )

  ,  ( "split-objs"	, NoArg (if can_split
				    then do writeIORef v_Split_object_files True
					    add v_Opt_C "-fglobalise-toplev-names"
				    else hPutStrLn stderr
					    "warning: don't know how to  split \
					    \object files on this architecture"
				) )

	------- Include/Import Paths ----------------------------------------
  ,  ( "i"		, OptPrefix (addToDirList v_Import_paths) )
  ,  ( "I" 		, Prefix    (addToDirList v_Include_paths) )

	------- Libraries ---------------------------------------------------
  ,  ( "L"		, Prefix (addToDirList v_Library_paths) )
  ,  ( "l"		, Prefix (add v_Cmdline_libraries) )

        ------- Packages ----------------------------------------------------
  ,  ( "package-name"   , HasArg (\s -> add v_Opt_C ("-inpackage="++s)) )

  ,  ( "package"        , HasArg (addPackage) )
  ,  ( "syslib"         , HasArg (addPackage) )	-- for compatibility w/ old vsns

  ,  ( "-list-packages"  , NoArg (listPackages) )
  ,  ( "-add-package"    , NoArg (newPackage) )
  ,  ( "-delete-package" , SepArg (deletePackage) )

        ------- Specific phases  --------------------------------------------
  ,  ( "pgmL"           , HasArg (writeIORef v_Pgm_L) )
  ,  ( "pgmP"           , HasArg (writeIORef v_Pgm_P) )
  ,  ( "pgmc"           , HasArg (writeIORef v_Pgm_c) )
  ,  ( "pgmm"           , HasArg (writeIORef v_Pgm_m) )
  ,  ( "pgms"           , HasArg (writeIORef v_Pgm_s) )
  ,  ( "pgma"           , HasArg (writeIORef v_Pgm_a) )
  ,  ( "pgml"           , HasArg (writeIORef v_Pgm_l) )

  ,  ( "optdep"		, HasArg (add v_Opt_dep) )
  ,  ( "optl"		, HasArg (add v_Opt_l) )
  ,  ( "optdll"		, HasArg (add v_Opt_dll) )

	------ Warning opts -------------------------------------------------
  ,  ( "W"		, NoArg (writeIORef v_Warning_opt W_) )
  ,  ( "Wall"		, NoArg (writeIORef v_Warning_opt W_all) )
  ,  ( "Wnot"		, NoArg (writeIORef v_Warning_opt W_not) )
  ,  ( "w"		, NoArg (writeIORef v_Warning_opt W_not) )

	----- Linker --------------------------------------------------------
  ,  ( "static" 	, NoArg (writeIORef v_Static True) )
  ,  ( "rdynamic"       , NoArg (return ()) ) -- ignored for compat w/ gcc

	----- RTS opts ------------------------------------------------------
  ,  ( "H"                 , HasArg (setHeapSize . fromIntegral . decodeSize) )

        ------ Compiler flags -----------------------------------------------
  ,  ( "O2-for-C"	   , NoArg (writeIORef v_minus_o2_for_C True) )
  ,  ( "O"		   , OptPrefix (setOptLevel) )

  ,  ( "fno-asm-mangling"  , NoArg (writeIORef v_Do_asm_mangling False) )

  ,  ( "fmax-simplifier-iterations", 
		Prefix (writeIORef v_MaxSimplifierIterations . read) )

  ,  ( "fusagesp"	   , NoArg (do writeIORef v_UsageSPInf True
				       add v_Opt_C "-fusagesp-on") )

  ,  ( "fexcess-precision" , NoArg (do writeIORef v_Excess_precision True
				       add v_Opt_C "-fexcess-precision"))

	-- Optimisation flags are treated specially, so the normal
	-- -fno-* pattern below doesn't work.  We therefore allow
	-- certain optimisation passes to be turned off explicitly:
  ,  ( "fno-strictness"	   , NoArg (writeIORef v_Strictness False) )
  ,  ( "fno-cpr"	   , NoArg (writeIORef v_CPR False) )
  ,  ( "fno-cse"	   , NoArg (writeIORef v_CSE False) )

	-- flags that are "active negatives"
  ,  ( "fno-prune-tydecls"	, PassFlag (add v_Opt_C) )
  ,  ( "fno-prune-instdecls"	, PassFlag (add v_Opt_C) )
  ,  ( "fno-pre-inlining"	, PassFlag (add v_Opt_C) )

	-- All other "-fno-<blah>" options cancel out "-f<blah>" on the hsc cmdline
  ,  ( "fno-",			PrefixPred (\s -> isStaticHscFlag ("f"++s))
				    (\s -> add v_Anti_opt_C ("-f"++s)) )

	-- Pass all remaining "-f<blah>" options to hsc
  ,  ( "f", 			AnySuffixPred (isStaticHscFlag) (add v_Opt_C) )
  ]

-----------------------------------------------------------------------------
-- parse the dynamic arguments

-- v_InitDynFlags 
--	is the "baseline" dynamic flags, initialised from
-- 	the defaults and command line options.
--
-- v_DynFlags
--	is the dynamic flags for the current compilation.  It is reset
--	to the value of v_InitDynFlags before each compilation, then
--	updated by reading any OPTIONS pragma in the current module.

GLOBAL_VAR(v_InitDynFlags, defaultDynFlags, DynFlags)
GLOBAL_VAR(v_DynFlags,     defaultDynFlags, DynFlags)

updDynFlags f = do
   dfs <- readIORef v_DynFlags
   writeIORef v_DynFlags (f dfs)

getDynFlags :: IO DynFlags
getDynFlags = readIORef v_DynFlags

dynFlag :: (DynFlags -> a) -> IO a
dynFlag f = do dflags <- readIORef v_DynFlags; return (f dflags)

setDynFlag f   = updDynFlags (\dfs -> dfs{ flags = f : flags dfs })
unSetDynFlag f = updDynFlags (\dfs -> dfs{ flags = filter (/= f) (flags dfs) })

-- we can only change HscC to HscAsm and vice-versa with dynamic flags 
-- (-fvia-C and -fasm).
setLang l = do
   dfs <- readIORef v_DynFlags
   case hscLang dfs of
	HscC   -> writeIORef v_DynFlags dfs{ hscLang = l }
	HscAsm -> writeIORef v_DynFlags dfs{ hscLang = l }
	_      -> return ()

setVerbosityAtLeast n =
  updDynFlags (\dfs -> if verbosity dfs < n 
			  then dfs{ verbosity = n }
			  else dfs)

setVerbosity "" = updDynFlags (\dfs -> dfs{ verbosity = 2 })
setVerbosity n 
  | all isDigit n = updDynFlags (\dfs -> dfs{ verbosity = read n })
  | otherwise     = throwDyn (OtherError "can't parse verbosity flag (-v<n>)")

getVerbFlag = do
   verb <- dynFlag verbosity
   if verb >= 3  then return  "-v" else return ""

dynamic_flags = [

     ( "cpp",		NoArg  (updateState (\s -> s{ cpp_flag = True })) )
  ,  ( "#include",	HasArg (addCmdlineHCInclude) )

  ,  ( "v",		OptPrefix (setVerbosity) )

  ,  ( "optL",		HasArg (addOpt_L) )
  ,  ( "optP",		HasArg (addOpt_P) )
  ,  ( "optc",		HasArg (addOpt_c) )
  ,  ( "optm",		HasArg (addOpt_m) )
  ,  ( "opta",		HasArg (addOpt_a) )

	------ HsCpp opts ---------------------------------------------------
  ,  ( "D",		Prefix (\s -> addOpt_P ("-D'"++s++"'") ) )
  ,  ( "U",		Prefix (\s -> addOpt_P ("-U'"++s++"'") ) )

	------ Debugging ----------------------------------------------------
  ,  ( "dstg-stats",	NoArg (writeIORef v_StgStats True) )

  ,  ( "ddump-absC",         	 NoArg (setDynFlag Opt_D_dump_absC) )
  ,  ( "ddump-asm",          	 NoArg (setDynFlag Opt_D_dump_asm) )
  ,  ( "ddump-cpranal",      	 NoArg (setDynFlag Opt_D_dump_cpranal) )
  ,  ( "ddump-deriv",        	 NoArg (setDynFlag Opt_D_dump_deriv) )
  ,  ( "ddump-ds",           	 NoArg (setDynFlag Opt_D_dump_ds) )
  ,  ( "ddump-flatC",        	 NoArg (setDynFlag Opt_D_dump_flatC) )
  ,  ( "ddump-foreign",      	 NoArg (setDynFlag Opt_D_dump_foreign) )
  ,  ( "ddump-inlinings",    	 NoArg (setDynFlag Opt_D_dump_inlinings) )
  ,  ( "ddump-occur-anal",   	 NoArg (setDynFlag Opt_D_dump_occur_anal) )
  ,  ( "ddump-parsed",       	 NoArg (setDynFlag Opt_D_dump_parsed) )
  ,  ( "ddump-realC",        	 NoArg (setDynFlag Opt_D_dump_realC) )
  ,  ( "ddump-rn",           	 NoArg (setDynFlag Opt_D_dump_rn) )
  ,  ( "ddump-simpl",        	 NoArg (setDynFlag Opt_D_dump_simpl) )
  ,  ( "ddump-simpl-iterations", NoArg (setDynFlag Opt_D_dump_simpl_iterations) )
  ,  ( "ddump-spec",         	 NoArg (setDynFlag Opt_D_dump_spec) )
  ,  ( "ddump-sat",          	 NoArg (setDynFlag Opt_D_dump_sat) )
  ,  ( "ddump-stg",          	 NoArg (setDynFlag Opt_D_dump_stg) )
  ,  ( "ddump-stranal",      	 NoArg (setDynFlag Opt_D_dump_stranal) )
  ,  ( "ddump-tc",           	 NoArg (setDynFlag Opt_D_dump_tc) )
  ,  ( "ddump-types",        	 NoArg (setDynFlag Opt_D_dump_types) )
  ,  ( "ddump-rules",        	 NoArg (setDynFlag Opt_D_dump_rules) )
  ,  ( "ddump-usagesp",      	 NoArg (setDynFlag Opt_D_dump_usagesp) )
  ,  ( "ddump-cse",          	 NoArg (setDynFlag Opt_D_dump_cse) )
  ,  ( "ddump-worker-wrapper",   NoArg (setDynFlag Opt_D_dump_worker_wrapper) )
  ,  ( "dshow-passes",           NoArg (setVerbosity "2") )
  ,  ( "ddump-rn-trace",         NoArg (setDynFlag Opt_D_dump_rn_trace) )
  ,  ( "ddump-rn-stats",         NoArg (setDynFlag Opt_D_dump_rn_stats) )
  ,  ( "ddump-stix",             NoArg (setDynFlag Opt_D_dump_stix) )
  ,  ( "ddump-simpl-stats",      NoArg (setDynFlag Opt_D_dump_simpl_stats) )
  ,  ( "ddump-interpsyn",        NoArg (setDynFlag Opt_D_dump_InterpSyn) )
  ,  ( "dsource-stats",          NoArg (setDynFlag Opt_D_source_stats) )
  ,  ( "dverbose-core2core",     NoArg (setDynFlag Opt_D_verbose_core2core) )
  ,  ( "dverbose-stg2stg",       NoArg (setDynFlag Opt_D_verbose_stg2stg) )
  ,  ( "ddump-hi-diffs",         NoArg (setDynFlag Opt_D_dump_hi_diffs) )
  ,  ( "ddump-hi",               NoArg (setDynFlag Opt_D_dump_hi) )
  ,  ( "ddump-minimal-imports",  NoArg (setDynFlag Opt_D_dump_minimal_imports) )
  ,  ( "dcore-lint",       	 NoArg (setDynFlag Opt_DoCoreLinting) )
  ,  ( "dstg-lint",        	 NoArg (setDynFlag Opt_DoStgLinting) )
  ,  ( "dusagesp-lint",        	 NoArg (setDynFlag Opt_DoUSPLinting) )

	------ Machine dependant (-m<blah>) stuff ---------------------------

  ,  ( "monly-2-regs", 	NoArg (updateState (\s -> s{stolen_x86_regs = 2}) ))
  ,  ( "monly-3-regs", 	NoArg (updateState (\s -> s{stolen_x86_regs = 3}) ))
  ,  ( "monly-4-regs", 	NoArg (updateState (\s -> s{stolen_x86_regs = 4}) ))

        ------ Compiler flags -----------------------------------------------

  ,  ( "fasm",		AnySuffix (\_ -> setLang HscAsm) )
  ,  ( "fvia-c",	NoArg (setLang HscC) )
  ,  ( "fvia-C",	NoArg (setLang HscC) )

	-- "active negatives"
  ,  ( "fno-implicit-prelude",  NoArg (setDynFlag Opt_NoImplicitPrelude) )

	-- the rest of the -f* and -fno-* flags
  ,  ( "fno-", 		PrefixPred (\f -> isFFlag f) (\f -> unSetDynFlag (getFFlag f)) )
  ,  ( "f",		PrefixPred (\f -> isFFlag f) (\f -> setDynFlag (getFFlag f)) )
 ]

-- these -f<blah> flags can all be reversed with -fno-<blah>

fFlags = [
  ( "warn-duplicate-exports",    	Opt_WarnDuplicateExports ),
  ( "warn-hi-shadowing",         	Opt_WarnHiShadows ),
  ( "warn-incomplete-patterns",  	Opt_WarnIncompletePatterns ),
  ( "warn-missing-fields",       	Opt_WarnMissingFields ),
  ( "warn-missing-methods",      	Opt_WarnMissingMethods ),
  ( "warn-missing-signatures",   	Opt_WarnMissingSigs ),
  ( "warn-name-shadowing",       	Opt_WarnNameShadowing ),
  ( "warn-overlapping-patterns", 	Opt_WarnOverlappingPatterns ),
  ( "warn-simple-patterns",      	Opt_WarnSimplePatterns ),
  ( "warn-type-defaults",        	Opt_WarnTypeDefaults ),
  ( "warn-unused-binds",         	Opt_WarnUnusedBinds ),
  ( "warn-unused-imports",       	Opt_WarnUnusedImports ),
  ( "warn-unused-matches",       	Opt_WarnUnusedMatches ),
  ( "warn-deprecations",         	Opt_WarnDeprecations ),
  ( "glasgow-exts", 		 	Opt_GlasgowExts ),
  ( "allow-overlapping-instances", 	Opt_AllowOverlappingInstances ),
  ( "allow-undecidable-instances", 	Opt_AllowUndecidableInstances ),
  ( "fgenerics",  			Opt_Generics ),
  ( "report-compile", 		 	Opt_ReportCompile )
  ]

isFFlag f = f `elem` (map fst fFlags)
getFFlag f = fromJust (lookup f fFlags)

-----------------------------------------------------------------------------
-- convert sizes like "3.5M" into integers

decodeSize :: String -> Integer
decodeSize str
  | c == ""		 = truncate n
  | c == "K" || c == "k" = truncate (n * 1000)
  | c == "M" || c == "m" = truncate (n * 1000 * 1000)
  | c == "G" || c == "g" = truncate (n * 1000 * 1000 * 1000)
  | otherwise            = throwDyn (OtherError ("can't decode size: " ++ str))
  where (m, c) = span pred str
        n      = read m  :: Double
	pred c = isDigit c || c == '.'

floatOpt :: IORef Double -> String -> IO ()
floatOpt ref str
  = writeIORef ref (read str :: Double)

#if __GLASGOW_HASKELL__ >= 411
foreign import "setHeapSize" unsafe setHeapSize :: Int -> IO ()
#else
setHeapSize :: Int -> IO ()		-- -H<size> is ignored
setHeapSize _ = return ()
#endif

-----------------------------------------------------------------------------
-- Build the Hsc static command line opts

buildStaticHscOpts :: IO [String]
buildStaticHscOpts = do

  opt_C_ <- getStaticOpts v_Opt_C	-- misc hsc opts from the command line

	-- optimisation
  minus_o <- readIORef v_OptLevel
  let optimisation_opts = 
        case minus_o of
	    0 -> hsc_minusNoO_flags
	    1 -> hsc_minusO_flags
	    2 -> hsc_minusO2_flags
	    _ -> error "unknown opt level"
	    -- ToDo: -Ofile
 
  let stg_opts = [ "-flet-no-escape" ]
	-- let-no-escape always on for now

	-- take into account -fno-* flags by removing the equivalent -f*
	-- flag from our list.
  anti_flags <- getStaticOpts v_Anti_opt_C
  let basic_opts = opt_C_ ++ optimisation_opts ++ stg_opts
      filtered_opts = filter (`notElem` anti_flags) basic_opts

  static <- (do s <- readIORef v_Static; if s then return "-static" 
					      else return "")

  return ( static : filtered_opts )

-----------------------------------------------------------------------------
-- Running an external program

-- sigh, here because both DriverMkDepend & DriverPipeline need it.

runSomething phase_name cmd
 = do
   verb <- dynFlag verbosity
   when (verb >= 2) $ putStrLn ("*** " ++ phase_name)
   when (verb >= 3) $ putStrLn cmd
   hFlush stdout

   -- test for -n flag
   n <- readIORef v_Dry_run
   unless n $ do 

   -- and run it!
   exit_code <- kludgedSystem cmd phase_name

   if exit_code /= ExitSuccess
	then throwDyn (PhaseFailed phase_name exit_code)
	else do when (verb >= 3) (putStr "\n")
	        return ()
