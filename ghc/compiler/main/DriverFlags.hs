-----------------------------------------------------------------------------
-- $Id: DriverFlags.hs,v 1.1 2000/10/11 11:54:58 simonmar Exp $
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
import CmdLineOpts
import Config
import Util
import CmdLineOpts

import Exception
import IOExts
import IO
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
processArgs spec args@(arg@('-':_):args') spare = do
  case findArg spec arg of
    Just (rest,action) -> 
      do args' <- processOneArg action rest args
	 processArgs spec args' spare
    Nothing -> 
      processArgs spec args' (arg:spare)
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
	   arg_ok k arg rest ] 
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
arg_ok (AnySuffix _)        rest arg = not (null rest)
arg_ok (AnySuffixPred p _)  rest arg = not (null rest) && p arg

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
				      ++ ", version " ++ version_str)
				     exitWith ExitSuccess))
  ,  ( "-numeric-version", NoArg (do hPutStrLn stdout version_str
				     exitWith ExitSuccess))

      ------- verbosity ----------------------------------------------------
  ,  ( "v"		, NoArg (writeIORef verbose True) )
  ,  ( "n"              , NoArg (writeIORef dry_run True) )

	------- recompilation checker --------------------------------------
  ,  ( "recomp"		, NoArg (writeIORef recomp True) )
  ,  ( "no-recomp"  	, NoArg (writeIORef recomp False) )

	------- ways --------------------------------------------------------
  ,  ( "prof"		, NoArg (addNoDups ways	WayProf) )
  ,  ( "unreg"		, NoArg (addNoDups ways	WayUnreg) )
  ,  ( "dll"            , NoArg (addNoDups ways WayDll) )
  ,  ( "ticky"		, NoArg (addNoDups ways	WayTicky) )
  ,  ( "parallel"	, NoArg (addNoDups ways	WayPar) )
  ,  ( "gransim"	, NoArg (addNoDups ways	WayGran) )
  ,  ( "smp"		, NoArg (addNoDups ways	WaySMP) )
  ,  ( "debug"		, NoArg (addNoDups ways	WayDebug) )
 	-- ToDo: user ways

	------ Debugging ----------------------------------------------------
  ,  ( "dppr-noprags",     PassFlag (add opt_C) )
  ,  ( "dppr-debug",       PassFlag (add opt_C) )
  ,  ( "dppr-user-length", AnySuffix (add opt_C) )
      -- rest of the debugging flags are dynamic

	------- Interface files ---------------------------------------------
  ,  ( "hi"		, NoArg (writeIORef produceHi True) )
  ,  ( "nohi"		, NoArg (writeIORef produceHi False) )

	--------- Profiling --------------------------------------------------
  ,  ( "auto-dicts"	, NoArg (add opt_C "-fauto-sccs-on-dicts") )
  ,  ( "auto-all"	, NoArg (add opt_C "-fauto-sccs-on-all-toplevs") )
  ,  ( "auto"		, NoArg (add opt_C "-fauto-sccs-on-exported-toplevs") )
  ,  ( "caf-all"	, NoArg (add opt_C "-fauto-sccs-on-individual-cafs") )
         -- "ignore-sccs"  doesn't work  (ToDo)

  ,  ( "no-auto-dicts"	, NoArg (add anti_opt_C "-fauto-sccs-on-dicts") )
  ,  ( "no-auto-all"	, NoArg (add anti_opt_C "-fauto-sccs-on-all-toplevs") )
  ,  ( "no-auto"	, NoArg (add anti_opt_C "-fauto-sccs-on-exported-toplevs") )
  ,  ( "no-caf-all"	, NoArg (add anti_opt_C "-fauto-sccs-on-individual-cafs") )

	------- Miscellaneous -----------------------------------------------
  ,  ( "no-link-chk"    , NoArg (return ()) ) -- ignored for backwards compat

	------- Output Redirection ------------------------------------------
  ,  ( "odir"		, HasArg (writeIORef output_dir  . Just) )
  ,  ( "o"		, SepArg (writeIORef output_file . Just) )
  ,  ( "osuf"		, HasArg (writeIORef output_suf  . Just) )
  ,  ( "hisuf"		, HasArg (writeIORef hi_suf) )
  ,  ( "tmpdir"		, HasArg (writeIORef v_TmpDir . (++ "/")) )
  ,  ( "ohi"		, HasArg (\s -> case s of 
					  "-" -> writeIORef hi_on_stdout True
					  _   -> writeIORef output_hi (Just s)) )
	-- -odump?

  ,  ( "keep-hc-file"   , AnySuffix (\_ -> writeIORef keep_hc_files True) )
  ,  ( "keep-s-file"    , AnySuffix (\_ -> writeIORef keep_s_files  True) )
  ,  ( "keep-raw-s-file", AnySuffix (\_ -> writeIORef keep_raw_s_files  True) )
  ,  ( "keep-tmp-files" , AnySuffix (\_ -> writeIORef keep_tmp_files True) )

  ,  ( "split-objs"	, NoArg (if can_split
				    then do writeIORef split_object_files True
					    add opt_C "-fglobalise-toplev-names"
-- TODO!!!!!				    add opt_c "-DUSE_SPLIT_MARKERS"
				    else hPutStrLn stderr
					    "warning: don't know how to  split \
					    \object files on this architecture"
				) )
  
	------- Include/Import Paths ----------------------------------------
  ,  ( "i"		, OptPrefix (addToDirList import_paths) )
  ,  ( "I" 		, Prefix    (addToDirList include_paths) )

	------- Libraries ---------------------------------------------------
  ,  ( "L"		, Prefix (addToDirList library_paths) )
  ,  ( "l"		, Prefix (add cmdline_libraries) )

        ------- Packages ----------------------------------------------------
  ,  ( "package-name"   , HasArg (\s -> add opt_C ("-inpackage="++s)) )

  ,  ( "package"        , HasArg (addPackage) )
  ,  ( "syslib"         , HasArg (addPackage) )	-- for compatibility w/ old vsns

  ,  ( "-list-packages"  , NoArg (listPackages) )
  ,  ( "-add-package"    , NoArg (newPackage) )
  ,  ( "-delete-package" , SepArg (deletePackage) )

        ------- Specific phases  --------------------------------------------
  ,  ( "pgmL"           , HasArg (writeIORef pgm_L) )
  ,  ( "pgmP"           , HasArg (writeIORef pgm_P) )
  ,  ( "pgmC"           , HasArg (writeIORef pgm_C) )
  ,  ( "pgmc"           , HasArg (writeIORef pgm_c) )
  ,  ( "pgmm"           , HasArg (writeIORef pgm_m) )
  ,  ( "pgms"           , HasArg (writeIORef pgm_s) )
  ,  ( "pgma"           , HasArg (writeIORef pgm_a) )
  ,  ( "pgml"           , HasArg (writeIORef pgm_l) )

  ,  ( "optdep"		, HasArg (add opt_dep) )
  ,  ( "optl"		, HasArg (add opt_l) )
  ,  ( "optdll"		, HasArg (add opt_dll) )

	------ Warning opts -------------------------------------------------
  ,  ( "W"		, NoArg (writeIORef warning_opt W_) )
  ,  ( "Wall"		, NoArg (writeIORef warning_opt	W_all) )
  ,  ( "Wnot"		, NoArg (writeIORef warning_opt	W_not) )
  ,  ( "w"		, NoArg (writeIORef warning_opt	W_not) )

	----- Linker --------------------------------------------------------
  ,  ( "static" 	, NoArg (writeIORef static True) )

        ------ Compiler flags -----------------------------------------------
  ,  ( "O2-for-C"	   , NoArg (writeIORef opt_minus_o2_for_C True) )
  ,  ( "O"		   , OptPrefix (setOptLevel) )

  ,  ( "fasm"		   , OptPrefix (\_ -> writeIORef hsc_lang HscAsm) )

  ,  ( "fvia-c"		   , NoArg (writeIORef hsc_lang HscC) )
  ,  ( "fvia-C"		   , NoArg (writeIORef hsc_lang HscC) )

  ,  ( "fno-asm-mangling"  , NoArg (writeIORef do_asm_mangling False) )

  ,  ( "fmax-simplifier-iterations", 
		Prefix (writeIORef opt_MaxSimplifierIterations . read) )

  ,  ( "fusagesp"	   , NoArg (do writeIORef opt_UsageSPInf True
				       add opt_C "-fusagesp-on") )

  ,  ( "fexcess-precision" , NoArg (do writeIORef excess_precision True
				       add opt_C "-fexcess-precision"))

	-- flags that are "active negatives"
  ,  ( "fno-implicit-prelude"	, PassFlag (add opt_C) )
  ,  ( "fno-prune-tydecls"	, PassFlag (add opt_C) )
  ,  ( "fno-prune-instdecls"	, PassFlag (add opt_C) )
  ,  ( "fno-pre-inlining"	, PassFlag (add opt_C) )

	-- All other "-fno-<blah>" options cancel out "-f<blah>" on the hsc cmdline
  ,  ( "fno-",			PrefixPred (\s -> isStaticHscFlag ("f"++s))
				    (\s -> add anti_opt_C ("-f"++s)) )

	-- Pass all remaining "-f<blah>" options to hsc
  ,  ( "f", 			AnySuffixPred (isStaticHscFlag) (add opt_C) )
  ]

-----------------------------------------------------------------------------
-- parse the dynamic arguments

GLOBAL_VAR(dynFlags, error "no dynFlags", DynFlags)

setDynFlag f = do
   dfs <- readIORef dynFlags
   writeIORef dynFlags dfs{ flags = f : flags dfs }

unSetDynFlag f = do
   dfs <- readIORef dynFlags
   writeIORef dynFlags dfs{ flags = filter (/= f) (flags dfs) }

dynamic_flags = [

     ( "cpp",		NoArg  (updateState (\s -> s{ cpp_flag = True })) )
  ,  ( "#include",	HasArg (addCmdlineHCInclude) )

  ,  ( "optL",		HasArg (addOpt_L) )
  ,  ( "optP",		HasArg (addOpt_P) )
  ,  ( "optc",		HasArg (addOpt_c) )
  ,  ( "optm",		HasArg (addOpt_m) )
  ,  ( "opta",		HasArg (addOpt_a) )

	------ HsCpp opts ---------------------------------------------------
  ,  ( "D",		Prefix (\s -> addOpt_P ("-D'"++s++"'") ) )
  ,  ( "U",		Prefix (\s -> addOpt_P ("-U'"++s++"'") ) )

	------ Debugging ----------------------------------------------------
  ,  ( "dstg-stats",	NoArg (writeIORef opt_StgStats True) )

  ,  ( "ddump_all",         	 NoArg (setDynFlag Opt_D_dump_all) )
  ,  ( "ddump_most",         	 NoArg (setDynFlag Opt_D_dump_most) )
  ,  ( "ddump_absC",         	 NoArg (setDynFlag Opt_D_dump_absC) )
  ,  ( "ddump_asm",          	 NoArg (setDynFlag Opt_D_dump_asm) )
  ,  ( "ddump_cpranal",      	 NoArg (setDynFlag Opt_D_dump_cpranal) )
  ,  ( "ddump_deriv",        	 NoArg (setDynFlag Opt_D_dump_deriv) )
  ,  ( "ddump_ds",           	 NoArg (setDynFlag Opt_D_dump_ds) )
  ,  ( "ddump_flatC",        	 NoArg (setDynFlag Opt_D_dump_flatC) )
  ,  ( "ddump_foreign",      	 NoArg (setDynFlag Opt_D_dump_foreign) )
  ,  ( "ddump_inlinings",    	 NoArg (setDynFlag Opt_D_dump_inlinings) )
  ,  ( "ddump_occur_anal",   	 NoArg (setDynFlag Opt_D_dump_occur_anal) )
  ,  ( "ddump_parsed",       	 NoArg (setDynFlag Opt_D_dump_parsed) )
  ,  ( "ddump_realC",        	 NoArg (setDynFlag Opt_D_dump_realC) )
  ,  ( "ddump_rn",           	 NoArg (setDynFlag Opt_D_dump_rn) )
  ,  ( "ddump_simpl",        	 NoArg (setDynFlag Opt_D_dump_simpl) )
  ,  ( "ddump_simpl_iterations", NoArg (setDynFlag Opt_D_dump_simpl_iterations) )
  ,  ( "ddump_spec",         	 NoArg (setDynFlag Opt_D_dump_spec) )
  ,  ( "ddump_stg",          	 NoArg (setDynFlag Opt_D_dump_stg) )
  ,  ( "ddump_stranal",      	 NoArg (setDynFlag Opt_D_dump_stranal) )
  ,  ( "ddump_tc",           	 NoArg (setDynFlag Opt_D_dump_tc) )
  ,  ( "ddump_types",        	 NoArg (setDynFlag Opt_D_dump_types) )
  ,  ( "ddump_rules",        	 NoArg (setDynFlag Opt_D_dump_rules) )
  ,  ( "ddump_usagesp",      	 NoArg (setDynFlag Opt_D_dump_usagesp) )
  ,  ( "ddump_cse",          	 NoArg (setDynFlag Opt_D_dump_cse) )
  ,  ( "ddump_worker_wrapper",   NoArg (setDynFlag Opt_D_dump_worker_wrapper) )
  ,  ( "dshow_passes",           NoArg (setDynFlag Opt_D_show_passes) )
  ,  ( "ddump_rn_trace",         NoArg (setDynFlag Opt_D_dump_rn_trace) )
  ,  ( "ddump_rn_stats",         NoArg (setDynFlag Opt_D_dump_rn_stats) )
  ,  ( "ddump_stix",             NoArg (setDynFlag Opt_D_dump_stix) )
  ,  ( "ddump_simpl_stats",      NoArg (setDynFlag Opt_D_dump_simpl_stats) )
  ,  ( "dsource_stats",          NoArg (setDynFlag Opt_D_source_stats) )
  ,  ( "dverbose_core2core",     NoArg (setDynFlag Opt_D_verbose_core2core) )
  ,  ( "dverbose_stg2stg",       NoArg (setDynFlag Opt_D_verbose_stg2stg) )
  ,  ( "ddump_hi_diffs",         NoArg (setDynFlag Opt_D_dump_hi_diffs) )
  ,  ( "ddump_minimal_imports",  NoArg (setDynFlag Opt_D_dump_minimal_imports) )
  ,  ( "DoCoreLinting",       	 NoArg (setDynFlag Opt_DoCoreLinting) )
  ,  ( "DoStgLinting",        	 NoArg (setDynFlag Opt_DoStgLinting) )
  ,  ( "DoUSPLinting",        	 NoArg (setDynFlag Opt_DoUSPLinting) )

	------ Machine dependant (-m<blah>) stuff ---------------------------

  ,  ( "monly-2-regs", 	NoArg (updateState (\s -> s{stolen_x86_regs = 2}) ))
  ,  ( "monly-3-regs", 	NoArg (updateState (\s -> s{stolen_x86_regs = 3}) ))
  ,  ( "monly-4-regs", 	NoArg (updateState (\s -> s{stolen_x86_regs = 4}) ))

        ------ Compiler flags -----------------------------------------------

  ,  ( "fglasgow-exts", NoArg (setDynFlag Opt_GlasgowExts) )

  ,  ( "fallow-overlapping-instances",	
		NoArg (setDynFlag Opt_AllowOverlappingInstances) )

  ,  ( "fallow-undecidable-instances",
		NoArg (setDynFlag Opt_AllowUndecidableInstances) )
 ]

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
