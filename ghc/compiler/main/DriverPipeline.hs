-----------------------------------------------------------------------------
-- $Id: DriverPipeline.hs,v 1.21 2000/11/13 17:12:37 sewardj Exp $
--
-- GHC Driver
--
-- (c) Simon Marlow 2000
--
-----------------------------------------------------------------------------

module DriverPipeline (

	-- interfaces for the batch-mode driver
   GhcMode(..), getGhcMode, v_GhcMode,
   genPipeline, runPipeline,

	-- interfaces for the compilation manager (interpreted/batch-mode)
   preprocess, compile, CompResult(..),

	-- batch-mode linking interface
   doLink
  ) where

#include "HsVersions.h"

import CmSummarise
import CmLink
import DriverState
import DriverUtil
import DriverMkDepend
import DriverPhases
import DriverFlags
import HscMain
import TmpFiles
import HscTypes
import Outputable
import Module
import CmdLineOpts
import Config
import Util

import Directory
import System
import IOExts
import Exception

import IO
import Monad
import Maybe

-----------------------------------------------------------------------------
-- GHC modes of operation

data GhcMode
  = DoMkDependHS			-- ghc -M
  | DoMkDLL				-- ghc -mk-dll
  | StopBefore Phase			-- ghc -E | -C | -S | -c
  | DoMake				-- ghc --make
  | DoInteractive			-- ghc --interactive
  | DoLink				-- [ the default ]
  deriving (Eq)

GLOBAL_VAR(v_GhcMode, error "todo", GhcMode)

modeFlag :: String -> Maybe GhcMode
modeFlag "-M" 		 = Just $ DoMkDependHS
modeFlag "-E" 		 = Just $ StopBefore Hsc
modeFlag "-C" 		 = Just $ StopBefore HCc
modeFlag "-S" 		 = Just $ StopBefore As
modeFlag "-c" 		 = Just $ StopBefore Ln
modeFlag "--make"        = Just $ DoMake
modeFlag "--interactive" = Just $ DoInteractive
modeFlag _               = Nothing

getGhcMode :: [String]
	 -> IO ( [String]   -- rest of command line
	       , GhcMode
	       , String	    -- "GhcMode" flag
	       )
getGhcMode flags 
  = case my_partition modeFlag flags of
	([]   , rest) -> return (rest, DoLink,  "") -- default is to do linking
	([(flag,one)], rest) -> return (rest, one, flag)
	(_    , _   ) -> 
	  throwDyn (OtherError 
		"only one of the flags -M, -E, -C, -S, -c, --make, --interactive is allowed")

-----------------------------------------------------------------------------
-- genPipeline
--
-- Herein is all the magic about which phases to run in which order, whether
-- the intermediate files should be in /tmp or in the current directory,
-- what the suffix of the intermediate files should be, etc.

-- The following compilation pipeline algorithm is fairly hacky.  A
-- better way to do this would be to express the whole comilation as a
-- data flow DAG, where the nodes are the intermediate files and the
-- edges are the compilation phases.  This framework would also work
-- nicely if a haskell dependency generator was included in the
-- driver.

-- It would also deal much more cleanly with compilation phases that
-- generate multiple intermediates, (eg. hsc generates .hc, .hi, and
-- possibly stub files), where some of the output files need to be
-- processed further (eg. the stub files need to be compiled by the C
-- compiler).

-- A cool thing to do would then be to execute the data flow graph
-- concurrently, automatically taking advantage of extra processors on
-- the host machine.  For example, when compiling two Haskell files
-- where one depends on the other, the data flow graph would determine
-- that the C compiler from the first comilation can be overlapped
-- with the hsc comilation for the second file.

data IntermediateFileType
  = Temporary
  | Persistent
  deriving (Eq)

genPipeline
   :: GhcMode		-- when to stop
   -> String		-- "stop after" flag (for error messages)
   -> Bool		-- True => output is persistent
   -> String		-- original filename
   -> IO [ 		-- list of phases to run for this file
	     (Phase,
	      IntermediateFileType,  -- keep the output from this phase?
	      String)   	     -- output file suffix
         ]	

genPipeline todo stop_flag persistent_output filename
 = do
   split      <- readIORef v_Split_object_files
   mangle     <- readIORef v_Do_asm_mangling
   lang       <- readIORef v_Hsc_Lang
   keep_hc    <- readIORef v_Keep_hc_files
   keep_raw_s <- readIORef v_Keep_raw_s_files
   keep_s     <- readIORef v_Keep_s_files
   osuf       <- readIORef v_Object_suf

   let
   ----------- -----  ----   ---   --   --  -  -  -
    (_basename, suffix) = splitFilename filename

    start_phase = startPhase suffix

    haskellish = haskellish_suffix suffix
    cish = cish_suffix suffix

   -- for a .hc file, or if the -C flag is given, we need to force lang to HscC
    real_lang | suffix == "hc"  = HscC
	      | otherwise       = lang

   let
   ----------- -----  ----   ---   --   --  -  -  -
    pipeline
      | todo == DoMkDependHS = [ Unlit, Cpp, MkDependHS ]

      | haskellish = 
       case real_lang of
	HscC    | split && mangle -> [ Unlit, Cpp, Hsc, HCc, Mangle, 
					SplitMangle, SplitAs ]
	        | mangle          -> [ Unlit, Cpp, Hsc, HCc, Mangle, As ]
	        | split	       	  -> not_valid
	        | otherwise       -> [ Unlit, Cpp, Hsc, HCc, As ]

	HscAsm  | split           -> [ Unlit, Cpp, Hsc, SplitMangle, SplitAs ]
	        | otherwise       -> [ Unlit, Cpp, Hsc, As ]

	HscJava	| split	          -> not_valid
		| otherwise       -> error "not implemented: compiling via Java"

      | cish      = [ Cc, As ]

      | otherwise = [ ]  -- just pass this file through to the linker

	-- ToDo: this is somewhat cryptic
    not_valid = throwDyn (OtherError ("invalid option combination"))
   ----------- -----  ----   ---   --   --  -  -  -

	-- this shouldn't happen.
   if start_phase /= Ln && start_phase `notElem` pipeline
	then throwDyn (OtherError ("can't find starting phase for "
				    ++ filename))
	else do

	-- if we can't find the phase we're supposed to stop before,
	-- something has gone wrong.
   case todo of
	StopBefore phase -> 
	   when (phase /= Ln 
		 && phase `notElem` pipeline
	   	 && not (phase == As && SplitAs `elem` pipeline)) $
	      throwDyn (OtherError 
		("flag " ++ stop_flag
		 ++ " is incompatible with source file `" ++ filename ++ "'"))
	_ -> return ()

   let
   ----------- -----  ----   ---   --   --  -  -  -
      myPhaseInputExt Ln = case osuf of Nothing -> phaseInputExt Ln
					Just s  -> s
      myPhaseInputExt other = phaseInputExt other

      annotatePipeline
	 :: [Phase]		-- raw pipeline
	 -> Phase		-- phase to stop before
     	 -> [(Phase, IntermediateFileType, String{-file extension-})]
      annotatePipeline []     _    = []
      annotatePipeline (Ln:_) _    = []
      annotatePipeline (phase:next_phase:ps) stop = 
     	  (phase, keep_this_output, myPhaseInputExt next_phase)
	     : annotatePipeline (next_phase:ps) stop
     	  where
     		keep_this_output
     		     | next_phase == stop 
                     = if persistent_output then Persistent else Temporary
     		     | otherwise
     		     = case next_phase of
     			     Ln -> Persistent
     			     Mangle | keep_raw_s -> Persistent
     			     As     | keep_s     -> Persistent
     			     HCc    | keep_hc    -> Persistent
     			     _other              -> Temporary

	-- add information about output files to the pipeline
	-- the suffix on an output file is determined by the next phase
	-- in the pipeline, so we add linking to the end of the pipeline
	-- to force the output from the final phase to be a .o file.
      stop_phase = case todo of StopBefore phase -> phase
				DoMkDependHS	 -> Ln
				DoLink           -> Ln
      annotated_pipeline = annotatePipeline (pipeline ++ [ Ln ]) stop_phase

      phase_ne p (p1,_,_) = (p1 /= p)
   ----------- -----  ----   ---   --   --  -  -  -

   return $
     dropWhile (phase_ne start_phase) . 
	foldr (\p ps -> if phase_ne stop_phase p then p:ps else [])  []
		$ annotated_pipeline


runPipeline
  :: [ (Phase, IntermediateFileType, String) ] -- phases to run
  -> String			-- input file
  -> Bool			-- doing linking afterward?
  -> Bool			-- take into account -o when generating output?
  -> IO String			-- return final filename

runPipeline pipeline input_fn do_linking use_ofile
  = pipeLoop pipeline input_fn do_linking use_ofile basename suffix
  where (basename, suffix) = splitFilename input_fn

pipeLoop [] input_fn _ _ _ _ = return input_fn
pipeLoop ((phase, keep, o_suffix):phases) 
	input_fn do_linking use_ofile orig_basename orig_suffix
  = do

     output_fn <- outputFileName (null phases) keep o_suffix

     carry_on <- run_phase phase orig_basename orig_suffix input_fn output_fn
	-- sometimes we bail out early, eg. when the compiler's recompilation
	-- checker has determined that recompilation isn't necessary.
     if not carry_on 
	then do let (_,keep,final_suffix) = last phases
	        ofile <- outputFileName True keep final_suffix
		return ofile
	else do -- carry on ...

     pipeLoop phases output_fn do_linking use_ofile orig_basename orig_suffix

  where
     outputFileName last_phase keep suffix
  	= do o_file <- readIORef v_Output_file
   	     if last_phase && not do_linking && use_ofile && isJust o_file
   	       then case o_file of 
   		       Just s  -> return s
   		       Nothing -> error "outputFileName"
   	       else if keep == Persistent
   			   then odir_ify (orig_basename ++ '.':suffix)
   			   else newTempName suffix

-------------------------------------------------------------------------------
-- Unlit phase 

run_phase Unlit _basename _suff input_fn output_fn
  = do unlit <- readIORef v_Pgm_L
       unlit_flags <- getOpts opt_L
       run_something "Literate pre-processor"
	  ("echo '# 1 \"" ++input_fn++"\"' > "++output_fn++" && "
	   ++ unlit ++ ' ':input_fn ++ " - >> " ++ output_fn)
       return True

-------------------------------------------------------------------------------
-- Cpp phase 

run_phase Cpp basename suff input_fn output_fn
  = do src_opts <- getOptionsFromSource input_fn
       unhandled_flags <- processArgs dynamic_flags src_opts []

       when (not (null unhandled_flags)) 
            (throwDyn (OtherError (
                          basename ++ "." ++ suff 
                          ++ ": static flags are not allowed in {-# OPTIONS #-} pragmas:\n\t" 
                          ++ unwords unhandled_flags)) (ExitFailure 1))

       do_cpp <- readState cpp_flag
       if do_cpp
          then do
       	    cpp <- readIORef v_Pgm_P
	    hscpp_opts <- getOpts opt_P
       	    hs_src_cpp_opts <- readIORef v_Hs_source_cpp_opts

	    cmdline_include_paths <- readIORef v_Include_paths
	    pkg_include_dirs <- getPackageIncludePath
	    let include_paths = map (\p -> "-I"++p) (cmdline_include_paths
							++ pkg_include_dirs)

	    verb <- is_verbose
	    run_something "C pre-processor" 
		(unwords
       	    	   (["echo '{-# LINE 1 \"" ++ input_fn ++ "\" -}'", ">", output_fn, "&&",
		     cpp, verb] 
		    ++ include_paths
		    ++ hs_src_cpp_opts
		    ++ hscpp_opts
		    ++ [ "-x", "c", input_fn, ">>", output_fn ]
		   ))
	  else do
	    run_something "Ineffective C pre-processor"
	           ("echo '{-# LINE 1 \""  ++ input_fn ++ "\" -}' > " 
		    ++ output_fn ++ " && cat " ++ input_fn
		    ++ " >> " ++ output_fn)
       return True

-----------------------------------------------------------------------------
-- MkDependHS phase

run_phase MkDependHS basename suff input_fn _output_fn = do 
   src <- readFile input_fn
   let imports = getImports src

   deps <- mapM (findDependency basename) imports

   osuf_opt <- readIORef v_Object_suf
   let osuf = case osuf_opt of
			Nothing -> phaseInputExt Ln
			Just s  -> s

   extra_suffixes <- readIORef v_Dep_suffixes
   let suffixes = osuf : map (++ ('_':osuf)) extra_suffixes
       ofiles = map (\suf -> basename ++ '.':suf) suffixes
   	   
   objs <- mapM odir_ify ofiles
   
   hdl <- readIORef v_Dep_tmp_hdl

	-- std dependency of the object(s) on the source file
   hPutStrLn hdl (unwords objs ++ " : " ++ basename ++ '.':suff)

   let genDep (dep, False {- not an hi file -}) = 
	  hPutStrLn hdl (unwords objs ++ " : " ++ dep)
       genDep (dep, True  {- is an hi file -}) = do
	  hisuf <- readIORef v_Hi_suf
	  let dep_base = remove_suffix '.' dep
	      deps = (dep_base ++ hisuf)
		     : map (\suf -> dep_base ++ suf ++ '_':hisuf) extra_suffixes
		  -- length objs should be == length deps
	  sequence_ (zipWith (\o d -> hPutStrLn hdl (o ++ " : " ++ d)) objs deps)

   mapM genDep [ d | Just d <- deps ]

   return True

-- add the lines to dep_makefile:
	   -- always:
		   -- this.o : this.hs

  	   -- if the dependency is on something other than a .hi file:
   		   -- this.o this.p_o ... : dep
   	   -- otherwise
   		   -- if the import is {-# SOURCE #-}
   			   -- this.o this.p_o ... : dep.hi-boot[-$vers]
   			   
   		   -- else
   			   -- this.o ...   : dep.hi
   			   -- this.p_o ... : dep.p_hi
   			   -- ...
   
   	   -- (where .o is $osuf, and the other suffixes come from
   	   -- the cmdline -s options).
   
-----------------------------------------------------------------------------
-- Hsc phase

-- Compilation of a single module, in "legacy" mode (_not_ under
-- the direction of the compilation manager).
run_phase Hsc basename suff input_fn output_fn
  = do
	
  -- we add the current directory (i.e. the directory in which
  -- the .hs files resides) to the import path, since this is
  -- what gcc does, and it's probably what you want.
	let current_dir = getdir basename
	
	paths <- readIORef v_Include_paths
	writeIORef v_Include_paths (current_dir : paths)
	
  -- figure out where to put the .hi file
	ohi    <- readIORef v_Output_hi
	hisuf  <- readIORef v_Hi_suf
	let hifile = case ohi of
			   Nothing -> current_dir ++ "/" ++ basename
					++ "." ++ hisuf
			   Just fn -> fn

  -- figure out if the source has changed, for recompilation avoidance.
  -- only do this if we're eventually going to generate a .o file.
  -- (ToDo: do when generating .hc files too?)
  --
  -- Setting source_unchanged to True means that M.o seems
  -- to be up to date wrt M.hs; so no need to recompile unless imports have
  -- changed (which the compiler itself figures out).
  -- Setting source_unchanged to False tells the compiler that M.o is out of
  -- date wrt M.hs (or M.o doesn't exist) so we must recompile regardless.
	do_recomp <- readIORef v_Recomp
	todo <- readIORef v_GhcMode
        o_file <- odir_ify (basename ++ '.':phaseInputExt Ln)
	source_unchanged <- 
          if not (do_recomp && ( todo == DoLink || todo == StopBefore Ln ))
	     then return False
	     else do t1 <- getModificationTime (basename ++ '.':suff)
		     o_file_exists <- doesFileExist o_file
		     if not o_file_exists
		        then return False	-- Need to recompile
			else do t2 <- getModificationTime o_file
			        if t2 > t1
				  then return True
				  else return False

   -- build a ModuleLocation to pass to hscMain.
        let location = ModuleLocation {
                          ml_hs_file   = Nothing,
                          ml_hspp_file = Just input_fn,
                          ml_hi_file   = Just hifile,
                          ml_obj_file  = Just o_file
                       }

  -- get the DynFlags
        dyn_flags <- readIORef v_DynFlags

  -- run the compiler!
        pcs <- initPersistentCompilerState
	result <- hscMain dyn_flags{ hscOutName = output_fn }
			  source_unchanged
			  location
			  Nothing	 -- no iface
			  emptyModuleEnv -- HomeSymbolTable
			  emptyModuleEnv -- HomeIfaceTable
			  pcs

	case result of {

	    HscFail pcs -> throwDyn (PhaseFailed "hsc" (ExitFailure 1));

	    HscOK details maybe_iface maybe_stub_h maybe_stub_c 
			_maybe_interpreted_code pcs -> do

	    -- deal with stubs
	maybe_stub_o <- dealWithStubs basename maybe_stub_h maybe_stub_c
	case maybe_stub_o of
		Nothing -> return ()
		Just stub_o -> add v_Ld_inputs stub_o

        let keep_going = case maybe_iface of Just _ -> True; Nothing -> False
	return keep_going
    }

-----------------------------------------------------------------------------
-- Cc phase

-- we don't support preprocessing .c files (with -E) now.  Doing so introduces
-- way too many hacks, and I can't say I've ever used it anyway.

run_phase cc_phase _basename _suff input_fn output_fn
   | cc_phase == Cc || cc_phase == HCc
   = do	cc <- readIORef v_Pgm_c
       	cc_opts <- (getOpts opt_c)
       	cmdline_include_dirs <- readIORef v_Include_paths

        let hcc = cc_phase == HCc

		-- add package include paths even if we're just compiling
		-- .c files; this is the Value Add(TM) that using
		-- ghc instead of gcc gives you :)
        pkg_include_dirs <- getPackageIncludePath
	let include_paths = map (\p -> "-I"++p) (cmdline_include_dirs 
							++ pkg_include_dirs)

	c_includes <- getPackageCIncludes
	cmdline_includes <- readState cmdline_hc_includes -- -#include options

	let cc_injects | hcc = unlines (map mk_include 
					(c_includes ++ reverse cmdline_includes))
		       | otherwise = ""
	    mk_include h_file = 
		case h_file of 
		   '"':_{-"-} -> "#include "++h_file
		   '<':_      -> "#include "++h_file
		   _          -> "#include \""++h_file++"\""

	cc_help <- newTempName "c"
	h <- openFile cc_help WriteMode
	hPutStr h cc_injects
	hPutStrLn h ("#include \"" ++ input_fn ++ "\"\n")
	hClose h

	ccout <- newTempName "ccout"

	mangle <- readIORef v_Do_asm_mangling
	(md_c_flags, md_regd_c_flags) <- machdepCCOpts

        verb <- is_verbose

	o2 <- readIORef v_minus_o2_for_C
	let opt_flag | o2        = "-O2"
		     | otherwise = "-O"

	pkg_extra_cc_opts <- getPackageExtraCcOpts

	split_objs <- readIORef v_Split_object_files
	let split_opt | hcc && split_objs = [ "-DUSE_SPLIT_MARKERS" ]
		      | otherwise         = [ ]

	excessPrecision <- readIORef v_Excess_precision

	run_something "C Compiler"
	 (unwords ([ cc, "-x", "c", cc_help, "-o", output_fn ]
		   ++ md_c_flags
		   ++ (if cc_phase == HCc && mangle
			 then md_regd_c_flags
			 else [])
		   ++ [ verb, "-S", "-Wimplicit", opt_flag ]
		   ++ [ "-D__GLASGOW_HASKELL__="++cProjectVersionInt ]
		   ++ cc_opts
		   ++ split_opt
#ifdef mingw32_TARGET_OS
                   ++ [" -mno-cygwin"]
#endif
		   ++ (if excessPrecision then [] else [ "-ffloat-store" ])
		   ++ include_paths
		   ++ pkg_extra_cc_opts
--		   ++ [">", ccout]
		   ))
	return True

	-- ToDo: postprocess the output from gcc

-----------------------------------------------------------------------------
-- Mangle phase

run_phase Mangle _basename _suff input_fn output_fn
  = do mangler <- readIORef v_Pgm_m
       mangler_opts <- getOpts opt_m
       machdep_opts <-
	 if (prefixMatch "i386" cTARGETPLATFORM)
	    then do n_regs <- readState stolen_x86_regs
		    return [ show n_regs ]
	    else return []
       run_something "Assembly Mangler"
	(unwords (mangler : 
		     mangler_opts
		  ++ [ input_fn, output_fn ]
		  ++ machdep_opts
		))
       return True

-----------------------------------------------------------------------------
-- Splitting phase

run_phase SplitMangle _basename _suff input_fn _output_fn
  = do  splitter <- readIORef v_Pgm_s

	-- this is the prefix used for the split .s files
	tmp_pfx <- readIORef v_TmpDir
	x <- myGetProcessID
	let split_s_prefix = tmp_pfx ++ "/ghc" ++ show x
	writeIORef v_Split_prefix split_s_prefix
	addFilesToClean [split_s_prefix ++ "__*"] -- d:-)

	-- allocate a tmp file to put the no. of split .s files in (sigh)
	n_files <- newTempName "n_files"

	run_something "Split Assembly File"
	 (unwords [ splitter
		  , input_fn
		  , split_s_prefix
		  , n_files ]
	 )

	-- save the number of split files for future references
	s <- readFile n_files
	let n = read s :: Int
	writeIORef v_N_split_files n
	return True

-----------------------------------------------------------------------------
-- As phase

run_phase As _basename _suff input_fn output_fn
  = do 	as <- readIORef v_Pgm_a
        as_opts <- getOpts opt_a

        cmdline_include_paths <- readIORef v_Include_paths
        let cmdline_include_flags = map (\p -> "-I"++p) cmdline_include_paths
        run_something "Assembler"
	   (unwords (as : as_opts
		       ++ cmdline_include_flags
		       ++ [ "-c", input_fn, "-o",  output_fn ]
		    ))
	return True

run_phase SplitAs basename _suff _input_fn _output_fn
  = do  as <- readIORef v_Pgm_a
        as_opts <- getOpts opt_a

	split_s_prefix <- readIORef v_Split_prefix
	n <- readIORef v_N_split_files

	odir <- readIORef v_Output_dir
	let real_odir = case odir of
				Nothing -> basename
				Just d  -> d

	let assemble_file n = do
		    let input_s  = split_s_prefix ++ "__" ++ show n ++ ".s"
		    let output_o = newdir real_odir 
					(basename ++ "__" ++ show n ++ ".o")
		    real_o <- osuf_ify output_o
		    run_something "Assembler" 
			    (unwords (as : as_opts
				      ++ [ "-c", "-o", real_o, input_s ]
			    ))
	
	mapM_ assemble_file [1..n]
	return True

-----------------------------------------------------------------------------
-- Linking

doLink :: [String] -> IO ()
doLink o_files = do
    ln <- readIORef v_Pgm_l
    verb <- is_verbose
    static <- readIORef v_Static
    let imp = if static then "" else "_imp"
    no_hs_main <- readIORef v_NoHsMain

    o_file <- readIORef v_Output_file
    let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }

    pkg_lib_paths <- getPackageLibraryPath
    let pkg_lib_path_opts = map ("-L"++) pkg_lib_paths

    lib_paths <- readIORef v_Library_paths
    let lib_path_opts = map ("-L"++) lib_paths

    pkg_libs <- getPackageLibraries
    let pkg_lib_opts = map (\lib -> "-l" ++ lib ++ imp) pkg_libs

    libs <- readIORef v_Cmdline_libraries
    let lib_opts = map ("-l"++) (reverse libs)
	 -- reverse because they're added in reverse order from the cmd line

    pkg_extra_ld_opts <- getPackageExtraLdOpts

	-- probably _stub.o files
    extra_ld_inputs <- readIORef v_Ld_inputs

	-- opts from -optl-<blah>
    extra_ld_opts <- getStaticOpts v_Opt_l

    rts_pkg <- getPackageDetails ["rts"]
    std_pkg <- getPackageDetails ["std"]
#ifdef mingw32_TARGET_OS
    let extra_os = if static || no_hs_main
                   then []
                   else [ head (library_dirs (head rts_pkg)) ++ "/Main.dll_o",
                          head (library_dirs (head std_pkg)) ++ "/PrelMain.dll_o" ]
#endif
    (md_c_flags, _) <- machdepCCOpts
    run_something "Linker"
       (unwords
	 ([ ln, verb, "-o", output_fn ]
	 ++ md_c_flags
	 ++ o_files
#ifdef mingw32_TARGET_OS
	 ++ extra_os
#endif
	 ++ extra_ld_inputs
	 ++ lib_path_opts
	 ++ lib_opts
	 ++ pkg_lib_path_opts
	 ++ pkg_lib_opts
	 ++ pkg_extra_ld_opts
	 ++ extra_ld_opts
#ifdef mingw32_TARGET_OS
         ++ if static then [ "-u _PrelMain_mainIO_closure" , "-u ___init_PrelMain"] else []
#else
	 ++ [ "-u PrelMain_mainIO_closure" , "-u __init_PrelMain"]
#endif
	)
       )

-----------------------------------------------------------------------------
-- Just preprocess a file, put the result in a temp. file (used by the
-- compilation manager during the summary phase).

preprocess :: FilePath -> IO FilePath
preprocess filename =
  ASSERT(haskellish_file filename) 
  do pipeline <- genPipeline (StopBefore Hsc) ("preprocess") False filename
     runPipeline pipeline filename False{-no linking-} False{-no -o flag-}


-----------------------------------------------------------------------------
-- Compile a single module, under the control of the compilation manager.
--
-- This is the interface between the compilation manager and the
-- compiler proper (hsc), where we deal with tedious details like
-- reading the OPTIONS pragma from the source file, and passing the
-- output of hsc through the C compiler.

-- The driver sits between 'compile' and 'hscMain', translating calls
-- to the former into calls to the latter, and results from the latter
-- into results from the former.  It does things like preprocessing
-- the .hs file if necessary, and compiling up the .stub_c files to
-- generate Linkables.

compile :: ModSummary              -- summary, including source
        -> Maybe ModIface          -- old interface, if available
        -> HomeSymbolTable         -- for home module ModDetails
	-> HomeIfaceTable	   -- for home module Ifaces
        -> PersistentCompilerState -- persistent compiler state
        -> IO CompResult

data CompResult
   = CompOK   ModDetails  -- new details (HST additions)
              (Maybe (ModIface, Linkable))
                       -- summary and code; Nothing => compilation not reqd
                       -- (old summary and code are still valid)
              PersistentCompilerState	-- updated PCS

   | CompErrs PersistentCompilerState	-- updated PCS


compile summary old_iface hst hit pcs = do 
   verb <- readIORef v_Verbose
   when verb (hPutStrLn stderr 
                 (showSDoc (text "compile: compiling" 
                            <+> ppr (name_of_summary summary))))

   init_dyn_flags <- readIORef v_InitDynFlags
   writeIORef v_DynFlags init_dyn_flags

   let location = ms_location summary   
   let input_fn = unJust (ml_hs_file location) "compile:hs"

   when verb (hPutStrLn stderr ("compile: input file " ++ input_fn))

   opts <- getOptionsFromSource input_fn
   processArgs dynamic_flags opts []
   dyn_flags <- readIORef v_DynFlags

   hsc_lang <- readIORef v_Hsc_Lang
   output_fn <- case hsc_lang of
		    HscAsm         -> newTempName (phaseInputExt As)
		    HscC           -> newTempName (phaseInputExt HCc)
        	    HscJava        -> newTempName "java" -- ToDo
		    HscInterpreted -> return (error "no output file")

   -- run the compiler
   hsc_result <- hscMain dyn_flags{ hscOutName = output_fn } 
			 False -- (panic "compile:source_unchanged")
                         location old_iface hst hit pcs

   case hsc_result of {
      HscFail pcs -> return (CompErrs pcs);

      HscOK details maybe_iface 
	maybe_stub_h maybe_stub_c maybe_interpreted_code pcs -> do
	   
	   -- if no compilation happened, bail out early
	   case maybe_iface of {
		Nothing -> return (CompOK details Nothing pcs);
		Just iface -> do

	   let (basename, _) = splitFilename input_fn
	   maybe_stub_o <- dealWithStubs basename maybe_stub_h maybe_stub_c
	   let stub_unlinked = case maybe_stub_o of
				  Nothing -> []
				  Just stub_o -> [ DotO stub_o ]

	   hs_unlinked <-
	     case hsc_lang of

		-- in interpreted mode, just return the compiled code
		-- as our "unlinked" object.
		HscInterpreted -> 
		    case maybe_interpreted_code of
			Just (code,itbl_env) -> return [Trees code itbl_env]
			Nothing -> panic "compile: no interpreted code"

		-- we're in batch mode: finish the compilation pipeline.
		_other -> do pipe <- genPipeline (StopBefore Ln) "" True output_fn
			     o_file <- runPipeline pipe output_fn False False
			     return [ DotO o_file ]

	   let linkable = LM (moduleName (ms_mod summary)) 
				(hs_unlinked ++ stub_unlinked)

	   return (CompOK details (Just (iface, linkable)) pcs)
          }
   }

-----------------------------------------------------------------------------
-- stub .h and .c files (for foreign export support)

dealWithStubs basename maybe_stub_h maybe_stub_c

 = do	let stub_h = basename ++ "_stub.h"
	let stub_c = basename ++ "_stub.c"

  -- copy the .stub_h file into the current dir if necessary
	case maybe_stub_h of
	   Nothing -> return ()
	   Just tmp_stub_h -> do
	      	run_something "Copy stub .h file"
				("cp " ++ tmp_stub_h ++ ' ':stub_h)
	
			-- #include <..._stub.h> in .hc file
		addCmdlineHCInclude tmp_stub_h	-- hack

  -- copy the .stub_c file into the current dir, and compile it, if necessary
	case maybe_stub_c of
	   Nothing -> return Nothing
	   Just tmp_stub_c -> do  -- copy the _stub.c file into the current dir
		run_something "Copy stub .c file" 
		    (unwords [ 
			"rm -f", stub_c, "&&",
			"echo \'#include \""++stub_h++"\"\' >"++stub_c, " &&",
			"cat", tmp_stub_c, ">> ", stub_c
			])

			-- compile the _stub.c file w/ gcc
		pipeline <- genPipeline (StopBefore Ln) "" True stub_c
		stub_o <- runPipeline pipeline stub_c False{-no linking-} 
				False{-no -o option-}

		return (Just stub_o)
