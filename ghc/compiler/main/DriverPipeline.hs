-----------------------------------------------------------------------------
-- $Id: DriverPipeline.hs,v 1.72 2001/05/29 17:53:59 sof Exp $
--
-- GHC Driver
--
-- (c) Simon Marlow 2000
--
-----------------------------------------------------------------------------

#include "../includes/config.h"

module DriverPipeline (

	-- interfaces for the batch-mode driver
   GhcMode(..), getGhcMode, v_GhcMode,
   genPipeline, runPipeline, pipeLoop,

	-- interfaces for the compilation manager (interpreted/batch-mode)
   preprocess, compile, CompResult(..),

	-- batch-mode linking interface
   doLink,
        -- DLL building
   doMkDLL
  ) where

#include "HsVersions.h"

import CmStaticInfo
import CmTypes
import GetImports
import DriverState
import DriverUtil
import DriverMkDepend
import DriverPhases
import DriverFlags
import HscMain
import TmpFiles
import Finder
import HscTypes
import Outputable
import Module
import ErrUtils
import CmdLineOpts
import Config
import Panic
import Util

import Time 		( getClockTime )
import Directory
import System
import IOExts
import Exception

import IO
import Monad
import Maybe

import PackedString
import MatchPS

-----------------------------------------------------------------------------
-- GHC modes of operation

data GhcMode
  = DoMkDependHS			-- ghc -M
  | DoMkDLL				-- ghc --mk-dll
  | StopBefore Phase			-- ghc -E | -C | -S | -c
  | DoMake				-- ghc --make
  | DoInteractive			-- ghc --interactive
  | DoLink				-- [ the default ]
  deriving (Eq)

GLOBAL_VAR(v_GhcMode, error "todo", GhcMode)

modeFlag :: String -> Maybe GhcMode
modeFlag "-M" 		 = Just $ DoMkDependHS
modeFlag "--mk-dll"      = Just $ DoMkDLL
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
	  throwDyn (UsageError 
		"only one of the flags -M, -E, -C, -S, -c, --make, --interactive, -mk-dll is allowed")

-----------------------------------------------------------------------------
-- genPipeline
--
-- Herein is all the magic about which phases to run in which order, whether
-- the intermediate files should be in TMPDIR or in the current directory,
-- what the suffix of the intermediate files should be, etc.

-- The following compilation pipeline algorithm is fairly hacky.  A
-- better way to do this would be to express the whole compilation as a
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
-- that the C compiler from the first compilation can be overlapped
-- with the hsc compilation for the second file.

data IntermediateFileType
  = Temporary
  | Persistent
  deriving (Eq, Show)

genPipeline
   :: GhcMode		-- when to stop
   -> String		-- "stop after" flag (for error messages)
   -> Bool		-- True => output is persistent
   -> HscLang		-- preferred output language for hsc
   -> String		-- original filename
   -> IO [ 		-- list of phases to run for this file
	     (Phase,
	      IntermediateFileType,  -- keep the output from this phase?
	      String)   	     -- output file suffix
         ]	

genPipeline todo stop_flag persistent_output lang filename 
 = do
   split      <- readIORef v_Split_object_files
   mangle     <- readIORef v_Do_asm_mangling
   keep_hc    <- readIORef v_Keep_hc_files
   keep_raw_s <- readIORef v_Keep_raw_s_files
   keep_s     <- readIORef v_Keep_s_files
   osuf       <- readIORef v_Object_suf
   hcsuf      <- readIORef v_HC_suf

   let
   ----------- -----  ----   ---   --   --  -  -  -
    (_basename, suffix) = splitFilename filename

    start = startPhase suffix

      -- special case for mkdependHS: .hspp files go through MkDependHS
    start_phase | todo == DoMkDependHS && start == Hsc  = MkDependHS
	        | otherwise = start

    haskellish = haskellish_suffix suffix
    cish = cish_suffix suffix

       -- for a .hc file we need to force lang to HscC
    real_lang | start_phase == HCc || start_phase == Mangle = HscC
	      | otherwise                                   = lang

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
	HscILX  | split           -> not_valid
		| otherwise       -> [ Unlit, Cpp, Hsc ]

      | cish      = [ Cc, As ]

      | otherwise = [ ]  -- just pass this file through to the linker

	-- ToDo: this is somewhat cryptic

    not_valid = throwDyn (UsageError ("invalid option combination"))
   ----------- -----  ----   ---   --   --  -  -  -

	-- this shouldn't happen.
   if start_phase /= Ln && start_phase `notElem` pipeline
	then throwDyn (CmdLineError ("can't find starting phase for "
				     ++ filename))
	else do

   let
	-- .o and .hc suffixes can be overriden by command-line options:
      myPhaseInputExt Ln  | Just s <- osuf  = s
      myPhaseInputExt HCc | Just s <- hcsuf = s
      myPhaseInputExt other                 = phaseInputExt other

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
      stop_phase = case todo of 
			StopBefore As | split -> SplitAs
			StopBefore phase      -> phase
			DoMkDependHS	      -> Ln
			DoLink                -> Ln

      annotated_pipeline = annotatePipeline (pipeline ++ [Ln]) stop_phase

      phase_ne p (p1,_,_) = (p1 /= p)
   ----------- -----  ----   ---   --   --  -  -  -

	-- if we can't find the phase we're supposed to stop before,
	-- something has gone wrong.  This test carefully avoids the
	-- case where we aren't supposed to do any compilation, because the file
	-- is already in linkable form (for example).
   if start_phase `elem` pipeline && 
 	(stop_phase /= Ln && stop_phase `notElem` pipeline)
      then throwDyn (UsageError 
		("flag " ++ stop_flag
		 ++ " is incompatible with source file `" ++ filename ++ "'"))
      else do

   return (
     takeWhile (phase_ne stop_phase ) $
     dropWhile (phase_ne start_phase) $
     annotated_pipeline
    )


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
       runSomething "Literate pre-processor"
       	  (unlit ++ unwords unlit_flags ++ 
	  	    " -h " ++ input_fn ++ 
		    ' ':input_fn ++ 
		    ' ':output_fn)
       return True

-------------------------------------------------------------------------------
-- Cpp phase 

run_phase Cpp basename suff input_fn output_fn
  = do src_opts <- getOptionsFromSource input_fn
       unhandled_flags <- processArgs dynamic_flags src_opts []
       checkProcessArgsResult unhandled_flags basename suff

       do_cpp <- dynFlag cppFlag
       if do_cpp
          then do
       	    cpp <- readIORef v_Pgm_P >>= prependToolDir
	    hscpp_opts <- getOpts opt_P
       	    hs_src_cpp_opts <- readIORef v_Hs_source_cpp_opts

	    cmdline_include_paths <- readIORef v_Include_paths
	    pkg_include_dirs <- getPackageIncludePath
	    let include_paths = map (\p -> "-I"++p) (cmdline_include_paths
							++ pkg_include_dirs)

	    verb <- getVerbFlag
	    (md_c_flags, _) <- machdepCCOpts

	    runSomething "C pre-processor" 
		(unwords
       	    	   ([cpp, verb] 
		    ++ include_paths
		    ++ hs_src_cpp_opts
		    ++ hscpp_opts
		    ++ md_c_flags
		    ++ [ "-x", "c", input_fn, "-o", output_fn ]
		   ))
	-- ToDo: switch away from using 'echo' alltogether (but need
	-- a faster alternative than what's done below).
#if defined(mingw32_TARGET_OS) && defined(MINIMAL_UNIX_DEPS)
	  else (do
	    h <- openFile output_fn WriteMode
	    hPutStrLn h ("{-# LINE 1 \"" ++ input_fn ++ "\" #-}")
	    ls <- readFile input_fn -- inefficient, but it'll do for now.
	    			    -- ToDo: speed up via slurping.
	    hPutStrLn h ls
	    hClose h) `catchAllIO`
	    	        (\_ -> throwDyn (PhaseFailed "Ineffective C pre-processor" (ExitFailure 1)))
#else
	  else do
	    runSomething "Ineffective C pre-processor"
	           ("echo '{-# LINE 1 \""  ++ input_fn ++ "\" #-}' > " 
		    ++ output_fn ++ " && cat " ++ input_fn
		    ++ " >> " ++ output_fn)
#endif
       return True

-----------------------------------------------------------------------------
-- MkDependHS phase

run_phase MkDependHS basename suff input_fn _output_fn = do 
   src <- readFile input_fn
   let (import_sources, import_normals, module_name) = getImports src

   let orig_fn = basename ++ '.':suff
   deps_sources <- mapM (findDependency True  orig_fn) import_sources
   deps_normals <- mapM (findDependency False orig_fn) import_normals
   let deps = deps_sources ++ deps_normals

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
			   Nothing -> basename ++ '.':hisuf
			   Just fn -> fn

  -- figure out which header files to #include in a generated .hc file
	c_includes <- getPackageCIncludes
	cmdline_includes <- dynFlag cmdlineHcIncludes -- -#include options

	let cc_injects = unlines (map mk_include 
				 (c_includes ++ reverse cmdline_includes))
	    mk_include h_file = 
		case h_file of 
		   '"':_{-"-} -> "#include "++h_file
		   '<':_      -> "#include "++h_file
		   _          -> "#include \""++h_file++"\""

	writeIORef v_HCHeader cc_injects

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
        o_file' <- odir_ify (basename ++ '.':phaseInputExt Ln)
        o_file <- osuf_ify o_file'
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
        (srcimps,imps,mod_name) <- getImportsFromFile input_fn

	Just (mod, location)
	   <- mkHomeModuleLocn mod_name basename (basename ++ '.':suff)

  -- get the DynFlags
        dyn_flags <- readIORef v_DynFlags

        let dyn_flags' = dyn_flags { hscOutName = output_fn,
		   		     hscStubCOutName = basename ++ "_stub.c",
				     hscStubHOutName = basename ++ "_stub.h" }

  -- run the compiler!
        pcs <- initPersistentCompilerState
	result <- hscMain OneShot
                          dyn_flags' mod
			  location{ ml_hspp_file=Just input_fn }
			  source_unchanged
			  False
			  Nothing	 -- no iface
			  emptyModuleEnv -- HomeSymbolTable
			  emptyModuleEnv -- HomeIfaceTable
			  pcs

	case result of {

	    HscFail pcs -> throwDyn (PhaseFailed "hsc" (ExitFailure 1));

            HscNoRecomp pcs details iface -> 
	        do {
#if defined(mingw32_TARGET_OS) && defined(MINIMAL_UNIX_DEPS)
		  touch <- readIORef v_Pgm_T;
		  runSomething "Touching object file" (unwords [dosifyPath touch, dosifyPath o_file]);
#else
		  runSomething "Touching object file" (unwords [cTOUCH, o_file]);
#endif
		  return False;
		};

	    HscRecomp pcs details iface stub_h_exists stub_c_exists
		      _maybe_interpreted_code -> do

	    -- deal with stubs
	maybe_stub_o <- compileStub dyn_flags' stub_c_exists
	case maybe_stub_o of
		Nothing -> return ()
		Just stub_o -> add v_Ld_inputs stub_o

	return True
    }

-----------------------------------------------------------------------------
-- Cc phase

-- we don't support preprocessing .c files (with -E) now.  Doing so introduces
-- way too many hacks, and I can't say I've ever used it anyway.

run_phase cc_phase basename suff input_fn output_fn
   | cc_phase == Cc || cc_phase == HCc
   = do	cc  <- readIORef v_Pgm_c >>= prependToolDir >>= appendInstallDir
       	cc_opts <- (getOpts opt_c)
       	cmdline_include_dirs <- readIORef v_Include_paths

        let hcc = cc_phase == HCc

		-- add package include paths even if we're just compiling
		-- .c files; this is the Value Add(TM) that using
		-- ghc instead of gcc gives you :)
        pkg_include_dirs <- getPackageIncludePath
	let include_paths = map (\p -> "-I"++p) (cmdline_include_dirs 
							++ pkg_include_dirs)

	mangle <- readIORef v_Do_asm_mangling
	(md_c_flags, md_regd_c_flags) <- machdepCCOpts

        verb <- getVerbFlag

	o2 <- readIORef v_minus_o2_for_C
	let opt_flag | o2        = "-O2"
		     | otherwise = "-O"

	pkg_extra_cc_opts <- getPackageExtraCcOpts

	split_objs <- readIORef v_Split_object_files
	let split_opt | hcc && split_objs = [ "-DUSE_SPLIT_MARKERS" ]
		      | otherwise         = [ ]

	excessPrecision <- readIORef v_Excess_precision
	runSomething "C Compiler"
	 (unwords ([ cc, "-x", "c", input_fn, "-o", output_fn ]
		   ++ md_c_flags
		   ++ (if cc_phase == HCc && mangle
			 then md_regd_c_flags
			 else [])
		   ++ [ verb, "-S", "-Wimplicit", opt_flag ]
		   ++ [ "-D__GLASGOW_HASKELL__="++cProjectVersionInt ]
		   ++ cc_opts
		   ++ split_opt
		   ++ (if excessPrecision then [] else [ "-ffloat-store" ])
		   ++ include_paths
		   ++ pkg_extra_cc_opts
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
	    then do n_regs <- dynFlag stolen_x86_regs
		    return [ show n_regs ]
	    else return []
#if defined(mingw32_TARGET_OS) && defined(MINIMAL_UNIX_DEPS)
       perl_path <- prependToolDir ("perl")
       let real_mangler = unwords [perl_path, mangler]
#else
       let real_mangler = mangler
#endif
       runSomething "Assembly Mangler"
	(unwords (real_mangler : mangler_opts
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

#if defined(mingw32_TARGET_OS) && defined(MINIMAL_UNIX_DEPS)
        perl_path <- prependToolDir ("perl")
        let real_splitter = unwords [perl_path, splitter]
#else
        let real_splitter = splitter
#endif
	runSomething "Split Assembly File"
	 (unwords [ real_splitter
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
  = do	as <- readIORef v_Pgm_a >>= prependToolDir >>= appendInstallDir
        as_opts <- getOpts opt_a

        cmdline_include_paths <- readIORef v_Include_paths
        let cmdline_include_flags = map (\p -> "-I"++p) cmdline_include_paths
        runSomething "Assembler"
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
		    runSomething "Assembler" 
			    (unwords (as : as_opts
				      ++ [ "-c", "-o", real_o, input_s ]
			    ))
	
	mapM_ assemble_file [1..n]
	return True

-----------------------------------------------------------------------------
-- MoveBinary sort-of-phase
-- After having produced a binary, move it somewhere else and generate a
-- wrapper script calling the binary. Currently, we need this only in 
-- a parallel way (i.e. in GUM), because PVM expects the binary in a
-- central directory.
-- This is called from doLink below, after linking. I haven't made it
-- a separate phase to minimise interfering with other modules, and
-- we don't need the generality of a phase (MoveBinary is always
-- done after linking and makes only sense in a parallel setup)   -- HWL

run_phase_MoveBinary input_fn
  = do	
        top_dir <- readIORef v_TopDir
        pvm_root <- getEnv "PVM_ROOT"
        pvm_arch <- getEnv "PVM_ARCH"
        let 
           pvm_executable_base = "=" ++ input_fn
           pvm_executable = pvm_root ++ "/bin/" ++ pvm_arch ++ "/" ++ pvm_executable_base
           sysMan = top_dir ++ "/ghc/rts/parallel/SysMan";
        -- nuke old binary; maybe use configur'ed names for cp and rm?
        system ("rm -f " ++ pvm_executable)
        -- move the newly created binary into PVM land
        system ("cp -p " ++ input_fn ++ " " ++ pvm_executable)
        -- generate a wrapper script for running a parallel prg under PVM
        writeFile input_fn (mk_pvm_wrapper_script pvm_executable pvm_executable_base sysMan)
	return True

-- generates a Perl skript starting a parallel prg under PVM
mk_pvm_wrapper_script :: String -> String -> String -> String
mk_pvm_wrapper_script pvm_executable pvm_executable_base sysMan = unlines $
 [
  "eval 'exec perl -S $0 ${1+\"$@\"}'", 
  "  if $running_under_some_shell;",
  "# =!=!=!=!=!=!=!=!=!=!=!",
  "# This script is automatically generated: DO NOT EDIT!!!",
  "# Generated by Glasgow Haskell Compiler",
  "# ngoqvam choHbogh vaj' vIHoHnISbej !!!!",
  "#",
  "$pvm_executable      = '" ++ pvm_executable ++ "';",
  "$pvm_executable_base = '" ++ pvm_executable_base ++ "';",
  "$SysMan = '" ++ sysMan ++ "';",
  "",
  {- ToDo: add the magical shortcuts again iff we actually use them -- HWL
  "# first, some magical shortcuts to run "commands" on the binary",
  "# (which is hidden)",
  "if ($#ARGV == 1 && $ARGV[0] eq '+RTS' && $ARGV[1] =~ /^--((size|file|strip|rm|nm).*)/ ) {",
  "    local($cmd) = $1;",
  "    system("$cmd $pvm_executable");",
  "    exit(0); # all done",
  "}", -}
  "",
  "# Now, run the real binary; process the args first",
  "$ENV{'PE'} = $pvm_executable_base;", --  ++ pvm_executable_base,
  "$debug = '';",
  "$nprocessors = 0; # the default: as many PEs as machines in PVM config",
  "@nonPVM_args = ();",
  "$in_RTS_args = 0;",
  "",
  "args: while ($a = shift(@ARGV)) {",
  "    if ( $a eq '+RTS' ) {",
  "	$in_RTS_args = 1;",
  "    } elsif ( $a eq '-RTS' ) {",
  "	$in_RTS_args = 0;",
  "    }",
  "    if ( $a eq '-d' && $in_RTS_args ) {",
  "	$debug = '-';",
  "    } elsif ( $a =~ /^-qN(\\d+)/ && $in_RTS_args ) {",
  "	$nprocessors = $1;",
  "    } elsif ( $a =~ /^-qp(\\d+)/ && $in_RTS_args ) {",
  "	$nprocessors = $1;",
  "    } else {",
  "	push(@nonPVM_args, $a);",
  "    }",
  "}",
  "",
  "local($return_val) = 0;",
  "# Start the parallel execution by calling SysMan",
  "system(\"$SysMan $debug $pvm_executable $nprocessors @nonPVM_args\");",
  "$return_val = $?;",
  "# ToDo: fix race condition moving files and flushing them!!",
  "system(\"cp $ENV{'HOME'}/$pvm_executable_base.???.gr .\") if -f \"$ENV{'HOME'}/$pvm_executable_base.002.gr\";",
  "exit($return_val);"
 ]

-----------------------------------------------------------------------------
-- Complain about non-dynamic flags in OPTIONS pragmas

checkProcessArgsResult flags basename suff
  = do when (not (null flags)) (throwDyn (ProgramError (
           basename ++ "." ++ suff 
           ++ ": static flags are not allowed in {-# OPTIONS #-} pragmas:\n\t" 
           ++ unwords flags)) (ExitFailure 1))

-----------------------------------------------------------------------------
-- Linking

doLink :: [String] -> IO ()
doLink o_files = do
    ln <- readIORef v_Pgm_l >>= prependToolDir >>= appendInstallDir
    verb <- getVerbFlag
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
    runSomething "Linker"
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
    -- parallel only: move binary to another dir -- HWL
    ways_ <- readIORef v_Ways
    when (WayPar `elem` ways_) (do 
                                  success <- run_phase_MoveBinary output_fn
                                  if success then return ()
                                             else throwDyn (InstallationError ("cannot move binary to PVM dir")))

-----------------------------------------------------------------------------
-- Making a DLL

-- only for Win32, but bits that are #ifdefed in doLn are still #ifdefed here
-- in a vain attempt to aid future portability
doMkDLL :: [String] -> IO ()
doMkDLL o_files = do
    ln <- readIORef v_Pgm_dll >>= prependToolDir >>= appendInstallDir
    verb <- getVerbFlag
    static <- readIORef v_Static
    let imp = if static then "" else "_imp"
    no_hs_main <- readIORef v_NoHsMain

    o_file <- readIORef v_Output_file
    let output_fn = case o_file of { Just s -> s; Nothing -> "HSdll.dll"; }

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

	-- opts from -optdll-<blah>
    extra_ld_opts <- getStaticOpts v_Opt_dll

    rts_pkg <- getPackageDetails ["rts"]
    std_pkg <- getPackageDetails ["std"]
#ifdef mingw32_TARGET_OS
    let extra_os = if static || no_hs_main
                   then []
                   else [ head (library_dirs (head rts_pkg)) ++ "/Main.dll_o",
                          head (library_dirs (head std_pkg)) ++ "/PrelMain.dll_o" ]
#endif
    (md_c_flags, _) <- machdepCCOpts
    runSomething "DLL creator"
       (unwords
	 ([ ln, verb, "-o", output_fn ]
	 ++ md_c_flags
	 ++ o_files
#ifdef mingw32_TARGET_OS
	 ++ extra_os
	 ++ [ "--target=i386-mingw32" ]
#endif
	 ++ extra_ld_inputs
	 ++ lib_path_opts
	 ++ lib_opts
	 ++ pkg_lib_path_opts
	 ++ pkg_lib_opts
	 ++ pkg_extra_ld_opts
         ++ (case findPS (packString (concat extra_ld_opts)) (packString "--def") of
               Nothing -> [ "--export-all" ]
	       Just _  -> [ "" ])
	 ++ extra_ld_opts
	)
       )

-----------------------------------------------------------------------------
-- Just preprocess a file, put the result in a temp. file (used by the
-- compilation manager during the summary phase).

preprocess :: FilePath -> IO FilePath
preprocess filename =
  ASSERT(haskellish_src_file filename) 
  do init_dyn_flags <- readIORef v_InitDynFlags
     writeIORef v_DynFlags init_dyn_flags
     pipeline <- genPipeline (StopBefore Hsc) ("preprocess") False 
			defaultHscLang filename
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

-- NB.  No old interface can also mean that the source has changed.

compile :: GhciMode                -- distinguish batch from interactive
        -> ModSummary              -- summary, including source
	-> Bool			   -- True <=> source unchanged
	-> Bool			   -- True <=> have object
        -> Maybe ModIface          -- old interface, if available
        -> HomeSymbolTable         -- for home module ModDetails
	-> HomeIfaceTable	   -- for home module Ifaces
        -> PersistentCompilerState -- persistent compiler state
        -> IO CompResult

data CompResult
   = CompOK   PersistentCompilerState	-- updated PCS
              ModDetails  -- new details (HST additions)
              ModIface    -- new iface   (HIT additions)
              (Maybe Linkable)
                       -- new code; Nothing => compilation was not reqd
                       -- (old code is still valid)

   | CompErrs PersistentCompilerState	-- updated PCS


compile ghci_mode summary source_unchanged have_object 
	old_iface hst hit pcs = do 
   init_dyn_flags <- readIORef v_InitDynFlags
   writeIORef v_DynFlags init_dyn_flags

   showPass init_dyn_flags 
	(showSDoc (text "Compiling" <+> ppr (name_of_summary summary)))

   let verb = verbosity init_dyn_flags
   let location   = ms_location summary
   let input_fn   = unJust "compile:hs" (ml_hs_file location) 
   let input_fnpp = unJust "compile:hspp" (ml_hspp_file location)

   when (verb >= 2) (hPutStrLn stderr ("compile: input file " ++ input_fnpp))

   opts <- getOptionsFromSource input_fnpp
   processArgs dynamic_flags opts []
   dyn_flags <- readIORef v_DynFlags

   let hsc_lang = hscLang dyn_flags
       (basename, _) = splitFilename input_fn
       
   output_fn <- case hsc_lang of
		    HscAsm         -> newTempName (phaseInputExt As)
		    HscC           -> newTempName (phaseInputExt HCc)
        	    HscJava        -> newTempName "java" -- ToDo
		    HscILX         -> return (basename ++ ".ilx") 	-- newTempName "ilx"	-- ToDo
		    HscInterpreted -> return (error "no output file")

   let dyn_flags' = dyn_flags { hscOutName = output_fn,
				hscStubCOutName = basename ++ "_stub.c",
				hscStubHOutName = basename ++ "_stub.h" }

   -- figure out which header files to #include in a generated .hc file
   c_includes <- getPackageCIncludes
   cmdline_includes <- dynFlag cmdlineHcIncludes -- -#include options

   let cc_injects = unlines (map mk_include 
                                 (c_includes ++ reverse cmdline_includes))
       mk_include h_file = 
	case h_file of 
           '"':_{-"-} -> "#include "++h_file
           '<':_      -> "#include "++h_file
           _          -> "#include \""++h_file++"\""

   writeIORef v_HCHeader cc_injects

   -- run the compiler
   hsc_result <- hscMain ghci_mode dyn_flags'
			 (ms_mod summary) location
			 source_unchanged have_object old_iface hst hit pcs

   case hsc_result of
      HscFail pcs -> return (CompErrs pcs)

      HscNoRecomp pcs details iface -> return (CompOK pcs details iface Nothing)

      HscRecomp pcs details iface
	stub_h_exists stub_c_exists maybe_interpreted_code -> do
	   
	   let 
	   maybe_stub_o <- compileStub dyn_flags' stub_c_exists
	   let stub_unlinked = case maybe_stub_o of
				  Nothing -> []
				  Just stub_o -> [ DotO stub_o ]

	   (hs_unlinked, unlinked_time) <-
	     case hsc_lang of

		-- in interpreted mode, just return the compiled code
		-- as our "unlinked" object.
		HscInterpreted -> 
		    case maybe_interpreted_code of
		       Just (bcos,itbl_env) -> do tm <- getClockTime 
                                                  return ([BCOs bcos itbl_env], tm)
		       Nothing -> panic "compile: no interpreted code"

		-- we're in batch mode: finish the compilation pipeline.
		_other -> do pipe <- genPipeline (StopBefore Ln) "" True 
					hsc_lang output_fn
                             -- runPipeline takes input_fn so it can split off 
                             -- the base name and use it as the base of 
                             -- the output object file.
                             let (basename, suffix) = splitFilename input_fn
			     o_file <- pipeLoop pipe output_fn False False 
                                                basename suffix
                             o_time <- getModificationTime o_file
			     return ([DotO o_file], o_time)

	   let linkable = LM unlinked_time (moduleName (ms_mod summary)) 
			     (hs_unlinked ++ stub_unlinked)

	   return (CompOK pcs details iface (Just linkable))


-----------------------------------------------------------------------------
-- stub .h and .c files (for foreign export support)

compileStub dflags stub_c_exists
  | not stub_c_exists = return Nothing
  | stub_c_exists = do
	-- compile the _stub.c file w/ gcc
	let stub_c = hscStubCOutName dflags
	pipeline <- genPipeline (StopBefore Ln) "" True defaultHscLang stub_c
	stub_o <- runPipeline pipeline stub_c False{-no linking-} 
			False{-no -o option-}

	return (Just stub_o)
