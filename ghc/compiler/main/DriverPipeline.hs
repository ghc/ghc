-----------------------------------------------------------------------------
--
-- GHC Driver
--
-- (c) The University of Glasgow 2002
--
-----------------------------------------------------------------------------

#include "../includes/config.h"

module DriverPipeline (

	-- Interfaces for the batch-mode driver
   runPipeline, staticLink,

	-- Interfaces for the compilation manager (interpreted/batch-mode)
   preprocess, 
   compile, CompResult(..), 
   link, 

        -- DLL building
   doMkDLL
  ) where

#include "HsVersions.h"

import Packages
import GetImports
import DriverState
import DriverUtil
import DriverMkDepend
import DriverPhases
import DriverFlags
import SysTools		( newTempName, addFilesToClean, getSysMan, copy )
import qualified SysTools	
import HscMain
import Finder
import HscTypes
import Outputable
import Module
import ErrUtils
import CmdLineOpts
import Config
import Panic
import Util
import BasicTypes	( SuccessFlag(..) )
import Maybes		( expectJust )

import ParserCoreUtils ( getCoreModuleName )

import EXCEPTION
import DATA_IOREF	( readIORef, writeIORef )

#ifdef GHCI
import Time 		( getClockTime )
#endif
import Directory
import System
import IO
import Monad
import Maybe


-- ---------------------------------------------------------------------------
-- Pre-process

-- Just preprocess a file, put the result in a temp. file (used by the
-- compilation manager during the summary phase).

preprocess :: FilePath -> IO FilePath
preprocess filename =
  ASSERT(haskellish_src_file filename) 
  do restoreDynFlags	-- Restore to state of last save
     runPipeline (StopBefore Hsc) ("preprocess") 
	False{-temporary output file-}
	Nothing{-no specific output file-}
	filename

-- ---------------------------------------------------------------------------
-- Compile

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
	-> Module
	-> ModLocation
	-> Bool			   -- True <=> source unchanged
	-> Bool			   -- True <=> have object
        -> Maybe ModIface          -- old interface, if available
        -> HomePackageTable        -- For home-module stuff
        -> PersistentCompilerState -- persistent compiler state
        -> IO CompResult

data CompResult
   = CompOK   PersistentCompilerState	-- Updated PCS
              ModDetails 		-- New details
              ModIface			-- New iface
              (Maybe Linkable)	-- New code; Nothing => compilation was not reqd
		                --			(old code is still valid)

   | CompErrs PersistentCompilerState	-- Updated PCS


compile ghci_mode this_mod location
	source_unchanged have_object 
	old_iface hpt pcs = do 

   dyn_flags <- restoreDynFlags		-- Restore to the state of the last save

   showPass dyn_flags 
	(showSDoc (text "Compiling" <+> ppr this_mod))

   let verb	  = verbosity dyn_flags
   let input_fn   = expectJust "compile:hs" (ml_hs_file location) 
   let input_fnpp = expectJust "compile:hspp" (ml_hspp_file location)
   let mod_name   = moduleName this_mod

   when (verb >= 2) (hPutStrLn stderr ("compile: input file " ++ input_fnpp))

   opts <- getOptionsFromSource input_fnpp
   processArgs dynamic_flags opts []
   dyn_flags <- getDynFlags

   let hsc_lang      = hscLang dyn_flags
       (basename, _) = splitFilename input_fn
       
   keep_hc <- readIORef v_Keep_hc_files
#ifdef ILX
   keep_il <- readIORef v_Keep_il_files
#endif
   keep_s  <- readIORef v_Keep_s_files

   output_fn <- 
	case hsc_lang of
	   HscAsm  | keep_s    -> return (basename ++ '.':phaseInputExt As)
		   | otherwise -> newTempName (phaseInputExt As)
	   HscC    | keep_hc   -> return (basename ++ '.':phaseInputExt HCc)
		   | otherwise -> newTempName (phaseInputExt HCc)
           HscJava             -> newTempName "java" -- ToDo
#ifdef ILX
	   HscILX  | keep_il   -> return (basename ++ '.':phaseInputExt Ilasm)
                   | otherwise -> newTempName (phaseInputExt Ilx2Il) 	
#endif
	   HscInterpreted      -> return (error "no output file")
           HscNothing	       -> return (error "no output file")

   let dyn_flags' = dyn_flags { hscOutName = output_fn,
				hscStubCOutName = basename ++ "_stub.c",
				hscStubHOutName = basename ++ "_stub.h",
				extCoreName = basename ++ ".hcr" }

   -- -no-recomp should also work with --make
   do_recomp <- readIORef v_Recomp
   let source_unchanged' = source_unchanged && do_recomp
       hsc_env = HscEnv { hsc_mode = ghci_mode,
			  hsc_dflags = dyn_flags',
			  hsc_HPT    = hpt }

   -- run the compiler
   hsc_result <- hscMain hsc_env pcs this_mod location
			 source_unchanged' have_object old_iface

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
#ifdef GHCI
		       Just comp_bc -> do tm <- getClockTime 
                                          return ([BCOs comp_bc], tm)
#endif
		       Nothing -> panic "compile: no interpreted code"

		-- we're in batch mode: finish the compilation pipeline.
		_other -> do
		   let object_filename = ml_obj_file location
		       object_dir = directoryOf object_filename

		   -- create the object dir if it doesn't exist
		   createDirectoryHierarchy object_dir

		   runPipeline (StopBefore Ln) ""
			True (Just object_filename) output_fn

		   o_time <- getModificationTime object_filename
		   return ([DotO object_filename], o_time)

	   let linkable = LM unlinked_time mod_name
			     (hs_unlinked ++ stub_unlinked)

	   return (CompOK pcs details iface (Just linkable))

-----------------------------------------------------------------------------
-- stub .h and .c files (for foreign export support)

compileStub dflags stub_c_exists
  | not stub_c_exists = return Nothing
  | stub_c_exists = do
	-- compile the _stub.c file w/ gcc
	let stub_c = hscStubCOutName dflags
	stub_o <- runPipeline (StopBefore Ln) "stub-compile"
			True{-persistent output-} 
			Nothing{-no specific output file-}
			stub_c
	return (Just stub_o)


-- ---------------------------------------------------------------------------
-- Link

link :: GhciMode		-- interactive or batch
     -> DynFlags		-- dynamic flags
     -> Bool			-- attempt linking in batch mode?
     -> HomePackageTable	-- what to link
     -> IO SuccessFlag

-- For the moment, in the batch linker, we don't bother to tell doLink
-- which packages to link -- it just tries all that are available.
-- batch_attempt_linking should only be *looked at* in batch mode.  It
-- should only be True if the upsweep was successful and someone
-- exports main, i.e., we have good reason to believe that linking
-- will succeed.

#ifdef GHCI
link Interactive dflags batch_attempt_linking hpt
    = do -- Not Linking...(demand linker will do the job)
	 return Succeeded
#endif

link Batch dflags batch_attempt_linking hpt
   | batch_attempt_linking
   = do 
	let 
	    home_mod_infos = moduleEnvElts hpt

	    -- the packages we depend on
	    pkg_deps  = concatMap (dep_pkgs . mi_deps . hm_iface) home_mod_infos

	    -- the linkables to link
	    linkables = map hm_linkable home_mod_infos

        when (verb >= 3) $ do
	     hPutStrLn stderr "link: linkables are ..."
             hPutStrLn stderr (showSDoc (vcat (map ppr linkables)))

	-- check for the -no-link flag
	omit_linking <- readIORef v_NoLink
	if omit_linking 
	  then do when (verb >= 3) $
		    hPutStrLn stderr "link(batch): linking omitted (-no-link flag given)."
	          return Succeeded
	  else do

	when (verb >= 1) $
             hPutStrLn stderr "Linking ..."

	let getOfiles (LM _ _ us) = map nameOfObject (filter isObject us)
	    obj_files = concatMap getOfiles linkables

	-- Don't showPass in Batch mode; doLink will do that for us.
        staticLink obj_files pkg_deps

        when (verb >= 3) (hPutStrLn stderr "link: done")

	-- staticLink only returns if it succeeds
        return Succeeded

   | otherwise
   = do when (verb >= 3) $ do
	    hPutStrLn stderr "link(batch): upsweep (partially) failed OR"
            hPutStrLn stderr "   Main.main not exported; not linking."
        return Succeeded
   where
      verb = verbosity dflags
      
-- ---------------------------------------------------------------------------
-- Run a compilation pipeline, consisting of multiple phases.

runPipeline
  :: GhcMode		-- when to stop
  -> String		-- "stop after" flag
  -> Bool		-- final output is persistent?
  -> Maybe FilePath	-- where to put the output, optionally
  -> FilePath 		-- input filename
  -> IO FilePath	-- output filename

runPipeline todo stop_flag keep_output maybe_output_filename input_fn
  = do
  split <- readIORef v_Split_object_files
  let (basename, suffix) = splitFilename input_fn
      start_phase = startPhase suffix

      stop_phase = case todo of 
			StopBefore As | split -> SplitAs
			StopBefore phase      -> phase
			DoMkDependHS	      -> Ln
			DoLink                -> Ln
			DoMkDLL               -> Ln

  -- We want to catch cases of "you can't get there from here" before
  -- we start the pipeline, because otherwise it will just run off the
  -- end.
  --
  -- There is a partial ordering on phases, where A < B iff A occurs
  -- before B in a normal compilation pipeline.
  --
  when (not (start_phase `happensBefore` stop_phase)) $
	throwDyn (UsageError 
		    ("flag `" ++ stop_flag
		     ++ "' is incompatible with source file `"
		     ++ input_fn ++ "'"))

  -- generate a function which will be used to calculate output file names
  -- as we go along.
  get_output_fn <- genOutputFilenameFunc keep_output maybe_output_filename
			stop_phase basename

  -- and execute the pipeline...
  output_fn <- pipeLoop start_phase stop_phase input_fn basename suffix 
		 get_output_fn

  -- sometimes, a compilation phase doesn't actually generate any output
  -- (eg. the CPP phase when -fcpp is not turned on).  If we end on this
  -- stage, but we wanted to keep the output, then we have to explicitly
  -- copy the file.
  if keep_output
	then do final_fn <- get_output_fn stop_phase
	        when (final_fn /= output_fn) $
	 	  copy ("Copying `" ++ output_fn ++ "' to `" ++ final_fn
			++ "'") output_fn final_fn
	        return final_fn
	else
	     return output_fn


pipeLoop :: Phase -> Phase -> FilePath -> String -> Suffix
  -> (Phase -> IO FilePath) -> IO FilePath
pipeLoop phase stop_phase input_fn orig_basename orig_suff get_output_fn
  | phase == stop_phase  =  return input_fn  -- all done
  | otherwise = do
	maybe_next_phase <- runPhase phase orig_basename orig_suff input_fn
				get_output_fn
	case maybe_next_phase of
	  (Nothing, output_fn) -> return output_fn
	  (Just next_phase, output_fn) -> 
		pipeLoop next_phase stop_phase output_fn
			orig_basename orig_suff get_output_fn

  
genOutputFilenameFunc :: Bool -> Maybe FilePath -> Phase -> String
  -> IO (Phase{-next phase-} -> IO FilePath)
genOutputFilenameFunc keep_output maybe_output_filename stop_phase basename
 = do
   hcsuf      <- readIORef v_HC_suf
   osuf       <- readIORef v_Object_suf
   keep_hc    <- readIORef v_Keep_hc_files
#ifdef ILX
   keep_il    <- readIORef v_Keep_il_files
   keep_ilx   <- readIORef v_Keep_ilx_files
#endif
   keep_raw_s <- readIORef v_Keep_raw_s_files
   keep_s     <- readIORef v_Keep_s_files
   let
        myPhaseInputExt HCc | Just s <- hcsuf = s
        myPhaseInputExt Ln    = osuf
        myPhaseInputExt other = phaseInputExt other

	func next_phase
     		| next_phase == stop_phase
                     = case maybe_output_filename of
			     Just file -> return file
			     Nothing | keep_output -> return persistent
				     | otherwise   -> newTempName suffix
			-- sometimes, we keep output from intermediate stages
     		| otherwise
     		     = case next_phase of
     			     Ln                  -> return persistent
     			     Mangle | keep_raw_s -> return persistent
     			     As     | keep_s     -> return persistent
     			     HCc    | keep_hc    -> return persistent
     			     _other              -> newTempName suffix
	   where
		suffix = myPhaseInputExt next_phase
		persistent = basename ++ '.':suffix

   return func


-- -----------------------------------------------------------------------------
-- Each phase in the pipeline returns the next phase to execute, and the
-- name of the file in which the output was placed.
--
-- We must do things dynamically this way, because we often don't know
-- what the rest of the phases will be until part-way through the
-- compilation: for example, an {-# OPTIONS -fasm #-} at the beginning
-- of a source file can change the latter stages of the pipeline from
-- taking the via-C route to using the native code generator.

runPhase :: Phase
	  -> String	-- basename of original input source
	  -> String	-- its extension
	  -> FilePath	-- name of file which contains the input to this phase.
	  -> (Phase -> IO FilePath)	-- how to calculate the output filename
	  -> IO (Maybe Phase,   -- next phase
		 FilePath)	-- output filename

-------------------------------------------------------------------------------
-- Unlit phase 

runPhase Unlit _basename _suff input_fn get_output_fn
  = do unlit_flags <- getOpts opt_L
       -- The -h option passes the file name for unlit to put in a #line directive
       output_fn <- get_output_fn Cpp

       SysTools.runUnlit (map SysTools.Option unlit_flags ++
       			  [ SysTools.Option     "-h"
 			  , SysTools.Option     input_fn
			  , SysTools.FileOption "" input_fn
			  , SysTools.FileOption "" output_fn
			  ])

       return (Just Cpp, output_fn)

-------------------------------------------------------------------------------
-- Cpp phase 

runPhase Cpp basename suff input_fn get_output_fn
  = do src_opts <- getOptionsFromSource input_fn
       unhandled_flags <- processArgs dynamic_flags src_opts []
       checkProcessArgsResult unhandled_flags basename suff

       do_cpp <- dynFlag cppFlag
       if not do_cpp then
           -- no need to preprocess CPP, just pass input file along
	   -- to the next phase of the pipeline.
          return (Just HsPp, input_fn)
	else do
	    hscpp_opts	    <- getOpts opt_P
       	    hs_src_cpp_opts <- readIORef v_Hs_source_cpp_opts

	    cmdline_include_paths <- readIORef v_Include_paths

	    pkg_include_dirs <- getPackageIncludePath []
	    let include_paths = foldr (\ x xs -> "-I" : x : xs) []
				  (cmdline_include_paths ++ pkg_include_dirs)

	    verb <- getVerbFlag
	    (md_c_flags, _) <- machdepCCOpts

	    output_fn <- get_output_fn HsPp

	    SysTools.runCpp ([SysTools.Option verb]
			    ++ map SysTools.Option include_paths
			    ++ map SysTools.Option hs_src_cpp_opts
			    ++ map SysTools.Option hscpp_opts
			    ++ map SysTools.Option md_c_flags
			    ++ [ SysTools.Option     "-x"
			       , SysTools.Option     "c"
			       , SysTools.Option     input_fn
	-- We hackily use Option instead of FileOption here, so that the file
	-- name is not back-slashed on Windows.  cpp is capable of
	-- dealing with / in filenames, so it works fine.  Furthermore
	-- if we put in backslashes, cpp outputs #line directives
	-- with *double* backslashes.   And that in turn means that
	-- our error messages get double backslashes in them.
	-- In due course we should arrange that the lexer deals
	-- with these \\ escapes properly.
			       , SysTools.Option     "-o"
			       , SysTools.FileOption "" output_fn
			       ])

	    return (Just HsPp, output_fn)

-------------------------------------------------------------------------------
-- HsPp phase 

runPhase HsPp basename suff input_fn get_output_fn
  = do do_pp   <- dynFlag ppFlag
       if not do_pp then
           -- no need to preprocess, just pass input file along
	   -- to the next phase of the pipeline.
          return (Just Hsc, input_fn)
	else do
	    hspp_opts	   <- getOpts opt_F
       	    hs_src_pp_opts <- readIORef v_Hs_source_pp_opts
	    let orig_fn = basename ++ '.':suff
	    output_fn <- get_output_fn Hsc
	    SysTools.runPp ( [ SysTools.Option     orig_fn
			     , SysTools.Option     input_fn
			     , SysTools.FileOption "" output_fn
			     ] ++
			     map SysTools.Option hs_src_pp_opts ++
			     map SysTools.Option hspp_opts
			   )
	    return (Just Hsc, output_fn)

-----------------------------------------------------------------------------
-- Hsc phase

-- Compilation of a single module, in "legacy" mode (_not_ under
-- the direction of the compilation manager).
runPhase Hsc basename suff input_fn get_output_fn = do
  todo <- readIORef v_GhcMode
  if todo == DoMkDependHS then do
       doMkDependHSPhase basename suff input_fn
       return (Nothing, input_fn)  -- Ln is a dummy stop phase 

   else do
      -- normal Hsc mode, not mkdependHS

  -- we add the current directory (i.e. the directory in which
  -- the .hs files resides) to the import path, since this is
  -- what gcc does, and it's probably what you want.
	let current_dir = directoryOf basename
	
	paths <- readIORef v_Include_paths
	writeIORef v_Include_paths (current_dir : paths)
	
  -- gather the imports and module name
        (_,_,mod_name) <- 
            if extcoreish_suffix suff
	     then do
               -- no explicit imports in ExtCore input.
	       m <- getCoreModuleName input_fn
	       return ([], [], mkModuleName m)
	     else 
  	       getImportsFromFile input_fn

  -- build a ModLocation to pass to hscMain.
	let (path,file) = splitFilenameDir basename
	(mod, location') <- mkHomeModLocation mod_name True path file suff

  -- take -ohi into account if present
	ohi <- readIORef v_Output_hi
	let location | Just fn <- ohi = location'{ ml_hi_file = fn }
		     | otherwise      = location'

  -- figure out if the source has changed, for recompilation avoidance.
  -- only do this if we're eventually going to generate a .o file.
  -- (ToDo: do when generating .hc files too?)
  --
  -- Setting source_unchanged to True means that M.o seems
  -- to be up to date wrt M.hs; so no need to recompile unless imports have
  -- changed (which the compiler itself figures out).
  -- Setting source_unchanged to False tells the compiler that M.o is out of
  -- date wrt M.hs (or M.o doesn't exist) so we must recompile regardless.
	do_recomp   <- readIORef v_Recomp
	expl_o_file <- readIORef v_Output_file

	let o_file -- if the -o option is given and IT IS THE OBJECT FILE FOR
		   -- THIS COMPILATION, then use that to determine if the 
		   -- source is unchanged.
		| Just x <- expl_o_file, todo == StopBefore Ln  =  x
		| otherwise = ml_obj_file location

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

  -- get the DynFlags
        dyn_flags <- getDynFlags
	let hsc_lang = hscLang dyn_flags
	split <- readIORef v_Split_object_files

	let next_phase = case hsc_lang of
				HscC -> HCc
				HscAsm | split -> SplitMangle
				       | otherwise -> As
				HscNothing -> HCc

	output_fn <- get_output_fn next_phase

        let dyn_flags' = dyn_flags { hscOutName = output_fn,
		   		     hscStubCOutName = basename ++ "_stub.c",
				     hscStubHOutName = basename ++ "_stub.h",
				     extCoreName = basename ++ ".hcr" }
	    hsc_env = HscEnv { hsc_mode = OneShot,
			       hsc_dflags = dyn_flags',
			       hsc_HPT    = emptyHomePackageTable }
			

  -- run the compiler!
        pcs <- initPersistentCompilerState
	result <- hscMain hsc_env pcs mod
			  location{ ml_hspp_file=Just input_fn }
			  source_unchanged
			  False
			  Nothing	 -- no iface

	case result of

	    HscFail pcs -> throwDyn (PhaseFailed "hsc" (ExitFailure 1))

            HscNoRecomp pcs details iface -> do
		SysTools.touch "Touching object file" o_file
		return (Nothing, output_fn)

	    HscRecomp _pcs _details _iface stub_h_exists stub_c_exists
		      _maybe_interpreted_code -> do

		-- deal with stubs
		maybe_stub_o <- compileStub dyn_flags' stub_c_exists
		case maybe_stub_o of
		      Nothing -> return ()
		      Just stub_o -> add v_Ld_inputs stub_o
		case hscLang dyn_flags of
                      HscNothing -> return (Nothing, output_fn)
		      _ -> return (Just next_phase, output_fn)

-----------------------------------------------------------------------------
-- Cc phase

-- we don't support preprocessing .c files (with -E) now.  Doing so introduces
-- way too many hacks, and I can't say I've ever used it anyway.

runPhase cc_phase basename suff input_fn get_output_fn
   | cc_phase == Cc || cc_phase == HCc
   = do	cc_opts <- getOpts opt_c
       	cmdline_include_paths <- readIORef v_Include_paths

	split  <- readIORef v_Split_object_files
	mangle <- readIORef v_Do_asm_mangling

        let hcc = cc_phase == HCc

	    next_phase
		| hcc && mangle     = Mangle
		| otherwise         = As

	output_fn <- get_output_fn next_phase

	-- HC files have the dependent packages stamped into them
	pkgs <- if hcc then getHCFilePackages input_fn else return []

	-- add package include paths even if we're just compiling .c
	-- files; this is the Value Add(TM) that using ghc instead of
	-- gcc gives you :)
        pkg_include_dirs <- getPackageIncludePath pkgs
        let include_paths = foldr (\ x xs -> "-I" : x : xs) []
			      (cmdline_include_paths ++ pkg_include_dirs)

	mangle <- readIORef v_Do_asm_mangling
	(md_c_flags, md_regd_c_flags) <- machdepCCOpts

        verb <- getVerbFlag

	o2 <- readIORef v_minus_o2_for_C
	let opt_flag | o2        = "-O2"
		     | otherwise = "-O"

	pkg_extra_cc_opts <- getPackageExtraCcOpts pkgs

	split_objs <- readIORef v_Split_object_files
	let split_opt | hcc && split_objs = [ "-DUSE_SPLIT_MARKERS" ]
		      | otherwise         = [ ]

	excessPrecision <- readIORef v_Excess_precision

	-- force the C compiler to interpret this file as C when
	-- compiling .hc files, by adding the -x c option.
	let langopt
		| cc_phase == HCc = [ SysTools.Option "-x", SysTools.Option "c"]
		| otherwise       = [ ]

	SysTools.runCc (langopt ++
			[ SysTools.FileOption "" input_fn
			, SysTools.Option "-o"
			, SysTools.FileOption "" output_fn
			]
		       ++ map SysTools.Option (
		          md_c_flags
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

	return (Just next_phase, output_fn)

	-- ToDo: postprocess the output from gcc

-----------------------------------------------------------------------------
-- Mangle phase

runPhase Mangle _basename _suff input_fn get_output_fn
   = do mangler_opts <- getOpts opt_m
        machdep_opts <- if (prefixMatch "i386" cTARGETPLATFORM)
			  then do n_regs <- dynFlag stolen_x86_regs
			          return [ show n_regs ]
		          else return []

	split <- readIORef v_Split_object_files
	let next_phase
		| split = SplitMangle
		| otherwise = As
	output_fn <- get_output_fn next_phase

	SysTools.runMangle (map SysTools.Option mangler_opts
		          ++ [ SysTools.FileOption "" input_fn
			     , SysTools.FileOption "" output_fn
			     ]
			  ++ map SysTools.Option machdep_opts)

	return (Just next_phase, output_fn)

-----------------------------------------------------------------------------
-- Splitting phase

runPhase SplitMangle _basename _suff input_fn get_output_fn
  = do  -- tmp_pfx is the prefix used for the split .s files
	-- We also use it as the file to contain the no. of split .s files (sigh)
	split_s_prefix <- SysTools.newTempName "split"
	let n_files_fn = split_s_prefix

	SysTools.runSplit [ SysTools.FileOption "" input_fn
			  , SysTools.FileOption "" split_s_prefix
			  , SysTools.FileOption "" n_files_fn
			  ]

	-- Save the number of split files for future references
	s <- readFile n_files_fn
	let n_files = read s :: Int
	writeIORef v_Split_info (split_s_prefix, n_files)

	-- Remember to delete all these files
	addFilesToClean [ split_s_prefix ++ "__" ++ show n ++ ".s"
			| n <- [1..n_files]]

	return (Just SplitAs, "**splitmangle**")  -- we don't use the filename

-----------------------------------------------------------------------------
-- As phase

runPhase As _basename _suff input_fn get_output_fn
  = do	as_opts		      <- getOpts opt_a
        cmdline_include_paths <- readIORef v_Include_paths

	output_fn <- get_output_fn Ln

	SysTools.runAs (map SysTools.Option as_opts
		       ++ [ SysTools.Option ("-I" ++ p) | p <- cmdline_include_paths ]
		       ++ [ SysTools.Option "-c"
		          , SysTools.FileOption "" input_fn
			  , SysTools.Option "-o"
			  , SysTools.FileOption "" output_fn
			  ])

	return (Just Ln, output_fn)


runPhase SplitAs basename _suff _input_fn get_output_fn
  = do  as_opts <- getOpts opt_a

	(split_s_prefix, n) <- readIORef v_Split_info

	odir <- readIORef v_Output_dir
	let real_odir = case odir of
				Nothing -> basename ++ "_split"
				Just d  -> d

	let assemble_file n
	      = do  let input_s  = split_s_prefix ++ "__" ++ show n ++ ".s"
		    let output_o = replaceFilenameDirectory
					(basename ++ "__" ++ show n ++ ".o")
					 real_odir
		    real_o <- osuf_ify output_o
		    SysTools.runAs (map SysTools.Option as_opts ++
		    		    [ SysTools.Option "-c"
				    , SysTools.Option "-o"
				    , SysTools.FileOption "" real_o
				    , SysTools.FileOption "" input_s
				    ])
	
	mapM_ assemble_file [1..n]

	return (Just Ln, "**split_as**") -- we don't use the output file

#ifdef ILX
-----------------------------------------------------------------------------
-- Ilx2Il phase
-- Run ilx2il over the ILX output, getting an IL file

runPhase Ilx2Il _basename _suff input_fn get_output_fn
  = do	ilx2il_opts <- getOpts opt_I
        SysTools.runIlx2il (map SysTools.Option ilx2il_opts
                           ++ [ SysTools.Option "--no-add-suffix-to-assembly",
				SysTools.Option "mscorlib",
				SysTools.Option "-o",
				SysTools.FileOption "" output_fn,
				SysTools.FileOption "" input_fn ])
	return True

-----------------------------------------------------------------------------
-- Ilasm phase
-- Run ilasm over the IL, getting a DLL

runPhase Ilasm _basename _suff input_fn get_output_fn
  = do	ilasm_opts <- getOpts opt_i
        SysTools.runIlasm (map SysTools.Option ilasm_opts
		           ++ [ SysTools.Option "/QUIET",
				SysTools.Option "/DLL",
				SysTools.FileOption "/OUT=" output_fn,
				SysTools.FileOption "" input_fn ])
	return True

#endif /* ILX */

-----------------------------------------------------------------------------
-- MoveBinary sort-of-phase
-- After having produced a binary, move it somewhere else and generate a
-- wrapper script calling the binary. Currently, we need this only in 
-- a parallel way (i.e. in GUM), because PVM expects the binary in a
-- central directory.
-- This is called from staticLink below, after linking. I haven't made it
-- a separate phase to minimise interfering with other modules, and
-- we don't need the generality of a phase (MoveBinary is always
-- done after linking and makes only sense in a parallel setup)   -- HWL

runPhase_MoveBinary input_fn
  = do	
        sysMan   <- getSysMan
        pvm_root <- getEnv "PVM_ROOT"
        pvm_arch <- getEnv "PVM_ARCH"
        let 
           pvm_executable_base = "=" ++ input_fn
           pvm_executable = pvm_root ++ "/bin/" ++ pvm_arch ++ "/" ++ pvm_executable_base
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
  = do when (notNull flags) (throwDyn (ProgramError (
	  showSDoc (hang (text basename <> text ('.':suff) <> char ':')
		      4 (text "unknown flags in  {-# OPTIONS #-} pragma:" <+>
			  hsep (map text flags)))
	)))

-----------------------------------------------------------------------------
-- Look for the /* GHC_PACKAGES ... */ comment at the top of a .hc file

getHCFilePackages :: FilePath -> IO [PackageName]
getHCFilePackages filename =
  EXCEPTION.bracket (openFile filename ReadMode) hClose $ \h -> do
    l <- hGetLine h
    case l of
      '/':'*':' ':'G':'H':'C':'_':'P':'A':'C':'K':'A':'G':'E':'S':rest ->
	  return (map mkPackageName (words rest))
      _other ->
	  return []

-----------------------------------------------------------------------------
-- Static linking, of .o files

-- The list of packages passed to link is the list of packages on
-- which this program depends, as discovered by the compilation
-- manager.  It is combined with the list of packages that the user
-- specifies on the command line with -package flags.  
--
-- In one-shot linking mode, we can't discover the package
-- dependencies (because we haven't actually done any compilation or
-- read any interface files), so the user must explicitly specify all
-- the packages.

staticLink :: [FilePath] -> [PackageName] -> IO ()
staticLink o_files dep_packages = do
    verb       <- getVerbFlag
    static     <- readIORef v_Static
    no_hs_main <- readIORef v_NoHsMain

    -- get the full list of packages to link with, by combining the
    -- explicit packages with the auto packages and all of their
    -- dependencies, and eliminating duplicates.

    o_file <- readIORef v_Output_file
    let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }

    pkg_lib_paths <- getPackageLibraryPath dep_packages
    let pkg_lib_path_opts = map ("-L"++) pkg_lib_paths

    lib_paths <- readIORef v_Library_paths
    let lib_path_opts = map ("-L"++) lib_paths

    pkg_link_opts <- getPackageLinkOpts dep_packages

#ifdef darwin_TARGET_OS
    pkg_framework_paths <- getPackageFrameworkPath dep_packages
    let pkg_framework_path_opts = map ("-F"++) pkg_framework_paths

    framework_paths <- readIORef v_Framework_paths
    let framework_path_opts = map ("-F"++) framework_paths

    pkg_frameworks <- getPackageFrameworks dep_packages
    let pkg_framework_opts = concat [ ["-framework", fw] | fw <- pkg_frameworks ]

    frameworks <- readIORef v_Cmdline_frameworks
    let framework_opts = concat [ ["-framework", fw] | fw <- reverse frameworks ]
	 -- reverse because they're added in reverse order from the cmd line
#endif

	-- probably _stub.o files
    extra_ld_inputs <- readIORef v_Ld_inputs

	-- opts from -optl-<blah> (including -l<blah> options)
    extra_ld_opts <- getStaticOpts v_Opt_l

    [rts_pkg, std_pkg] <- getPackageDetails [rtsPackage, basePackage]

    let extra_os = if static || no_hs_main
                   then []
                   else [ head (library_dirs rts_pkg) ++ "/Main.dll_o",
                          head (library_dirs std_pkg) ++ "/PrelMain.dll_o" ]

    (md_c_flags, _) <- machdepCCOpts
    SysTools.runLink ( [ SysTools.Option verb
    		       , SysTools.Option "-o"
		       , SysTools.FileOption "" output_fn
		       ]
		      ++ map SysTools.Option (
		         md_c_flags
	 	      ++ o_files
		      ++ extra_os
		      ++ extra_ld_inputs
	 	      ++ lib_path_opts
	 	      ++ extra_ld_opts
#ifdef darwin_TARGET_OS
	 	      ++ framework_path_opts
	 	      ++ framework_opts
#endif
	 	      ++ pkg_lib_path_opts
	 	      ++ pkg_link_opts
#ifdef darwin_TARGET_OS
	 	      ++ pkg_framework_path_opts
	 	      ++ pkg_framework_opts
#endif
	              ++ if static && not no_hs_main then
			    [ "-u", prefixUnderscore "Main_zdmain_closure"] 
			 else []))

    -- parallel only: move binary to another dir -- HWL
    ways_ <- readIORef v_Ways
    when (WayPar `elem` ways_)
	 (do success <- runPhase_MoveBinary output_fn
             if success then return ()
                        else throwDyn (InstallationError ("cannot move binary to PVM dir")))

-----------------------------------------------------------------------------
-- Making a DLL (only for Win32)

doMkDLL :: [String] -> IO ()
doMkDLL o_files = do
    verb       <- getVerbFlag
    static     <- readIORef v_Static
    no_hs_main <- readIORef v_NoHsMain

    o_file <- readIORef v_Output_file
    let output_fn = case o_file of { Just s -> s; Nothing -> "HSdll.dll"; }

    pkg_lib_paths <- getPackageLibraryPath []
    let pkg_lib_path_opts = map ("-L"++) pkg_lib_paths

    lib_paths <- readIORef v_Library_paths
    let lib_path_opts = map ("-L"++) lib_paths

    pkg_link_opts <- getPackageLinkOpts []

	-- probably _stub.o files
    extra_ld_inputs <- readIORef v_Ld_inputs

	-- opts from -optdll-<blah>
    extra_ld_opts <- getStaticOpts v_Opt_dll

    [rts_pkg, std_pkg] <- getPackageDetails [rtsPackage, basePackage]

    let extra_os = if static || no_hs_main
                   then []
                   else [ head (library_dirs rts_pkg) ++ "/Main.dll_o",
                          head (library_dirs std_pkg) ++ "/PrelMain.dll_o" ]

    (md_c_flags, _) <- machdepCCOpts
    SysTools.runMkDLL
	 ([ SysTools.Option verb
	  , SysTools.Option "-o"
	  , SysTools.FileOption "" output_fn
	  ]
	 ++ map SysTools.Option (
	    md_c_flags
	 ++ o_files
	 ++ extra_os
	 ++ [ "--target=i386-mingw32" ]
	 ++ extra_ld_inputs
	 ++ lib_path_opts
	 ++ extra_ld_opts
	 ++ pkg_lib_path_opts
	 ++ pkg_link_opts
         ++ (if "--def" `elem` (concatMap words extra_ld_opts)
	       then [ "" ]
               else [ "--export-all" ])
	))
