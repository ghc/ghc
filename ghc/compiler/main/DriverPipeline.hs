-----------------------------------------------------------------------------
--
-- GHC Driver
--
-- (c) The University of Glasgow 2002
--
-----------------------------------------------------------------------------

#include "../includes/ghcconfig.h"

module DriverPipeline (

	-- Interfaces for the batch-mode driver
   compileFile, staticLink,

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
import RdrName		( GlobalRdrEnv )
import Panic
import Util
import StringBuffer	( hGetStringBuffer )
import BasicTypes	( SuccessFlag(..) )
import Maybes		( expectJust )

import ParserCoreUtils ( getCoreModuleName )

import EXCEPTION
import DATA_IOREF	( readIORef, writeIORef )

import Directory
import System
import IO
import Monad
import Maybe


-- ---------------------------------------------------------------------------
-- Pre-process

-- Just preprocess a file, put the result in a temp. file (used by the
-- compilation manager during the summary phase).
--
-- We return the augmented DynFlags, because they contain the result
-- of slurping in the OPTIONS pragmas

preprocess :: DynFlags -> FilePath -> IO (DynFlags, FilePath)
preprocess dflags filename =
  ASSERT2(isHaskellSrcFilename filename, text filename) 
  runPipeline (StopBefore anyHsc) dflags ("preprocess") 
	False{-temporary output file-}
	Nothing{-no specific output file-}
	filename
	Nothing{-no ModLocation-}



-- ---------------------------------------------------------------------------
--		Compile a file
--  	This is used in batch mode 
compileFile :: GhcMode -> DynFlags -> FilePath -> IO FilePath
compileFile mode dflags src = do
   exists <- doesFileExist src
   when (not exists) $ 
   	throwDyn (CmdLineError ("file `" ++ src ++ "' does not exist"))
   
   o_file  <- readIORef v_Output_file
   no_link <- readIORef v_NoLink	-- Set by -c or -no-link
	-- When linking, the -o argument refers to the linker's output.	
	-- otherwise, we use it as the name for the pipeline's output.
   let maybe_o_file | no_link   = o_file
		    | otherwise = Nothing

   stop_flag <- readIORef v_GhcModeFlag
   (_, out_file) <- runPipeline mode dflags stop_flag True maybe_o_file
				src Nothing{-no ModLocation-}
   return out_file


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

compile :: HscEnv
	-> ModSummary
	-> Bool			-- True <=> source unchanged
	-> Bool			-- True <=> have object
        -> Maybe ModIface       -- Old interface, if available
        -> IO CompResult

data CompResult
   = CompOK   ModDetails 		-- New details
	      (Maybe GlobalRdrEnv)	-- Lexical environment for the module
					-- (Maybe because we may have loaded it from
					--  its precompiled interface)
              ModIface			-- New iface
              (Maybe Linkable)	-- New code; Nothing => compilation was not reqd
		                --			(old code is still valid)

   | CompErrs 


compile hsc_env mod_summary
	source_unchanged have_object old_iface = do 

   let dyn_flags   = hsc_dflags hsc_env
       this_mod    = ms_mod mod_summary
       src_flavour = ms_hsc_src mod_summary

   showPass dyn_flags ("Compiling " ++ showModMsg have_object mod_summary)

   let verb	  = verbosity dyn_flags
   let location	  = ms_location mod_summary
   let input_fn   = expectJust "compile:hs" (ml_hs_file location) 
   let input_fnpp = expectJust "compile:hspp" (ms_hspp_file mod_summary)

   when (verb >= 2) (hPutStrLn stderr ("compile: input file " ++ input_fnpp))

   -- Add in the OPTIONS from the source file
   -- This is nasty: we've done this once already, in the compilation manager
   -- It might be better to cache the flags in the ml_hspp_file field,say
   opts <- getOptionsFromSource input_fnpp
   (dyn_flags,unhandled_flags) <- processDynamicFlags opts dyn_flags
   checkProcessArgsResult unhandled_flags input_fn

   let (basename, _) = splitFilename input_fn

  -- We add the directory in which the .hs files resides) to the import path.
  -- This is needed when we try to compile the .hc file later, if it
  -- imports a _stub.h file that we created here.
   let current_dir = directoryOf basename
   old_paths <- readIORef v_Include_paths
   writeIORef v_Include_paths (current_dir : old_paths)
   -- put back the old include paths afterward.
   later (writeIORef v_Include_paths old_paths) $ do

   -- Figure out what lang we're generating
   todo     <- readIORef v_GhcMode
   hsc_lang <- hscMaybeAdjustTarget todo src_flavour (hscTarget dyn_flags)
   -- ... and what the next phase should be
   next_phase <- hscNextPhase src_flavour hsc_lang
   -- ... and what file to generate the output into
   get_output_fn <- genOutputFilenameFunc next_phase False Nothing basename
   output_fn     <- get_output_fn next_phase (Just location)

   let dyn_flags' = dyn_flags { hscTarget = hsc_lang,
				hscOutName = output_fn,
				hscStubCOutName = basename ++ "_stub.c",
				hscStubHOutName = basename ++ "_stub.h",
				extCoreName = basename ++ ".hcr" }

   -- -no-recomp should also work with --make
   let do_recomp = recompFlag dyn_flags
       source_unchanged' = source_unchanged && do_recomp
       hsc_env' = hsc_env { hsc_dflags = dyn_flags' }

   -- run the compiler
   hsc_result <- hscMain hsc_env' printErrorsAndWarnings mod_summary
			 source_unchanged' have_object old_iface

   case hsc_result of
      HscFail -> return CompErrs

      HscNoRecomp details iface -> return (CompOK details Nothing iface Nothing)

      HscRecomp details rdr_env iface
		stub_h_exists stub_c_exists maybe_interpreted_code 

	| isHsBoot src_flavour	-- No further compilation to do
	-> return (CompOK details rdr_env iface Nothing)

	| otherwise		-- Normal Haskell source files
	-> do
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
		       Just comp_bc -> return ([BCOs comp_bc], ms_hs_date mod_summary)
			-- Why do we use the timestamp of the source file here,
			-- rather than the current time?  This works better in
			-- the case where the local clock is out of sync
			-- with the filesystem's clock.  It's just as accurate:
			-- if the source is modified, then the linkable will
			-- be out of date.
#endif
		       Nothing -> panic "compile: no interpreted code"

		-- we're in batch mode: finish the compilation pipeline.
		_other -> do
		   let object_filename = ml_obj_file location

		   runPipeline DoLink dyn_flags ""
			       True Nothing output_fn (Just location)
			-- the object filename comes from the ModLocation

		   o_time <- getModificationTime object_filename
		   return ([DotO object_filename], o_time)

	   let linkable = LM unlinked_time this_mod
			     (hs_unlinked ++ stub_unlinked)

	   return (CompOK details rdr_env iface (Just linkable))

-----------------------------------------------------------------------------
-- stub .h and .c files (for foreign export support)

compileStub dflags stub_c_exists
  | not stub_c_exists = return Nothing
  | stub_c_exists = do
	-- compile the _stub.c file w/ gcc
	let stub_c = hscStubCOutName dflags
	(_, stub_o) <- runPipeline DoLink dflags "stub-compile"
			    True{-persistent output-} 
			    Nothing{-no specific output file-}
			    stub_c
			    Nothing{-no ModLocation-}
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
		    hPutStrLn stderr "link(batch): linking omitted (-c flag given)."
	          return Succeeded
	  else do

	when (verb >= 1) $
             hPutStrLn stderr "Linking ..."

	let getOfiles (LM _ _ us) = map nameOfObject (filter isObject us)
	    obj_files = concatMap getOfiles linkables

	-- Don't showPass in Batch mode; doLink will do that for us.
        staticLink dflags obj_files pkg_deps

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

-- The DynFlags can be modified by phases in the pipeline (eg. by
-- OPTIONS pragmas), and the changes affect later phases in the
-- pipeline, but we throw away the resulting DynFlags at the end.

runPipeline
  :: GhcMode		-- when to stop
  -> DynFlags		-- dynamic flags
  -> String		-- "stop after" flag
  -> Bool		-- final output is persistent?
  -> Maybe FilePath	-- where to put the output, optionally
  -> FilePath 		-- input filename
  -> Maybe ModLocation  -- a ModLocation for this module, if we have one
  -> IO (DynFlags, FilePath)	-- (final flags, output filename)

runPipeline todo dflags stop_flag keep_output 
  maybe_output_filename input_fn maybe_loc
  = do
  split <- readIORef v_Split_object_files
  let (basename, suffix) = splitFilename input_fn
      start_phase = startPhase suffix

      todo' = case todo of
		StopBefore As | split -> StopBefore SplitAs
		other		      -> todo

  -- We want to catch cases of "you can't get there from here" before
  -- we start the pipeline, because otherwise it will just run off the
  -- end.
  --
  -- There is a partial ordering on phases, where A < B iff A occurs
  -- before B in a normal compilation pipeline.
  --
  let stop_phase = case todo' of 
			StopBefore phase -> phase
			other		 -> StopLn

  when (not (start_phase `happensBefore` stop_phase)) $
	throwDyn (UsageError 
		    ("flag `" ++ stop_flag
		     ++ "' is incompatible with source file `"
		     ++ input_fn ++ "'"))

  -- generate a function which will be used to calculate output file names
  -- as we go along.
  get_output_fn <- genOutputFilenameFunc stop_phase keep_output 
					 maybe_output_filename basename

  -- Execute the pipeline...
  (dflags', output_fn, maybe_loc) <- pipeLoop todo' dflags start_phase stop_phase input_fn 
				  	      basename suffix get_output_fn maybe_loc

  -- Sometimes, a compilation phase doesn't actually generate any output
  -- (eg. the CPP phase when -fcpp is not turned on).  If we end on this
  -- stage, but we wanted to keep the output, then we have to explicitly
  -- copy the file.
  if keep_output 
	then do final_fn <- get_output_fn stop_phase maybe_loc
	        when (final_fn /= output_fn) $
	 	  copy dflags ("Copying `" ++ output_fn ++ "' to `" ++ final_fn
			++ "'") output_fn final_fn
	        return (dflags', final_fn)
	else
	        return (dflags', output_fn)


pipeLoop :: GhcMode -> DynFlags -> Phase -> Phase 
	 -> FilePath  -> String -> Suffix
	 -> (Phase -> Maybe ModLocation -> IO FilePath)
	 -> Maybe ModLocation
	 -> IO (DynFlags, FilePath, Maybe ModLocation)

pipeLoop orig_todo dflags phase stop_phase 
	 input_fn orig_basename orig_suff 
	 orig_get_output_fn maybe_loc

  | phase `eqPhase` stop_phase		  -- All done
  = return (dflags, input_fn, maybe_loc)

  | not (phase `happensBefore` stop_phase)
	-- Something has gone wrong.  We'll try to cover all the cases when
	-- this could happen, so if we reach here it is a panic.
	-- eg. it might happen if the -C flag is used on a source file that
	-- has {-# OPTIONS -fasm #-}.
  = panic ("pipeLoop: at phase " ++ show phase ++ 
	   " but I wanted to stop at phase " ++ show stop_phase)

  | otherwise 
  = do	{ (next_phase, dflags', maybe_loc, output_fn)
		<- runPhase phase orig_todo dflags orig_basename 
			    orig_suff input_fn orig_get_output_fn maybe_loc
	; pipeLoop orig_todo dflags' next_phase stop_phase output_fn
		   orig_basename orig_suff orig_get_output_fn maybe_loc }

genOutputFilenameFunc :: Phase -> Bool -> Maybe FilePath -> String
  -> IO (Phase{-next phase-} -> Maybe ModLocation -> IO FilePath)
genOutputFilenameFunc stop_phase keep_final_output maybe_output_filename basename
 = do
   hcsuf      <- readIORef v_HC_suf
   odir       <- readIORef v_Output_dir
   osuf       <- readIORef v_Object_suf
   keep_hc    <- readIORef v_Keep_hc_files
#ifdef ILX
   keep_il    <- readIORef v_Keep_il_files
   keep_ilx   <- readIORef v_Keep_ilx_files
#endif
   keep_raw_s <- readIORef v_Keep_raw_s_files
   keep_s     <- readIORef v_Keep_s_files
   let
        myPhaseInputExt HCc    = hcsuf
        myPhaseInputExt StopLn = osuf
        myPhaseInputExt other  = phaseInputExt other

	func next_phase maybe_location
		| is_last_phase, Just f <- maybe_output_filename = return f
		| is_last_phase && keep_final_output = persistent_fn
		| keep_this_output 		     = persistent_fn
     		| otherwise        		     = newTempName suffix

	   where
		is_last_phase = next_phase `eqPhase` stop_phase

		-- sometimes, we keep output from intermediate stages
		keep_this_output = 
     		     case next_phase of
     			     StopLn              -> True
     			     Mangle | keep_raw_s -> True
     			     As     | keep_s     -> True
     			     HCc    | keep_hc    -> True
     			     _other              -> False

		suffix = myPhaseInputExt next_phase

		-- persistent object files get put in odir
	        persistent_fn 
		   | StopLn <- next_phase = return odir_persistent
		   | otherwise            = return persistent

		persistent = basename ++ '.':suffix

		odir_persistent
		   | Just loc <- maybe_location = ml_obj_file loc
		   | Just d <- odir = replaceFilenameDirectory persistent d
		   | otherwise      = persistent

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
	 -> GhcMode
	 -> DynFlags
	 -> String	-- basename of original input source
	 -> String	-- its extension
	 -> FilePath	-- name of file which contains the input to this phase.
	 -> (Phase -> Maybe ModLocation -> IO FilePath)
			-- how to calculate the output filename
	 -> Maybe ModLocation		-- the ModLocation, if we have one
	 -> IO (Phase,	  		-- next phase
		DynFlags,		-- new dynamic flags
		Maybe ModLocation,	-- the ModLocation, if we have one
		FilePath)		-- output filename

	-- Invariant: the output filename always contains the output
	-- Interesting case: Hsc when there is no recompilation to do
	--		     Then the output filename is still a .o file 

-------------------------------------------------------------------------------
-- Unlit phase 

runPhase (Unlit sf) _todo dflags _basename _suff input_fn get_output_fn maybe_loc
  = do let unlit_flags = getOpts dflags opt_L
       -- The -h option passes the file name for unlit to put in a #line directive
       output_fn <- get_output_fn (Cpp sf) maybe_loc

       SysTools.runUnlit dflags 
		(map SysTools.Option unlit_flags ++
       			  [ SysTools.Option     "-h"
 			  , SysTools.Option     input_fn
			  , SysTools.FileOption "" input_fn
			  , SysTools.FileOption "" output_fn
			  ])

       return (Cpp sf, dflags, maybe_loc, output_fn)

-------------------------------------------------------------------------------
-- Cpp phase : (a) gets OPTIONS out of file
--	       (b) runs cpp if necessary

runPhase (Cpp sf) _todo dflags basename suff input_fn get_output_fn maybe_loc
  = do src_opts <- getOptionsFromSource input_fn
       (dflags,unhandled_flags) <- processDynamicFlags src_opts dflags
       checkProcessArgsResult unhandled_flags (basename++'.':suff)

       if not (cppFlag dflags) then
           -- no need to preprocess CPP, just pass input file along
	   -- to the next phase of the pipeline.
          return (HsPp sf, dflags, maybe_loc, input_fn)
	else do
	    output_fn <- get_output_fn (HsPp sf) maybe_loc
	    doCpp dflags True{-raw-} False{-no CC opts-} input_fn output_fn
	    return (HsPp sf, dflags, maybe_loc, output_fn)

-------------------------------------------------------------------------------
-- HsPp phase 

runPhase (HsPp sf) _todo dflags basename suff input_fn get_output_fn maybe_loc
  = do if not (ppFlag dflags) then
           -- no need to preprocess, just pass input file along
	   -- to the next phase of the pipeline.
          return (Hsc sf, dflags, maybe_loc, input_fn)
	else do
	    let hspp_opts = getOpts dflags opt_F
       	    hs_src_pp_opts <- readIORef v_Hs_source_pp_opts
	    let orig_fn = basename ++ '.':suff
	    output_fn <- get_output_fn (Hsc sf) maybe_loc
	    SysTools.runPp dflags
			   ( [ SysTools.Option     orig_fn
			     , SysTools.Option     input_fn
			     , SysTools.FileOption "" output_fn
			     ] ++
			     map SysTools.Option hs_src_pp_opts ++
			     map SysTools.Option hspp_opts
			   )
	    return (Hsc sf, dflags, maybe_loc, output_fn)

-----------------------------------------------------------------------------
-- Hsc phase

-- Compilation of a single module, in "legacy" mode (_not_ under
-- the direction of the compilation manager).
runPhase (Hsc src_flavour) todo dflags basename suff input_fn get_output_fn _maybe_loc 
 = do	-- normal Hsc mode, not mkdependHS

  -- we add the current directory (i.e. the directory in which
  -- the .hs files resides) to the import path, since this is
  -- what gcc does, and it's probably what you want.
	let current_dir = directoryOf basename
	
	paths <- readIORef v_Include_paths
	writeIORef v_Include_paths (current_dir : paths)
	
  -- gather the imports and module name
        (hspp_buf,mod_name) <- 
            case src_flavour of
		ExtCoreFile -> do {  -- no explicit imports in ExtCore input.
			          ; m <- getCoreModuleName input_fn
			          ; return (Nothing, mkModule m) }

		other -> do { buf <- hGetStringBuffer input_fn
			    ; (_,_,mod_name) <- getImports dflags buf input_fn
			    ; return (Just buf, mod_name) }

  -- Build a ModLocation to pass to hscMain.
  -- The source filename is rather irrelevant by now, but it's used
  -- by hscMain for messages.  hscMain also needs 
  -- the .hi and .o filenames, and this is as good a way
  -- as any to generate them, and better than most. (e.g. takes 
  -- into accout the -osuf flags)
	location1 <- mkHomeModLocation2 mod_name basename suff

  -- Boot-ify it if necessary
	let location2 | isHsBoot src_flavour = addBootSuffixLocn location1
		      | otherwise	     = location1 
					

  -- Take -ohi into account if present
  -- This can't be done in mkHomeModuleLocation because
  -- it only applies to the module being compiles
	ohi <- readIORef v_Output_hi
	let location3 | Just fn <- ohi = location2{ ml_hi_file = fn }
		      | otherwise      = location2

  -- Take -o into account if present
  -- Very like -ohi, but we must *only* do this if we aren't linking
  -- (If we're linking then the -o applies to the linked thing, not to
  -- the object file for one module.)
  -- Note the nasty duplication with the same computation in compileFile above
	expl_o_file <- readIORef v_Output_file
	no_link     <- readIORef v_NoLink
	let location4 | Just ofile <- expl_o_file, no_link 
		      = location3 { ml_obj_file = ofile }
		      | otherwise = location3

  -- Tell the finder cache about this module
	addHomeModuleToFinder mod_name location4

  -- Make the ModSummary to hand to hscMain
	src_timestamp <- getModificationTime (basename ++ '.':suff)
	let
	    unused_field = panic "runPhase:ModSummary field"
		-- Some fields are not looked at by hscMain
	    mod_summary = ModSummary {	ms_mod 	     = mod_name, 
					ms_hsc_src   = src_flavour,
				 	ms_hspp_file = Just input_fn,
					ms_hspp_buf  = hspp_buf,
					ms_location  = location4,
					ms_hs_date   = src_timestamp,
					ms_imps	     = unused_field,
					ms_srcimps   = unused_field }

	    o_file = ml_obj_file location4 	-- The real object file


  -- Figure out if the source has changed, for recompilation avoidance.
  -- only do this if we're eventually going to generate a .o file.
  -- (ToDo: do when generating .hc files too?)
  --
  -- Setting source_unchanged to True means that M.o seems
  -- to be up to date wrt M.hs; so no need to recompile unless imports have
  -- changed (which the compiler itself figures out).
  -- Setting source_unchanged to False tells the compiler that M.o is out of
  -- date wrt M.hs (or M.o doesn't exist) so we must recompile regardless.
	let do_recomp = recompFlag dflags
	source_unchanged <- 
          if not (do_recomp && case todo of { DoLink -> True; other -> False })
	     then return False
	     else do o_file_exists <- doesFileExist o_file
		     if not o_file_exists
		        then return False	-- Need to recompile
			else do t2 <- getModificationTime o_file
			        if t2 > src_timestamp
				  then return True
				  else return False

  -- get the DynFlags
	hsc_lang   <- hscMaybeAdjustTarget todo src_flavour (hscTarget dflags)
	next_phase <- hscNextPhase src_flavour hsc_lang
	output_fn  <- get_output_fn next_phase (Just location4)

        let dflags' = dflags { hscTarget = hsc_lang,
			       hscOutName = output_fn,
		   	       hscStubCOutName = basename ++ "_stub.c",
			       hscStubHOutName = basename ++ "_stub.h",
			       extCoreName = basename ++ ".hcr" }

	hsc_env <- newHscEnv OneShot dflags'

  -- run the compiler!
	result <- hscMain hsc_env printErrorsAndWarnings
			  mod_summary source_unchanged 
			  False		-- No object file
			  Nothing	-- No iface

	case result of

	    HscFail -> throwDyn (PhaseFailed "hsc" (ExitFailure 1))

            HscNoRecomp details iface -> do
		SysTools.touch dflags' "Touching object file" o_file
		return (StopLn, dflags', Just location4, o_file)

	    HscRecomp _details _rdr_env _iface 
		      stub_h_exists stub_c_exists
		      _maybe_interpreted_code -> do

		-- Deal with stubs 
		maybe_stub_o <- compileStub dflags' stub_c_exists
		case maybe_stub_o of
		      Nothing     -> return ()
		      Just stub_o -> add v_Ld_inputs stub_o

		-- In the case of hs-boot files, generate a dummy .o-boot 
		-- stamp file for the benefit of Make
		case src_flavour of
		  HsBootFile -> SysTools.touch dflags' "Touching object file" o_file
		  other	     -> return ()

		return (next_phase, dflags', Just location4, output_fn)

-----------------------------------------------------------------------------
-- Cmm phase

runPhase CmmCpp todo dflags basename suff input_fn get_output_fn maybe_loc
  = do
       output_fn <- get_output_fn Cmm maybe_loc
       doCpp dflags False{-not raw-} True{-include CC opts-} input_fn output_fn	
       return (Cmm, dflags, maybe_loc, output_fn)

runPhase Cmm todo dflags basename suff input_fn get_output_fn maybe_loc
  = do
	hsc_lang <- hscMaybeAdjustTarget todo HsSrcFile (hscTarget dflags)
	next_phase <- hscNextPhase HsSrcFile hsc_lang
	output_fn <- get_output_fn next_phase maybe_loc

        let dflags' = dflags { hscTarget = hsc_lang,
			       hscOutName = output_fn,
		   	       hscStubCOutName = basename ++ "_stub.c",
			       hscStubHOutName = basename ++ "_stub.h",
			       extCoreName = basename ++ ".hcr" }

	ok <- hscCmmFile dflags' input_fn

	when (not ok) $ throwDyn (PhaseFailed "cmm" (ExitFailure 1))

	return (next_phase, dflags, maybe_loc, output_fn)

-----------------------------------------------------------------------------
-- Cc phase

-- we don't support preprocessing .c files (with -E) now.  Doing so introduces
-- way too many hacks, and I can't say I've ever used it anyway.

runPhase cc_phase todo dflags basename suff input_fn get_output_fn maybe_loc
   | cc_phase `eqPhase` Cc || cc_phase `eqPhase` HCc
   = do	let cc_opts = getOpts dflags opt_c
	    hcc = cc_phase `eqPhase` HCc

       	cmdline_include_paths <- readIORef v_Include_paths

	-- HC files have the dependent packages stamped into them
	pkgs <- if hcc then getHCFilePackages input_fn else return []

	-- add package include paths even if we're just compiling .c
	-- files; this is the Value Add(TM) that using ghc instead of
	-- gcc gives you :)
        pkg_include_dirs <- getPackageIncludePath dflags pkgs
        let include_paths = foldr (\ x xs -> "-I" : x : xs) []
			      (cmdline_include_paths ++ pkg_include_dirs)

	(md_c_flags, md_regd_c_flags) <- machdepCCOpts dflags

        let verb = getVerbFlag dflags

	pkg_extra_cc_opts <- getPackageExtraCcOpts dflags pkgs

	split_objs <- readIORef v_Split_object_files
	let split_opt | hcc && split_objs = [ "-DUSE_SPLIT_MARKERS" ]
		      | otherwise         = [ ]

	excessPrecision <- readIORef v_Excess_precision

	-- Decide next phase
	mangle <- readIORef v_Do_asm_mangling
        let next_phase
		| hcc && mangle     = Mangle
		| otherwise         = As
	output_fn <- get_output_fn next_phase maybe_loc

	-- force the C compiler to interpret this file as C when
	-- compiling .hc files, by adding the -x c option.
	let langopt | hcc = [ SysTools.Option "-x", SysTools.Option "c"]
		    | otherwise = [ ]

	SysTools.runCc dflags (langopt ++
			[ SysTools.FileOption "" input_fn
			, SysTools.Option "-o"
			, SysTools.FileOption "" output_fn
			]
		       ++ map SysTools.Option (
		          md_c_flags
		       ++ (if hcc && mangle
		  	     then md_regd_c_flags
		  	     else [])
		       ++ [ verb, "-S", "-Wimplicit", "-O" ]
		       ++ [ "-D__GLASGOW_HASKELL__="++cProjectVersionInt ]
		       ++ cc_opts
		       ++ split_opt
		       ++ (if excessPrecision then [] else [ "-ffloat-store" ])
		       ++ include_paths
		       ++ pkg_extra_cc_opts
		       ))

	return (next_phase, dflags, maybe_loc, output_fn)

	-- ToDo: postprocess the output from gcc

-----------------------------------------------------------------------------
-- Mangle phase

runPhase Mangle todo dflags _basename _suff input_fn get_output_fn maybe_loc
   = do let mangler_opts = getOpts dflags opt_m

#if i386_TARGET_ARCH
        machdep_opts <- return [ show (stolen_x86_regs dflags) ]
#else
	machdep_opts <- return []
#endif

	split <- readIORef v_Split_object_files
	let next_phase
		| split = SplitMangle
		| otherwise = As
	output_fn <- get_output_fn next_phase maybe_loc

	SysTools.runMangle dflags (map SysTools.Option mangler_opts
		          ++ [ SysTools.FileOption "" input_fn
			     , SysTools.FileOption "" output_fn
			     ]
			  ++ map SysTools.Option machdep_opts)

	return (next_phase, dflags, maybe_loc, output_fn)

-----------------------------------------------------------------------------
-- Splitting phase

runPhase SplitMangle todo dflags _basename _suff input_fn get_output_fn maybe_loc
  = do  -- tmp_pfx is the prefix used for the split .s files
	-- We also use it as the file to contain the no. of split .s files (sigh)
	split_s_prefix <- SysTools.newTempName "split"
	let n_files_fn = split_s_prefix

	SysTools.runSplit dflags
			  [ SysTools.FileOption "" input_fn
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

	return (SplitAs, dflags, maybe_loc, "**splitmangle**")
	  -- we don't use the filename

-----------------------------------------------------------------------------
-- As phase

runPhase As todo dflags _basename _suff input_fn get_output_fn maybe_loc
  = do	let as_opts =  getOpts dflags opt_a
        cmdline_include_paths <- readIORef v_Include_paths

	output_fn <- get_output_fn StopLn maybe_loc

	-- we create directories for the object file, because it
	-- might be a hierarchical module.
	createDirectoryHierarchy (directoryOf output_fn)

	SysTools.runAs dflags	
		       (map SysTools.Option as_opts
		       ++ [ SysTools.Option ("-I" ++ p) | p <- cmdline_include_paths ]
		       ++ [ SysTools.Option "-c"
		          , SysTools.FileOption "" input_fn
			  , SysTools.Option "-o"
			  , SysTools.FileOption "" output_fn
			  ])

	return (StopLn, dflags, maybe_loc, output_fn)


runPhase SplitAs todo dflags basename _suff _input_fn get_output_fn maybe_loc
  = do  let as_opts = getOpts dflags opt_a

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
		    SysTools.runAs dflags
				 (map SysTools.Option as_opts ++
		    		    [ SysTools.Option "-c"
				    , SysTools.Option "-o"
				    , SysTools.FileOption "" real_o
				    , SysTools.FileOption "" input_s
				    ])
	
	mapM_ assemble_file [1..n]

	output_fn <- get_output_fn StopLn maybe_loc
	return (StopLn, dflags, maybe_loc, output_fn)

#ifdef ILX
-----------------------------------------------------------------------------
-- Ilx2Il phase
-- Run ilx2il over the ILX output, getting an IL file

runPhase Ilx2Il todo dflags _basename _suff input_fn get_output_fn maybe_loc
  = do	let ilx2il_opts = getOpts dflags opt_I
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

runPhase Ilasm todo dflags _basename _suff input_fn get_output_fn maybe_loc
  = do	let ilasm_opts = getOpts dflags opt_i
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

checkProcessArgsResult flags filename
  = do when (notNull flags) (throwDyn (ProgramError (
	  showSDoc (hang (text filename <> char ':')
		      4 (text "unknown flags in  {-# OPTIONS #-} pragma:" <+>
			  hsep (map text flags)))
	)))

-----------------------------------------------------------------------------
-- Look for the /* GHC_PACKAGES ... */ comment at the top of a .hc file

getHCFilePackages :: FilePath -> IO [PackageId]
getHCFilePackages filename =
  EXCEPTION.bracket (openFile filename ReadMode) hClose $ \h -> do
    l <- hGetLine h
    case l of
      '/':'*':' ':'G':'H':'C':'_':'P':'A':'C':'K':'A':'G':'E':'S':rest ->
	  return (map stringToPackageId (words rest))
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

staticLink :: DynFlags -> [FilePath] -> [PackageId] -> IO ()
staticLink dflags o_files dep_packages = do
    let verb = getVerbFlag dflags
    static     <- readIORef v_Static
    no_hs_main <- readIORef v_NoHsMain

    -- get the full list of packages to link with, by combining the
    -- explicit packages with the auto packages and all of their
    -- dependencies, and eliminating duplicates.

    o_file <- readIORef v_Output_file
#if defined(mingw32_HOST_OS)
    let output_fn = case o_file of { Just s -> s; Nothing -> "main.exe"; }
#else
    let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }
#endif

    pkg_lib_paths <- getPackageLibraryPath dflags dep_packages
    let pkg_lib_path_opts = map ("-L"++) pkg_lib_paths

    lib_paths <- readIORef v_Library_paths
    let lib_path_opts = map ("-L"++) lib_paths

    pkg_link_opts <- getPackageLinkOpts dflags dep_packages

#ifdef darwin_TARGET_OS
    pkg_framework_paths <- getPackageFrameworkPath dflags dep_packages
    let pkg_framework_path_opts = map ("-F"++) pkg_framework_paths

    framework_paths <- readIORef v_Framework_paths
    let framework_path_opts = map ("-F"++) framework_paths

    pkg_frameworks <- getPackageFrameworks dflags dep_packages
    let pkg_framework_opts = concat [ ["-framework", fw] | fw <- pkg_frameworks ]
    frameworks <- readIORef v_Cmdline_frameworks
    let framework_opts = concat [ ["-framework", fw] | fw <- reverse frameworks ]
	 -- reverse because they're added in reverse order from the cmd line
#endif

	-- probably _stub.o files
    extra_ld_inputs <- readIORef v_Ld_inputs

	-- opts from -optl-<blah> (including -l<blah> options)
    extra_ld_opts <- getStaticOpts v_Opt_l

    ways <- readIORef v_Ways

    -- Here are some libs that need to be linked at the *end* of
    -- the command line, because they contain symbols that are referred to
    -- by the RTS.  We can't therefore use the ordinary way opts for these.
    let
	debug_opts | WayDebug `elem` ways = [ 
#if defined(HAVE_LIBBFD)
			"-lbfd", "-liberty"
#endif
			 ]
		   | otherwise            = []

    let
	thread_opts | WayThreaded `elem` ways = [ 
#if !defined(mingw32_TARGET_OS) && !defined(freebsd_TARGET_OS)
			"-lpthread"
#endif
#if defined(osf3_TARGET_OS)
			, "-lexc"
#endif
			]
		    | otherwise               = []

    (md_c_flags, _) <- machdepCCOpts dflags
    SysTools.runLink dflags ( 
		       [ SysTools.Option verb
    		       , SysTools.Option "-o"
		       , SysTools.FileOption "" output_fn
		       ]
		      ++ map SysTools.Option (
		         md_c_flags
	 	      ++ o_files
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
		      ++ debug_opts
		      ++ thread_opts
		    ))

    -- parallel only: move binary to another dir -- HWL
    ways_ <- readIORef v_Ways
    when (WayPar `elem` ways_)
	 (do success <- runPhase_MoveBinary output_fn
             if success then return ()
                        else throwDyn (InstallationError ("cannot move binary to PVM dir")))

-----------------------------------------------------------------------------
-- Making a DLL (only for Win32)

doMkDLL :: DynFlags -> [String] -> [PackageId] -> IO ()
doMkDLL dflags o_files dep_packages = do
    let verb = getVerbFlag dflags
    static     <- readIORef v_Static
    no_hs_main <- readIORef v_NoHsMain

    o_file <- readIORef v_Output_file
    let output_fn = case o_file of { Just s -> s; Nothing -> "HSdll.dll"; }

    pkg_lib_paths <- getPackageLibraryPath dflags dep_packages
    let pkg_lib_path_opts = map ("-L"++) pkg_lib_paths

    lib_paths <- readIORef v_Library_paths
    let lib_path_opts = map ("-L"++) lib_paths

    pkg_link_opts <- getPackageLinkOpts dflags dep_packages

	-- probably _stub.o files
    extra_ld_inputs <- readIORef v_Ld_inputs

	-- opts from -optdll-<blah>
    extra_ld_opts <- getStaticOpts v_Opt_dll

    let pstate = pkgState dflags
	rts_id | ExtPackage id <- rtsPackageId pstate = id
	       | otherwise = panic "staticLink: rts package missing"
	base_id | ExtPackage id <- basePackageId pstate = id
	        | otherwise = panic "staticLink: base package missing"
	rts_pkg  = getPackageDetails pstate rts_id
        base_pkg = getPackageDetails pstate base_id

    let extra_os = if static || no_hs_main
                   then []
                   else [ head (libraryDirs rts_pkg) ++ "/Main.dll_o",
                          head (libraryDirs base_pkg) ++ "/PrelMain.dll_o" ]

    (md_c_flags, _) <- machdepCCOpts dflags
    SysTools.runMkDLL dflags
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

-- -----------------------------------------------------------------------------
-- Misc.

doCpp :: DynFlags -> Bool -> Bool -> FilePath -> FilePath -> IO ()
doCpp dflags raw include_cc_opts input_fn output_fn = do
    let hscpp_opts = getOpts dflags opt_P

    cmdline_include_paths <- readIORef v_Include_paths

    pkg_include_dirs <- getPackageIncludePath dflags []
    let include_paths = foldr (\ x xs -> "-I" : x : xs) []
			  (cmdline_include_paths ++ pkg_include_dirs)

    let verb = getVerbFlag dflags

    cc_opts <- if not include_cc_opts 
		  then return []
		  else do let optc = getOpts dflags opt_c
			  (md_c_flags, _) <- machdepCCOpts dflags
			  return (optc ++ md_c_flags)

    let cpp_prog args | raw       = SysTools.runCpp dflags args
	              | otherwise = SysTools.runCc dflags (SysTools.Option "-E" : args)

    let target_defs = 
	  [ "-D" ++ HOST_OS     ++ "BUILD_OS=1",
	    "-D" ++ HOST_ARCH   ++ "BUILD_ARCH=1",
	    "-D" ++ TARGET_OS   ++ "HOST_OS=1",
	    "-D" ++ TARGET_ARCH ++ "HOST_ARCH=1" ]
	-- remember, in code we *compile*, the HOST is the same our TARGET,
	-- and BUILD is the same as our HOST.

    cpp_prog       ([SysTools.Option verb]
		    ++ map SysTools.Option include_paths
		    ++ map SysTools.Option hsSourceCppOpts
		    ++ map SysTools.Option hscpp_opts
		    ++ map SysTools.Option cc_opts
		    ++ map SysTools.Option target_defs
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

-- -----------------------------------------------------------------------------
-- Misc.

hscNextPhase :: HscSource -> HscTarget -> IO Phase
hscNextPhase HsBootFile hsc_lang 
  = return StopLn

hscNextPhase other hsc_lang = do
  split <- readIORef v_Split_object_files
  return (case hsc_lang of
		HscC -> HCc
		HscAsm | split -> SplitMangle
		       | otherwise -> As
		HscNothing     -> StopLn
		HscInterpreted -> StopLn
		_other         -> StopLn
	)

hscMaybeAdjustTarget :: GhcMode -> HscSource -> HscTarget -> IO HscTarget
hscMaybeAdjustTarget todo HsBootFile current_hsc_lang 
  = return HscNothing		-- No output (other than Foo.hi-boot) for hs-boot files
hscMaybeAdjustTarget todo other current_hsc_lang 
  = do	{ keep_hc <- readIORef v_Keep_hc_files
	; let hsc_lang
		-- don't change the lang if we're interpreting
		 | current_hsc_lang == HscInterpreted = current_hsc_lang

		-- force -fvia-C if we are being asked for a .hc file
		 | StopBefore HCc <- todo = HscC
		 | keep_hc 		  = HscC
		-- otherwise, stick to the plan
		 | otherwise = current_hsc_lang
	; return hsc_lang }
