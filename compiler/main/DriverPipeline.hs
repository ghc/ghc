{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-----------------------------------------------------------------------------
--
-- GHC Driver
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module DriverPipeline (
	-- Run a series of compilation steps in a pipeline, for a
	-- collection of source files.
   oneShot, compileFile,

	-- Interfaces for the batch-mode driver
   linkBinary,

	-- Interfaces for the compilation manager (interpreted/batch-mode)
   preprocess, 
   compile,
   link, 

  ) where

#include "HsVersions.h"

import Packages
import HeaderInfo
import DriverPhases
import SysTools
import HscMain
import Finder
import HscTypes
import Outputable
import Module
import UniqFM		( eltsUFM )
import ErrUtils
import DynFlags
import StaticFlags	( v_Ld_inputs, opt_Static, opt_HardwireLibPaths, WayName(..) )
import Config
import Panic
import Util
import StringBuffer	( hGetStringBuffer )
import BasicTypes	( SuccessFlag(..) )
import Maybes		( expectJust )
import ParserCoreUtils	( getCoreModuleName )
import SrcLoc		( unLoc )
import SrcLoc		( Located(..) )

import Control.Exception as Exception
import Data.IORef	( readIORef, writeIORef, IORef )
import GHC.Exts		( Int(..) )
import System.Directory
import System.IO
import SYSTEM_IO_ERROR as IO
import Control.Monad
import Data.List	( isSuffixOf )
import Data.Maybe
import System.Exit
import System.Environment

-- ---------------------------------------------------------------------------
-- Pre-process

-- Just preprocess a file, put the result in a temp. file (used by the
-- compilation manager during the summary phase).
--
-- We return the augmented DynFlags, because they contain the result
-- of slurping in the OPTIONS pragmas

preprocess :: DynFlags -> (FilePath, Maybe Phase) -> IO (DynFlags, FilePath)
preprocess dflags (filename, mb_phase) =
  ASSERT2(isJust mb_phase || isHaskellSrcFilename filename, text filename) 
  runPipeline anyHsc dflags (filename, mb_phase) 
        Nothing Temporary Nothing{-no ModLocation-}

-- ---------------------------------------------------------------------------
-- Compile

-- Compile a single module, under the control of the compilation manager.
--
-- This is the interface between the compilation manager and the
-- compiler proper (hsc), where we deal with tedious details like
-- reading the OPTIONS pragma from the source file, and passing the
-- output of hsc through the C compiler.

-- NB.  No old interface can also mean that the source has changed.

compile :: HscEnv
        -> ModSummary                   -- summary for module being compiled
        -> Int -> Int                   -- module N of M
        -> Maybe ModIface               -- old interface, if we have one
        -> Maybe Linkable               -- old linkable, if we have one
        -> IO (Maybe HomeModInfo)       -- the complete HomeModInfo, if successful

compile hsc_env summary mod_index nmods mb_old_iface maybe_old_linkable
 = do
   let dflags0     = ms_hspp_opts summary
       this_mod    = ms_mod summary
       src_flavour = ms_hsc_src summary

       have_object 
	       | Just l <- maybe_old_linkable, isObjectLinkable l = True
	       | otherwise = False

   let location	  = ms_location summary
   let input_fn   = expectJust "compile:hs" (ml_hs_file location) 
   let input_fnpp = ms_hspp_file summary

   debugTraceMsg dflags0 2 (text "compile: input file" <+> text input_fnpp)

   let (basename, _) = splitFilename input_fn

  -- We add the directory in which the .hs files resides) to the import path.
  -- This is needed when we try to compile the .hc file later, if it
  -- imports a _stub.h file that we created here.
   let current_dir = directoryOf basename
       old_paths   = includePaths dflags0
       dflags      = dflags0 { includePaths = current_dir : old_paths }

   -- Figure out what lang we're generating
   let hsc_lang = hscMaybeAdjustTarget dflags StopLn src_flavour (hscTarget dflags)
   -- ... and what the next phase should be
   let next_phase = hscNextPhase dflags src_flavour hsc_lang
   -- ... and what file to generate the output into
   output_fn <- getOutputFilename next_phase 
			Temporary basename dflags next_phase (Just location)

   let dflags' = dflags { hscTarget = hsc_lang,
				hscOutName = output_fn,
				extCoreName = basename ++ ".hcr" }

   -- -no-recomp should also work with --make
   let force_recomp = dopt Opt_ForceRecomp dflags
       source_unchanged = isJust maybe_old_linkable && not force_recomp
       hsc_env' = hsc_env { hsc_dflags = dflags' }
       object_filename = ml_obj_file location

   let getStubLinkable False = return []
       getStubLinkable True
           = do stub_o <- compileStub dflags' this_mod location
                return [ DotO stub_o ]

       handleBatch HscNoRecomp
           = ASSERT (isJust maybe_old_linkable)
             return maybe_old_linkable

       handleBatch (HscRecomp hasStub)
           | isHsBoot src_flavour
               = do when (isObjectTarget hsc_lang) $ -- interpreted reaches here too
                       SysTools.touch dflags' "Touching object file"
                                   object_filename
                    return maybe_old_linkable

           | otherwise
               = do stub_unlinked <- getStubLinkable hasStub
                    (hs_unlinked, unlinked_time) <-
                        case hsc_lang of
                          HscNothing
                            -> return ([], ms_hs_date summary)
                          -- We're in --make mode: finish the compilation pipeline.
                          _other
                            -> do runPipeline StopLn dflags (output_fn,Nothing)
                                              (Just basename)
                                              Persistent
                                              (Just location)
                                  -- The object filename comes from the ModLocation
                                  o_time <- getModificationTime object_filename
                                  return ([DotO object_filename], o_time)
                    let linkable = LM unlinked_time this_mod
			           (hs_unlinked ++ stub_unlinked)
                    return (Just linkable)

       handleInterpreted InteractiveNoRecomp
           = ASSERT (isJust maybe_old_linkable)
             return maybe_old_linkable
       handleInterpreted (InteractiveRecomp hasStub comp_bc modBreaks)
           = do stub_unlinked <- getStubLinkable hasStub
                let hs_unlinked = [BCOs comp_bc modBreaks]
                    unlinked_time = ms_hs_date summary
                  -- Why do we use the timestamp of the source file here,
                  -- rather than the current time?  This works better in
                  -- the case where the local clock is out of sync
                  -- with the filesystem's clock.  It's just as accurate:
                  -- if the source is modified, then the linkable will
                  -- be out of date.
                let linkable = LM unlinked_time this_mod
                               (hs_unlinked ++ stub_unlinked)
                return (Just linkable)

   let -- runCompiler :: Compiler result -> (result -> Maybe Linkable)
       --            -> IO (Maybe HomeModInfo)
       runCompiler compiler handle
           = do mbResult <- compiler hsc_env' summary source_unchanged mb_old_iface
                                     (Just (mod_index, nmods))
                case mbResult of
                  Nothing -> return Nothing
                  Just (result, iface, details) -> do
                        linkable <- handle result
                        return (Just HomeModInfo{ hm_details  = details,
                                                  hm_iface    = iface,
                                                  hm_linkable = linkable })
   -- run the compiler
   case hsc_lang of
      HscInterpreted
        | isHsBoot src_flavour -> 
                runCompiler hscCompileNothing handleBatch
        | otherwise -> 
                runCompiler hscCompileInteractive handleInterpreted
      HscNothing -> 
                runCompiler hscCompileNothing handleBatch
      _other -> 
                runCompiler hscCompileBatch handleBatch

-----------------------------------------------------------------------------
-- stub .h and .c files (for foreign export support)

-- The _stub.c file is derived from the haskell source file, possibly taking
-- into account the -stubdir option.
--
-- Consequently, we derive the _stub.o filename from the haskell object
-- filename.  
--
-- This isn't necessarily the same as the object filename we
-- would get if we just compiled the _stub.c file using the pipeline.
-- For example:
--
--    ghc src/A.hs -odir obj
-- 
-- results in obj/A.o, and src/A_stub.c.  If we compile src/A_stub.c with
-- -odir obj, we would get obj/src/A_stub.o, which is wrong; we want
-- obj/A_stub.o.

compileStub :: DynFlags -> Module -> ModLocation -> IO FilePath
compileStub dflags mod location = do
	let (o_base, o_ext) = splitFilename (ml_obj_file location)
	    stub_o = o_base ++ "_stub" `joinFileExt` o_ext

	-- compile the _stub.c file w/ gcc
	let (stub_c,_,_) = mkStubPaths dflags (moduleName mod) location
	runPipeline StopLn dflags (stub_c,Nothing)  Nothing
		(SpecificFile stub_o) Nothing{-no ModLocation-}

	return stub_o


-- ---------------------------------------------------------------------------
-- Link

link :: GhcLink                 -- interactive or batch
     -> DynFlags                -- dynamic flags
     -> Bool                    -- attempt linking in batch mode?
     -> HomePackageTable        -- what to link
     -> IO SuccessFlag

-- For the moment, in the batch linker, we don't bother to tell doLink
-- which packages to link -- it just tries all that are available.
-- batch_attempt_linking should only be *looked at* in batch mode.  It
-- should only be True if the upsweep was successful and someone
-- exports main, i.e., we have good reason to believe that linking
-- will succeed.

#ifdef GHCI
link LinkInMemory dflags batch_attempt_linking hpt
    = do -- Not Linking...(demand linker will do the job)
         return Succeeded
#endif

link NoLink dflags batch_attempt_linking hpt
   = return Succeeded

link LinkBinary dflags batch_attempt_linking hpt
   | batch_attempt_linking
   = do
        let
            home_mod_infos = eltsUFM hpt

            -- the packages we depend on
            pkg_deps  = concatMap (dep_pkgs . mi_deps . hm_iface) home_mod_infos

            -- the linkables to link
            linkables = map (expectJust "link".hm_linkable) home_mod_infos

        debugTraceMsg dflags 3 (text "link: linkables are ..." $$ vcat (map ppr linkables))

        -- check for the -no-link flag
        if isNoLink (ghcLink dflags)
          then do debugTraceMsg dflags 3 (text "link(batch): linking omitted (-c flag given).")
                  return Succeeded
          else do

        let getOfiles (LM _ _ us) = map nameOfObject (filter isObject us)
            obj_files = concatMap getOfiles linkables

            exe_file = exeFileName dflags

        -- if the modification time on the executable is later than the
        -- modification times on all of the objects, then omit linking
        -- (unless the -no-recomp flag was given).
        e_exe_time <- IO.try $ getModificationTime exe_file
        extra_ld_inputs <- readIORef v_Ld_inputs
        extra_times <- mapM (IO.try . getModificationTime) extra_ld_inputs
        let other_times = map linkableTime linkables
                       ++ [ t' | Right t' <- extra_times ]
            linking_needed
                | Left _  <- e_exe_time = True
                | Right t <- e_exe_time = any (t <) other_times

        if not (dopt Opt_ForceRecomp dflags) && not linking_needed
           then do debugTraceMsg dflags 2 (text exe_file <+> ptext SLIT("is up to date, linking not required."))
                   return Succeeded
           else do

        debugTraceMsg dflags 1 (ptext SLIT("Linking") <+> text exe_file
                                 <+> text "...")

        -- Don't showPass in Batch mode; doLink will do that for us.
        let link = case ghcLink dflags of
                LinkBinary  -> linkBinary
                LinkDynLib  -> linkDynLib
        link dflags obj_files pkg_deps

        debugTraceMsg dflags 3 (text "link: done")

        -- linkBinary only returns if it succeeds
        return Succeeded

   | otherwise
   = do debugTraceMsg dflags 3 (text "link(batch): upsweep (partially) failed OR" $$
                                text "   Main.main not exported; not linking.")
        return Succeeded

-- -----------------------------------------------------------------------------
-- Compile files in one-shot mode.

oneShot :: DynFlags -> Phase -> [(String, Maybe Phase)] -> IO ()
oneShot dflags stop_phase srcs = do
  o_files <- mapM (compileFile dflags stop_phase) srcs
  doLink dflags stop_phase o_files

compileFile :: DynFlags -> Phase -> (FilePath, Maybe Phase) -> IO FilePath
compileFile dflags stop_phase (src, mb_phase) = do
   exists <- doesFileExist src
   when (not exists) $ 
   	throwDyn (CmdLineError ("does not exist: " ++ src))
   
   let
	split     = dopt Opt_SplitObjs dflags
	mb_o_file = outputFile dflags
	ghc_link  = ghcLink dflags	-- Set by -c or -no-link

	-- When linking, the -o argument refers to the linker's output.	
	-- otherwise, we use it as the name for the pipeline's output.
        output
	 | StopLn <- stop_phase, not (isNoLink ghc_link) = Persistent
		-- -o foo applies to linker
	 | Just o_file <- mb_o_file = SpecificFile o_file
		-- -o foo applies to the file we are compiling now
	 | otherwise = Persistent

        stop_phase' = case stop_phase of 
			As | split -> SplitAs
			other      -> stop_phase

   (_, out_file) <- runPipeline stop_phase' dflags
			  (src, mb_phase) Nothing output 
                          Nothing{-no ModLocation-}
   return out_file


doLink :: DynFlags -> Phase -> [FilePath] -> IO ()
doLink dflags stop_phase o_files
  | not (isStopLn stop_phase)
  = return ()		-- We stopped before the linking phase

  | otherwise
  = case ghcLink dflags of
	NoLink     -> return ()
	LinkBinary -> linkBinary dflags o_files link_pkgs
	LinkDynLib -> linkDynLib dflags o_files []
  where
   -- Always link in the haskell98 package for static linking.  Other
   -- packages have to be specified via the -package flag.
    link_pkgs = [haskell98PackageId]


-- ---------------------------------------------------------------------------
-- Run a compilation pipeline, consisting of multiple phases.

-- This is the interface to the compilation pipeline, which runs
-- a series of compilation steps on a single source file, specifying
-- at which stage to stop.

-- The DynFlags can be modified by phases in the pipeline (eg. by
-- GHC_OPTIONS pragmas), and the changes affect later phases in the
-- pipeline.

data PipelineOutput 
  = Temporary
	-- output should be to a temporary file: we're going to
	-- run more compilation steps on this output later
  | Persistent
	-- we want a persistent file, i.e. a file in the current directory
	-- derived from the input filename, but with the appropriate extension.
	-- eg. in "ghc -c Foo.hs" the output goes into ./Foo.o.
  | SpecificFile FilePath
	-- the output must go into the specified file.

runPipeline
  :: Phase		        -- When to stop
  -> DynFlags		        -- Dynamic flags
  -> (FilePath,Maybe Phase)     -- Input filename (and maybe -x suffix)
  -> Maybe FilePath             -- original basename (if different from ^^^)
  -> PipelineOutput	        -- Output filename
  -> Maybe ModLocation          -- A ModLocation, if this is a Haskell module
  -> IO (DynFlags, FilePath)	-- (final flags, output filename)

runPipeline stop_phase dflags0 (input_fn, mb_phase) mb_basename output maybe_loc
  = do
  let
      (input_basename, suffix) = splitFilename input_fn
      basename | Just b <- mb_basename = b
               | otherwise             = input_basename

      -- Decide where dump files should go based on the pipeline output
      dflags = dflags0 { dumpPrefix = Just (basename ++ ".") }

	-- If we were given a -x flag, then use that phase to start from
      start_phase = fromMaybe (startPhase suffix) mb_phase

  -- We want to catch cases of "you can't get there from here" before
  -- we start the pipeline, because otherwise it will just run off the
  -- end.
  --
  -- There is a partial ordering on phases, where A < B iff A occurs
  -- before B in a normal compilation pipeline.

  when (not (start_phase `happensBefore` stop_phase)) $
	throwDyn (UsageError 
		    ("cannot compile this file to desired target: "
		       ++ input_fn))

  -- this is a function which will be used to calculate output file names
  -- as we go along (we partially apply it to some of its inputs here)
  let get_output_fn = getOutputFilename stop_phase output basename

  -- Execute the pipeline...
  (dflags', output_fn, maybe_loc) <- 
	pipeLoop dflags start_phase stop_phase input_fn 
	  	 basename suffix get_output_fn maybe_loc

  -- Sometimes, a compilation phase doesn't actually generate any output
  -- (eg. the CPP phase when -fcpp is not turned on).  If we end on this
  -- stage, but we wanted to keep the output, then we have to explicitly
  -- copy the file, remembering to prepend a {-# LINE #-} pragma so that
  -- further compilation stages can tell what the original filename was.
  case output of
    Temporary -> 
	return (dflags', output_fn)
    _other ->
	do final_fn <- get_output_fn dflags' stop_phase maybe_loc
	   when (final_fn /= output_fn) $ do
              let msg = ("Copying `" ++ output_fn ++"' to `" ++ final_fn ++ "'")
                  line_prag = Just ("{-# LINE 1 \"" ++ input_fn ++ "\" #-}\n")
	      copyWithHeader dflags msg line_prag output_fn final_fn
	   return (dflags', final_fn)



pipeLoop :: DynFlags -> Phase -> Phase 
	 -> FilePath  -> String -> Suffix
	 -> (DynFlags -> Phase -> Maybe ModLocation -> IO FilePath)
	 -> Maybe ModLocation
	 -> IO (DynFlags, FilePath, Maybe ModLocation)

pipeLoop dflags phase stop_phase 
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
		<- runPhase phase stop_phase dflags orig_basename 
			    orig_suff input_fn orig_get_output_fn maybe_loc
	; pipeLoop dflags' next_phase stop_phase output_fn
		   orig_basename orig_suff orig_get_output_fn maybe_loc }

getOutputFilename
  :: Phase -> PipelineOutput -> String
  -> DynFlags -> Phase{-next phase-} -> Maybe ModLocation -> IO FilePath
getOutputFilename stop_phase output basename
 = func
 where
	func dflags next_phase maybe_location
	   | is_last_phase, Persistent <- output     = persistent_fn
	   | is_last_phase, SpecificFile f <- output = return f
	   | keep_this_output	   		     = persistent_fn
     	   | otherwise        	   		     = newTempName dflags suffix
	   where
		hcsuf      = hcSuf dflags
		odir       = objectDir dflags
		osuf       = objectSuf dflags
		keep_hc    = dopt Opt_KeepHcFiles dflags
		keep_raw_s = dopt Opt_KeepRawSFiles dflags
		keep_s     = dopt Opt_KeepSFiles dflags

		myPhaseInputExt HCc    = hcsuf
		myPhaseInputExt StopLn = osuf
		myPhaseInputExt other  = phaseInputExt other

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

		persistent = basename `joinFileExt` suffix

		odir_persistent
		   | Just loc <- maybe_location = ml_obj_file loc
		   | Just d <- odir = d `joinFileName` persistent
		   | otherwise      = persistent


-- -----------------------------------------------------------------------------
-- Each phase in the pipeline returns the next phase to execute, and the
-- name of the file in which the output was placed.
--
-- We must do things dynamically this way, because we often don't know
-- what the rest of the phases will be until part-way through the
-- compilation: for example, an {-# OPTIONS -fasm #-} at the beginning
-- of a source file can change the latter stages of the pipeline from
-- taking the via-C route to using the native code generator.

runPhase :: Phase	-- Do this phase first
	 -> Phase	-- Stop just before this phase
	 -> DynFlags
	 -> String	-- basename of original input source
	 -> String	-- its extension
	 -> FilePath	-- name of file which contains the input to this phase.
	 -> (DynFlags -> Phase -> Maybe ModLocation -> IO FilePath)
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

runPhase (Unlit sf) _stop dflags _basename _suff input_fn get_output_fn maybe_loc
  = do let unlit_flags = getOpts dflags opt_L
       -- The -h option passes the file name for unlit to put in a #line directive
       output_fn <- get_output_fn dflags (Cpp sf) maybe_loc

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

runPhase (Cpp sf) _stop dflags0 basename suff input_fn get_output_fn maybe_loc
  = do src_opts <- getOptionsFromFile input_fn
       (dflags,unhandled_flags) <- parseDynamicFlags dflags0 (map unLoc src_opts)
       checkProcessArgsResult unhandled_flags (basename `joinFileExt` suff)

       if not (dopt Opt_Cpp dflags) then
           -- no need to preprocess CPP, just pass input file along
	   -- to the next phase of the pipeline.
          return (HsPp sf, dflags, maybe_loc, input_fn)
	else do
	    output_fn <- get_output_fn dflags (HsPp sf) maybe_loc
	    doCpp dflags True{-raw-} False{-no CC opts-} input_fn output_fn
	    return (HsPp sf, dflags, maybe_loc, output_fn)

-------------------------------------------------------------------------------
-- HsPp phase 

runPhase (HsPp sf) _stop dflags basename suff input_fn get_output_fn maybe_loc
  = do if not (dopt Opt_Pp dflags) then
           -- no need to preprocess, just pass input file along
	   -- to the next phase of the pipeline.
          return (Hsc sf, dflags, maybe_loc, input_fn)
	else do
	    let hspp_opts = getOpts dflags opt_F
	    let orig_fn = basename `joinFileExt` suff
	    output_fn <- get_output_fn dflags (Hsc sf) maybe_loc
	    SysTools.runPp dflags
			   ( [ SysTools.Option     orig_fn
			     , SysTools.Option     input_fn
			     , SysTools.FileOption "" output_fn
			     ] ++
			     map SysTools.Option hspp_opts
			   )
	    return (Hsc sf, dflags, maybe_loc, output_fn)

-----------------------------------------------------------------------------
-- Hsc phase

-- Compilation of a single module, in "legacy" mode (_not_ under
-- the direction of the compilation manager).
runPhase (Hsc src_flavour) stop dflags0 basename suff input_fn get_output_fn _maybe_loc 
 = do	-- normal Hsc mode, not mkdependHS

  -- we add the current directory (i.e. the directory in which
  -- the .hs files resides) to the include path, since this is
  -- what gcc does, and it's probably what you want.
	let current_dir = directoryOf basename
	
	    paths = includePaths dflags0
	    dflags = dflags0 { includePaths = current_dir : paths }
	
  -- gather the imports and module name
        (hspp_buf,mod_name,imps,src_imps) <- 
            case src_flavour of
		ExtCoreFile -> do {  -- no explicit imports in ExtCore input.
			          ; m <- getCoreModuleName input_fn
			          ; return (Nothing, mkModuleName m, [], []) }

		other -> do { buf <- hGetStringBuffer input_fn
			    ; (src_imps,imps,L _ mod_name) <- getImports dflags buf input_fn (basename `joinFileExt` suff)
			    ; return (Just buf, mod_name, imps, src_imps) }

  -- Build a ModLocation to pass to hscMain.
  -- The source filename is rather irrelevant by now, but it's used
  -- by hscMain for messages.  hscMain also needs 
  -- the .hi and .o filenames, and this is as good a way
  -- as any to generate them, and better than most. (e.g. takes 
  -- into accout the -osuf flags)
	location1 <- mkHomeModLocation2 dflags mod_name basename suff

  -- Boot-ify it if necessary
	let location2 | isHsBoot src_flavour = addBootSuffixLocn location1
		      | otherwise	     = location1 
					

  -- Take -ohi into account if present
  -- This can't be done in mkHomeModuleLocation because
  -- it only applies to the module being compiles
	let ohi = outputHi dflags
	    location3 | Just fn <- ohi = location2{ ml_hi_file = fn }
		      | otherwise      = location2

  -- Take -o into account if present
  -- Very like -ohi, but we must *only* do this if we aren't linking
  -- (If we're linking then the -o applies to the linked thing, not to
  -- the object file for one module.)
  -- Note the nasty duplication with the same computation in compileFile above
	let expl_o_file = outputFile dflags
	    location4 | Just ofile <- expl_o_file
		      , isNoLink (ghcLink dflags)
		      = location3 { ml_obj_file = ofile }
		      | otherwise = location3

	    o_file = ml_obj_file location4 	-- The real object file


  -- Figure out if the source has changed, for recompilation avoidance.
  --
  -- Setting source_unchanged to True means that M.o seems
  -- to be up to date wrt M.hs; so no need to recompile unless imports have
  -- changed (which the compiler itself figures out).
  -- Setting source_unchanged to False tells the compiler that M.o is out of
  -- date wrt M.hs (or M.o doesn't exist) so we must recompile regardless.
	src_timestamp <- getModificationTime (basename `joinFileExt` suff)

	let force_recomp = dopt Opt_ForceRecomp dflags
	source_unchanged <- 
          if force_recomp || not (isStopLn stop)
		-- Set source_unchanged to False unconditionally if
		--	(a) recompilation checker is off, or
		-- 	(b) we aren't going all the way to .o file (e.g. ghc -S)
	     then return False	
		-- Otherwise look at file modification dates
	     else do o_file_exists <- doesFileExist o_file
		     if not o_file_exists
		        then return False	-- Need to recompile
			else do t2 <- getModificationTime o_file
			        if t2 > src_timestamp
				  then return True
				  else return False

  -- get the DynFlags
	let hsc_lang = hscMaybeAdjustTarget dflags stop src_flavour (hscTarget dflags)
	let next_phase = hscNextPhase dflags src_flavour hsc_lang
	output_fn  <- get_output_fn dflags next_phase (Just location4)

        let dflags' = dflags { hscTarget = hsc_lang,
			       hscOutName = output_fn,
			       extCoreName = basename ++ ".hcr" }

	hsc_env <- newHscEnv dflags'

  -- Tell the finder cache about this module
	mod <- addHomeModuleToFinder hsc_env mod_name location4

  -- Make the ModSummary to hand to hscMain
	let
	    unused_field = panic "runPhase:ModSummary field"
		-- Some fields are not looked at by hscMain
	    mod_summary = ModSummary {	ms_mod 	     = mod, 
					ms_hsc_src   = src_flavour,
				 	ms_hspp_file = input_fn,
                                        ms_hspp_opts = dflags,
					ms_hspp_buf  = hspp_buf,
					ms_location  = location4,
					ms_hs_date   = src_timestamp,
					ms_obj_date  = Nothing,
					ms_imps	     = imps,
					ms_srcimps   = src_imps }

  -- run the compiler!
	mbResult <- hscCompileOneShot hsc_env
			  mod_summary source_unchanged 
			  Nothing	-- No iface
                          Nothing       -- No "module i of n" progress info

	case mbResult of
          Nothing -> throwDyn (PhaseFailed "hsc" (ExitFailure 1))
          Just HscNoRecomp
              -> do SysTools.touch dflags' "Touching object file" o_file
                    -- The .o file must have a later modification date
                    -- than the source file (else we wouldn't be in HscNoRecomp)
                    -- but we touch it anyway, to keep 'make' happy (we think).
                    return (StopLn, dflags', Just location4, o_file)
          Just (HscRecomp hasStub)
              -> do when hasStub $
                         do stub_o <- compileStub dflags' mod location4
                            consIORef v_Ld_inputs stub_o
                    -- In the case of hs-boot files, generate a dummy .o-boot 
                    -- stamp file for the benefit of Make
                    when (isHsBoot src_flavour) $
                      SysTools.touch dflags' "Touching object file" o_file
                    return (next_phase, dflags', Just location4, output_fn)

-----------------------------------------------------------------------------
-- Cmm phase

runPhase CmmCpp stop dflags basename suff input_fn get_output_fn maybe_loc
  = do
       output_fn <- get_output_fn dflags Cmm maybe_loc
       doCpp dflags False{-not raw-} True{-include CC opts-} input_fn output_fn	
       return (Cmm, dflags, maybe_loc, output_fn)

runPhase Cmm stop dflags basename suff input_fn get_output_fn maybe_loc
  = do
	let hsc_lang = hscMaybeAdjustTarget dflags stop HsSrcFile (hscTarget dflags)
	let next_phase = hscNextPhase dflags HsSrcFile hsc_lang
	output_fn <- get_output_fn dflags next_phase maybe_loc

        let dflags' = dflags { hscTarget = hsc_lang,
			       hscOutName = output_fn,
			       extCoreName = basename ++ ".hcr" }

	ok <- hscCmmFile dflags' input_fn

	when (not ok) $ throwDyn (PhaseFailed "cmm" (ExitFailure 1))

	return (next_phase, dflags, maybe_loc, output_fn)

-----------------------------------------------------------------------------
-- Cc phase

-- we don't support preprocessing .c files (with -E) now.  Doing so introduces
-- way too many hacks, and I can't say I've ever used it anyway.

runPhase cc_phase stop dflags basename suff input_fn get_output_fn maybe_loc
   | cc_phase `eqPhase` Cc || cc_phase `eqPhase` Ccpp || cc_phase `eqPhase` HCc
   = do	let cc_opts = getOpts dflags opt_c
	    hcc = cc_phase `eqPhase` HCc

       	let cmdline_include_paths = includePaths dflags

	-- HC files have the dependent packages stamped into them
	pkgs <- if hcc then getHCFilePackages input_fn else return []

	-- add package include paths even if we're just compiling .c
	-- files; this is the Value Add(TM) that using ghc instead of
	-- gcc gives you :)
        pkg_include_dirs <- getPackageIncludePath dflags pkgs
        let include_paths = foldr (\ x xs -> "-I" : x : xs) []
			      (cmdline_include_paths ++ pkg_include_dirs)

	let (md_c_flags, md_regd_c_flags) = machdepCCOpts dflags
        gcc_extra_viac_flags <- getExtraViaCOpts dflags
        let pic_c_flags = picCCOpts dflags

        let verb = getVerbFlag dflags

	pkg_extra_cc_opts <- getPackageExtraCcOpts dflags pkgs

	let split_objs = dopt Opt_SplitObjs dflags
	    split_opt | hcc && split_objs = [ "-DUSE_SPLIT_MARKERS" ]
		      | otherwise         = [ ]

	let excessPrecision = dopt Opt_ExcessPrecision dflags

	let cc_opt | optLevel dflags >= 2 = "-O2"
		   | otherwise            = "-O"

	-- Decide next phase
	
        let mangle = dopt Opt_DoAsmMangling dflags
            next_phase
		| hcc && mangle     = Mangle
		| otherwise         = As
	output_fn <- get_output_fn dflags next_phase maybe_loc

	let
	  more_hcc_opts =
#if i386_TARGET_ARCH
	   	-- on x86 the floating point regs have greater precision
	     	-- than a double, which leads to unpredictable results.
		-- By default, we turn this off with -ffloat-store unless
		-- the user specified -fexcess-precision.
		(if excessPrecision then [] else [ "-ffloat-store" ]) ++
#endif
		-- gcc's -fstrict-aliasing allows two accesses to memory
		-- to be considered non-aliasing if they have different types.
		-- This interacts badly with the C code we generate, which is
		-- very weakly typed, being derived from C--.
		["-fno-strict-aliasing"]



	SysTools.runCc dflags (
		-- force the C compiler to interpret this file as C when
		-- compiling .hc files, by adding the -x c option.
		-- Also useful for plain .c files, just in case GHC saw a 
		-- -x c option.
			[ SysTools.Option "-x", if cc_phase `eqPhase` Ccpp
                                                then SysTools.Option "c++" else SysTools.Option "c"] ++
			[ SysTools.FileOption "" input_fn
			, SysTools.Option "-o"
			, SysTools.FileOption "" output_fn
			]
		       ++ map SysTools.Option (
		          md_c_flags
                       ++ pic_c_flags
#ifdef sparc_TARGET_ARCH
        -- We only support SparcV9 and better because V8 lacks an atomic CAS
        -- instruction. Note that the user can still override this
	-- (e.g., -mcpu=ultrasparc) as GCC picks the "best" -mcpu flag
	-- regardless of the ordering.
        --
        -- This is a temporary hack.
                       ++ ["-mcpu=v9"]
#endif
		       ++ (if hcc && mangle
		  	     then md_regd_c_flags
		  	     else [])
		       ++ (if hcc
		  	     then if mangle 
                                     then gcc_extra_viac_flags
                                     else filter (=="-fwrapv")
                                                gcc_extra_viac_flags
                                -- still want -fwrapv even for unreg'd
		  	     else [])
		       ++ (if hcc 
			     then more_hcc_opts
			     else [])
		       ++ [ verb, "-S", "-Wimplicit", cc_opt ]
		       ++ [ "-D__GLASGOW_HASKELL__="++cProjectVersionInt ]
		       ++ cc_opts
		       ++ split_opt
		       ++ include_paths
		       ++ pkg_extra_cc_opts
		       ))

	return (next_phase, dflags, maybe_loc, output_fn)

	-- ToDo: postprocess the output from gcc

-----------------------------------------------------------------------------
-- Mangle phase

runPhase Mangle stop dflags _basename _suff input_fn get_output_fn maybe_loc
   = do let mangler_opts = getOpts dflags opt_m

#if i386_TARGET_ARCH
        machdep_opts <- return [ show (stolen_x86_regs dflags) ]
#else
	machdep_opts <- return []
#endif

	let split = dopt Opt_SplitObjs dflags
            next_phase
		| split = SplitMangle
		| otherwise = As
	output_fn <- get_output_fn dflags next_phase maybe_loc

	SysTools.runMangle dflags (map SysTools.Option mangler_opts
		          ++ [ SysTools.FileOption "" input_fn
			     , SysTools.FileOption "" output_fn
			     ]
			  ++ map SysTools.Option machdep_opts)

	return (next_phase, dflags, maybe_loc, output_fn)

-----------------------------------------------------------------------------
-- Splitting phase

runPhase SplitMangle stop dflags _basename _suff input_fn get_output_fn maybe_loc
  = do  -- tmp_pfx is the prefix used for the split .s files
	-- We also use it as the file to contain the no. of split .s files (sigh)
	split_s_prefix <- SysTools.newTempName dflags "split"
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

runPhase As stop dflags _basename _suff input_fn get_output_fn maybe_loc
  = do	let as_opts =  getOpts dflags opt_a
        let cmdline_include_paths = includePaths dflags

	output_fn <- get_output_fn dflags StopLn maybe_loc

	-- we create directories for the object file, because it
	-- might be a hierarchical module.
	createDirectoryHierarchy (directoryOf output_fn)

	SysTools.runAs dflags	
		       (map SysTools.Option as_opts
		       ++ [ SysTools.Option ("-I" ++ p) | p <- cmdline_include_paths ]
#ifdef sparc_TARGET_ARCH
        -- We only support SparcV9 and better because V8 lacks an atomic CAS
	-- instruction so we have to make sure that the assembler accepts the
        -- instruction set. Note that the user can still override this
	-- (e.g., -mcpu=ultrasparc). GCC picks the "best" -mcpu flag
	-- regardless of the ordering.
	--
	-- This is a temporary hack.
		       ++ [ SysTools.Option "-mcpu=v9" ]
#endif
		       ++ [ SysTools.Option "-c"
		          , SysTools.FileOption "" input_fn
			  , SysTools.Option "-o"
			  , SysTools.FileOption "" output_fn
			  ])

	return (StopLn, dflags, maybe_loc, output_fn)


runPhase SplitAs stop dflags basename _suff _input_fn get_output_fn maybe_loc
  = do  
	output_fn <- get_output_fn dflags StopLn maybe_loc

	let (base_o, _) = splitFilename output_fn
	    split_odir  = base_o ++ "_split"
	    osuf = objectSuf dflags

	createDirectoryHierarchy split_odir

	-- remove M_split/ *.o, because we're going to archive M_split/ *.o
	-- later and we don't want to pick up any old objects.
	fs <- getDirectoryContents split_odir 
	mapM_ removeFile $ map (split_odir `joinFileName`)
			 $ filter (osuf `isSuffixOf`) fs

	let as_opts = getOpts dflags opt_a

	(split_s_prefix, n) <- readIORef v_Split_info

	let split_s   n = split_s_prefix ++ "__" ++ show n `joinFileExt` "s"
	    split_obj n = split_odir `joinFileName`
				filenameOf base_o ++ "__" ++ show n
					`joinFileExt` osuf

	let assemble_file n
	      = SysTools.runAs dflags
			 (map SysTools.Option as_opts ++
		   	 [ SysTools.Option "-c"
			 , SysTools.Option "-o"
			 , SysTools.FileOption "" (split_obj n)
			 , SysTools.FileOption "" (split_s n)
			 ])
	
	mapM_ assemble_file [1..n]

	-- and join the split objects into a single object file:
	let ld_r args = SysTools.runLink dflags ([ 
				SysTools.Option "-nostdlib",
				SysTools.Option "-nodefaultlibs",
				SysTools.Option "-Wl,-r", 
				SysTools.Option ld_x_flag, 
				SysTools.Option "-o", 
				SysTools.FileOption "" output_fn ] ++ args)
            ld_x_flag | null cLD_X = ""
		      | otherwise  = "-Wl,-x"	  

	if cLdIsGNULd == "YES"
	    then do 
		  let script = split_odir `joinFileName` "ld.script"
		  writeFile script $
		      "INPUT(" ++ unwords (map split_obj [1..n]) ++ ")"
		  ld_r [SysTools.FileOption "" script]
	    else do
		  ld_r (map (SysTools.FileOption "" . split_obj) [1..n])

	return (StopLn, dflags, maybe_loc, output_fn)


-----------------------------------------------------------------------------
-- MoveBinary sort-of-phase
-- After having produced a binary, move it somewhere else and generate a
-- wrapper script calling the binary. Currently, we need this only in 
-- a parallel way (i.e. in GUM), because PVM expects the binary in a
-- central directory.
-- This is called from linkBinary below, after linking. I haven't made it
-- a separate phase to minimise interfering with other modules, and
-- we don't need the generality of a phase (MoveBinary is always
-- done after linking and makes only sense in a parallel setup)   -- HWL

runPhase_MoveBinary dflags input_fn
  = do	
        let sysMan = pgm_sysman dflags
        pvm_root <- getEnv "PVM_ROOT"
        pvm_arch <- getEnv "PVM_ARCH"
        let 
           pvm_executable_base = "=" ++ input_fn
           pvm_executable = pvm_root ++ "/bin/" ++ pvm_arch ++ "/" ++ pvm_executable_base
        -- nuke old binary; maybe use configur'ed names for cp and rm?
        Panic.try (removeFile pvm_executable)
        -- move the newly created binary into PVM land
        copy dflags "copying PVM executable" input_fn pvm_executable
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
  "        $in_RTS_args = 1;",
  "    } elsif ( $a eq '-RTS' ) {",
  "        $in_RTS_args = 0;",
  "    }",
  "    if ( $a eq '-d' && $in_RTS_args ) {",
  "        $debug = '-';",
  "    } elsif ( $a =~ /^-qN(\\d+)/ && $in_RTS_args ) {",
  "        $nprocessors = $1;",
  "    } elsif ( $a =~ /^-qp(\\d+)/ && $in_RTS_args ) {",
  "        $nprocessors = $1;",
  "    } else {",
  "        push(@nonPVM_args, $a);",
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
  Exception.bracket (openFile filename ReadMode) hClose $ \h -> do
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

linkBinary :: DynFlags -> [FilePath] -> [PackageId] -> IO ()
linkBinary dflags o_files dep_packages = do
    let verb = getVerbFlag dflags
        output_fn = exeFileName dflags

    -- get the full list of packages to link with, by combining the
    -- explicit packages with the auto packages and all of their
    -- dependencies, and eliminating duplicates.

    pkg_lib_paths <- getPackageLibraryPath dflags dep_packages
    let pkg_lib_path_opts = concat (map get_pkg_lib_path_opts pkg_lib_paths)
	get_pkg_lib_path_opts l | opt_HardwireLibPaths && not opt_Static = ["-L" ++ l, "-Wl,-rpath", "-Wl," ++ l]
				| otherwise = ["-L" ++ l]

    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    pkg_link_opts <- getPackageLinkOpts dflags dep_packages

#ifdef darwin_TARGET_OS
    pkg_framework_paths <- getPackageFrameworkPath dflags dep_packages
    let pkg_framework_path_opts = map ("-F"++) pkg_framework_paths

    let framework_paths = frameworkPaths dflags
        framework_path_opts = map ("-F"++) framework_paths

    pkg_frameworks <- getPackageFrameworks dflags dep_packages
    let pkg_framework_opts = concat [ ["-framework", fw] | fw <- pkg_frameworks ]
    
    let frameworks = cmdlineFrameworks dflags
        framework_opts = concat [ ["-framework", fw] | fw <- reverse frameworks ]
	 -- reverse because they're added in reverse order from the cmd line
#endif

	-- probably _stub.o files
    extra_ld_inputs <- readIORef v_Ld_inputs

	-- opts from -optl-<blah> (including -l<blah> options)
    let extra_ld_opts = getOpts dflags opt_l

    let ways = wayNames dflags

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

    rc_objs <- maybeCreateManifest dflags output_fn

    let (md_c_flags, _) = machdepCCOpts dflags
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
                      ++ rc_objs
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
    when (WayPar `elem` ways)
	 (do success <- runPhase_MoveBinary dflags output_fn
             if success then return ()
                        else throwDyn (InstallationError ("cannot move binary to PVM dir")))


exeFileName :: DynFlags -> FilePath
exeFileName dflags
  | Just s <- outputFile dflags = 
#if defined(mingw32_HOST_OS)
      if null (suffixOf s)
        then s `joinFileExt` "exe"
        else s
#else
      s
#endif
  | otherwise = 
#if defined(mingw32_HOST_OS)
	"main.exe"
#else
	"a.out"
#endif

maybeCreateManifest
   :: DynFlags
   -> FilePath                          -- filename of executable
   -> IO [FilePath]                     -- extra objects to embed, maybe
maybeCreateManifest dflags exe_filename = do
#ifndef mingw32_TARGET_OS
  return []
#else
  if not (dopt Opt_GenManifest dflags) then return [] else do

  let manifest_filename = exe_filename `joinFileExt` "manifest"

  writeFile manifest_filename $ 
      "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"++
      "  <assembly xmlns=\"urn:schemas-microsoft-com:asm.v1\" manifestVersion=\"1.0\">\n"++
      "  <assemblyIdentity version=\"1.0.0.0\"\n"++
      "     processorArchitecture=\"X86\"\n"++
      "     name=\"" ++ basenameOf exe_filename ++ "\"\n"++
      "     type=\"win32\"/>\n\n"++
      "  <trustInfo xmlns=\"urn:schemas-microsoft-com:asm.v3\">\n"++
      "    <security>\n"++
      "      <requestedPrivileges>\n"++
      "        <requestedExecutionLevel level=\"asInvoker\" uiAccess=\"false\"/>\n"++
      "        </requestedPrivileges>\n"++
      "       </security>\n"++
      "  </trustInfo>\n"++
      "</assembly>\n"

  -- Windows will fine the manifest file if it is named foo.exe.manifest.
  -- However, for extra robustness, and so that we can move the binary around,
  -- we can embed the manifest in the binary itself using windres:
  if not (dopt Opt_EmbedManifest dflags) then return [] else do

  rc_filename <- newTempName dflags "rc"
  rc_obj_filename <- newTempName dflags (objectSuf dflags)

  writeFile rc_filename $
      "1 24 MOVEABLE PURE " ++ show manifest_filename ++ "\n"
        -- magic numbers :-)
        -- show is a bit hackish above, but we need to esacpe the
        -- backslashes in the path.

  let wr_opts = getOpts dflags opt_windres
  runWindres dflags $ map SysTools.Option $
        ["--input="++rc_filename, 
         "--output="++rc_obj_filename,
         "--output-format=coff"] 
        ++ wr_opts
        -- no FileOptions here: windres doesn't like seeing
        -- backslashes, apparently

  return [rc_obj_filename]
#endif


linkDynLib :: DynFlags -> [String] -> [PackageId] -> IO ()
linkDynLib dflags o_files dep_packages = do
    let verb = getVerbFlag dflags
    let static = opt_Static
    let no_hs_main = dopt Opt_NoHsMain dflags
    let o_file = outputFile dflags

    pkg_lib_paths <- getPackageLibraryPath dflags dep_packages
    let pkg_lib_path_opts = map ("-L"++) pkg_lib_paths

    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    pkg_link_opts <- getPackageLinkOpts dflags dep_packages

	-- probably _stub.o files
    extra_ld_inputs <- readIORef v_Ld_inputs

    let (md_c_flags, _) = machdepCCOpts dflags
    let extra_ld_opts = getOpts dflags opt_l
#if defined(mingw32_HOST_OS)
    -----------------------------------------------------------------------------
    -- Making a DLL
    -----------------------------------------------------------------------------
    let output_fn = case o_file of { Just s -> s; Nothing -> "HSdll.dll"; }

    SysTools.runLink dflags
	 ([ SysTools.Option verb
	  , SysTools.Option "-o"
	  , SysTools.FileOption "" output_fn
	  , SysTools.Option "-shared"
	  , SysTools.FileOption "-Wl,--out-implib=" (output_fn ++ ".a")
	  ]
	 ++ map (SysTools.FileOption "") o_files
	 ++ map SysTools.Option (
	    md_c_flags
	 ++ extra_ld_inputs
	 ++ lib_path_opts
	 ++ extra_ld_opts
	 ++ pkg_lib_path_opts
	 ++ pkg_link_opts
	))
#elif defined(darwin_TARGET_OS)
    -----------------------------------------------------------------------------
    -- Making a darwin dylib
    -----------------------------------------------------------------------------
    -- About the options used for Darwin:
    -- -dynamiclib
    --   Apple's way of saying -shared
    -- -undefined dynamic_lookup:
    --   Without these options, we'd have to specify the correct dependencies
    --   for each of the dylibs. Note that we could (and should) do without this
    --	 for all libraries except the RTS; all we need to do is to pass the
    --	 correct HSfoo_dyn.dylib files to the link command.
    --	 This feature requires Mac OS X 10.3 or later; there is a similar feature,
    --	 -flat_namespace -undefined suppress, which works on earlier versions,
    --	 but it has other disadvantages.
    -- -single_module
    --	 Build the dynamic library as a single "module", i.e. no dynamic binding
    --	 nonsense when referring to symbols from within the library. The NCG
    --	 assumes that this option is specified (on i386, at least).
    -- -Wl,-macosx_version_min -Wl,10.3
    --	 Tell the linker its safe to assume that the library will run on 10.3 or
    --	 later, so that it will not complain about the use of the option
    --	 -undefined dynamic_lookup above.
    -- -install_name
    --   Causes the dynamic linker to ignore the DYLD_LIBRARY_PATH when loading
    --   this lib and instead look for it at its absolute path.
    --   When installing the .dylibs (see target.mk), we'll change that path to
    --   point to the place they are installed. Therefore, we won't have to set
    --	 up DYLD_LIBRARY_PATH specifically for ghc.
    -----------------------------------------------------------------------------

    let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }

    pwd <- getCurrentDirectory
    SysTools.runLink dflags
	 ([ SysTools.Option verb
	  , SysTools.Option "-dynamiclib"
	  , SysTools.Option "-o"
	  , SysTools.FileOption "" output_fn
	  ]
	 ++ map SysTools.Option (
	    md_c_flags
	 ++ o_files
	 ++ [ "-undefined", "dynamic_lookup", "-single_module", "-Wl,-macosx_version_min","-Wl,10.3", "-install_name " ++ (pwd `joinFileName` output_fn) ]
	 ++ extra_ld_inputs
	 ++ lib_path_opts
	 ++ extra_ld_opts
	 ++ pkg_lib_path_opts
	 ++ pkg_link_opts
	))
#else
    -----------------------------------------------------------------------------
    -- Making a DSO
    -----------------------------------------------------------------------------

    let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }

    SysTools.runLink dflags
	 ([ SysTools.Option verb
	  , SysTools.Option "-o"
	  , SysTools.FileOption "" output_fn
	  ]
	 ++ map SysTools.Option (
	    md_c_flags
	 ++ o_files
	 ++ [ "-shared", "-Wl,-Bsymbolic" ] -- we need symbolic linking to resolve non-PIC intra-package-relocations
	 ++ extra_ld_inputs
	 ++ lib_path_opts
	 ++ extra_ld_opts
	 ++ pkg_lib_path_opts
	 ++ pkg_link_opts
	))
#endif
-- -----------------------------------------------------------------------------
-- Running CPP

doCpp :: DynFlags -> Bool -> Bool -> FilePath -> FilePath -> IO ()
doCpp dflags raw include_cc_opts input_fn output_fn = do
    let hscpp_opts = getOpts dflags opt_P
    let cmdline_include_paths = includePaths dflags

    pkg_include_dirs <- getPackageIncludePath dflags []
    let include_paths = foldr (\ x xs -> "-I" : x : xs) []
			  (cmdline_include_paths ++ pkg_include_dirs)

    let verb = getVerbFlag dflags

    let cc_opts
	  | not include_cc_opts = []
	  | otherwise           = (optc ++ md_c_flags)
		where 
		      optc = getOpts dflags opt_c
		      (md_c_flags, _) = machdepCCOpts dflags

    let cpp_prog args | raw       = SysTools.runCpp dflags args
	              | otherwise = SysTools.runCc dflags (SysTools.Option "-E" : args)

    let target_defs = 
	  [ "-D" ++ HOST_OS     ++ "_BUILD_OS=1",
	    "-D" ++ HOST_ARCH   ++ "_BUILD_ARCH=1",
	    "-D" ++ TARGET_OS   ++ "_HOST_OS=1",
	    "-D" ++ TARGET_ARCH ++ "_HOST_ARCH=1" ]
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

cHaskell1Version = "5" -- i.e., Haskell 98

-- Default CPP defines in Haskell source
hsSourceCppOpts =
	[ "-D__HASKELL1__="++cHaskell1Version
	, "-D__GLASGOW_HASKELL__="++cProjectVersionInt				
	, "-D__HASKELL98__"
	, "-D__CONCURRENT_HASKELL__"
	]


-- -----------------------------------------------------------------------------
-- Misc.

hscNextPhase :: DynFlags -> HscSource -> HscTarget -> Phase
hscNextPhase dflags HsBootFile hsc_lang  =  StopLn
hscNextPhase dflags other hsc_lang = 
  case hsc_lang of
	HscC -> HCc
	HscAsm | dopt Opt_SplitObjs dflags -> SplitMangle
	       | otherwise -> As
	HscNothing     -> StopLn
	HscInterpreted -> StopLn
	_other         -> StopLn


hscMaybeAdjustTarget :: DynFlags -> Phase -> HscSource -> HscTarget -> HscTarget
hscMaybeAdjustTarget dflags stop other current_hsc_lang 
  = hsc_lang 
  where
	keep_hc = dopt Opt_KeepHcFiles dflags
	hsc_lang
		-- don't change the lang if we're interpreting
		 | current_hsc_lang == HscInterpreted = current_hsc_lang

		-- force -fvia-C if we are being asked for a .hc file
		 | HCc <- stop = HscC
		 | keep_hc     = HscC
		-- otherwise, stick to the plan
		 | otherwise = current_hsc_lang

GLOBAL_VAR(v_Split_info, ("",0), (String,Int))
	-- The split prefix and number of files
