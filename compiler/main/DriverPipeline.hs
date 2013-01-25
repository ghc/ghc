{-# OPTIONS -fno-cse #-}
{-# LANGUAGE NamedFieldPuns #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

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
   compile, compile',
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
import UniqFM           ( eltsUFM )
import ErrUtils
import DynFlags
import Config
import Panic
import Util
import StringBuffer     ( hGetStringBuffer )
import BasicTypes       ( SuccessFlag(..) )
import Maybes           ( expectJust )
import ParserCoreUtils  ( getCoreModuleName )
import SrcLoc
import FastString
import LlvmCodeGen      ( llvmFixupAsm )
import MonadUtils
import Platform

import Exception
import Data.IORef       ( readIORef )
import System.Directory
import System.FilePath
import System.IO
import Control.Monad
import Data.List        ( isSuffixOf )
import Data.Maybe
import System.Environment
import Data.Char

-- ---------------------------------------------------------------------------
-- Pre-process

-- | Just preprocess a file, put the result in a temp. file (used by the
-- compilation manager during the summary phase).
--
-- We return the augmented DynFlags, because they contain the result
-- of slurping in the OPTIONS pragmas

preprocess :: HscEnv
           -> (FilePath, Maybe Phase) -- ^ filename and starting phase
           -> IO (DynFlags, FilePath)
preprocess hsc_env (filename, mb_phase) =
  ASSERT2(isJust mb_phase || isHaskellSrcFilename filename, text filename)
  runPipeline anyHsc hsc_env (filename, mb_phase)
        Nothing Temporary Nothing{-no ModLocation-} Nothing{-no stub-}

-- ---------------------------------------------------------------------------

-- | Compile
--
-- Compile a single module, under the control of the compilation manager.
--
-- This is the interface between the compilation manager and the
-- compiler proper (hsc), where we deal with tedious details like
-- reading the OPTIONS pragma from the source file, converting the
-- C or assembly that GHC produces into an object file, and compiling
-- FFI stub files.
--
-- NB.  No old interface can also mean that the source has changed.

compile :: HscEnv
        -> ModSummary      -- ^ summary for module being compiled
        -> Int             -- ^ module N ...
        -> Int             -- ^ ... of M
        -> Maybe ModIface  -- ^ old interface, if we have one
        -> Maybe Linkable  -- ^ old linkable, if we have one
        -> SourceModified
        -> IO HomeModInfo   -- ^ the complete HomeModInfo, if successful

compile = compile' (hscCompileNothing, hscCompileInteractive, hscCompileBatch)

compile' :: 
           (Compiler (HscStatus, ModIface, ModDetails),
            Compiler (InteractiveStatus, ModIface, ModDetails),
            Compiler (HscStatus, ModIface, ModDetails))
        -> HscEnv
        -> ModSummary      -- ^ summary for module being compiled
        -> Int             -- ^ module N ...
        -> Int             -- ^ ... of M
        -> Maybe ModIface  -- ^ old interface, if we have one
        -> Maybe Linkable  -- ^ old linkable, if we have one
        -> SourceModified
        -> IO HomeModInfo   -- ^ the complete HomeModInfo, if successful

compile' (nothingCompiler, interactiveCompiler, batchCompiler)
        hsc_env0 summary mod_index nmods mb_old_iface maybe_old_linkable
        source_modified0
 = do
   let dflags0     = ms_hspp_opts summary
       this_mod    = ms_mod summary
       src_flavour = ms_hsc_src summary
       location    = ms_location summary
       input_fn    = expectJust "compile:hs" (ml_hs_file location)
       input_fnpp  = ms_hspp_file summary

   debugTraceMsg dflags0 2 (text "compile: input file" <+> text input_fnpp)

   let basename = dropExtension input_fn

  -- We add the directory in which the .hs files resides) to the import path.
  -- This is needed when we try to compile the .hc file later, if it
  -- imports a _stub.h file that we created here.
   let current_dir = takeDirectory basename
       old_paths   = includePaths dflags0
       dflags      = dflags0 { includePaths = current_dir : old_paths }
       hsc_env     = hsc_env0 {hsc_dflags = dflags}

   -- Figure out what lang we're generating
   let hsc_lang = hscTarget dflags
   -- ... and what the next phase should be
   let next_phase = hscPostBackendPhase dflags src_flavour hsc_lang
   -- ... and what file to generate the output into
   output_fn <- getOutputFilename next_phase
                        Temporary basename dflags next_phase (Just location)

   let dflags' = dflags { hscTarget = hsc_lang,
                                hscOutName = output_fn,
                                extCoreName = basename ++ ".hcr" }
   let hsc_env' = hsc_env { hsc_dflags = dflags' }

   -- -fforce-recomp should also work with --make
   let force_recomp = gopt Opt_ForceRecomp dflags
       source_modified
         | force_recomp || isNothing maybe_old_linkable = SourceModified
         | otherwise = source_modified0
       object_filename = ml_obj_file location

   let handleBatch HscNoRecomp
           = ASSERT (isJust maybe_old_linkable)
             return maybe_old_linkable

       handleBatch (HscRecomp hasStub _)
           | isHsBoot src_flavour
               = do when (isObjectTarget hsc_lang) $ -- interpreted reaches here too
                       liftIO $ touchObjectFile dflags' object_filename
                    return maybe_old_linkable

           | otherwise
               = do (hs_unlinked, unlinked_time) <-
                        case hsc_lang of
                          HscNothing ->
                            return ([], ms_hs_date summary)
                          -- We're in --make mode: finish the compilation pipeline.
                          _other -> do
                            maybe_stub_o <- case hasStub of
                               Nothing -> return Nothing
                               Just stub_c -> do
                                 stub_o <- compileStub hsc_env' stub_c
                                 return (Just stub_o)
                            _ <- runPipeline StopLn hsc_env' (output_fn,Nothing)
                                              (Just basename)
                                              Persistent
                                              (Just location)
                                              maybe_stub_o
                                  -- The object filename comes from the ModLocation
                            o_time <- getModificationUTCTime object_filename
                            return ([DotO object_filename], o_time)
                    
                    let linkable = LM unlinked_time this_mod hs_unlinked
                    return (Just linkable)

       handleInterpreted HscNoRecomp
           = ASSERT (isJust maybe_old_linkable)
             return maybe_old_linkable
       handleInterpreted (HscRecomp _hasStub Nothing)
           = ASSERT (isHsBoot src_flavour)
             return maybe_old_linkable
       handleInterpreted (HscRecomp hasStub (Just (comp_bc, modBreaks)))
           = do stub_o <- case hasStub of
                            Nothing -> return []
                            Just stub_c -> do
                              stub_o <- compileStub hsc_env' stub_c
                              return [DotO stub_o]

                let hs_unlinked = [BCOs comp_bc modBreaks]
                    unlinked_time = ms_hs_date summary
                  -- Why do we use the timestamp of the source file here,
                  -- rather than the current time?  This works better in
                  -- the case where the local clock is out of sync
                  -- with the filesystem's clock.  It's just as accurate:
                  -- if the source is modified, then the linkable will
                  -- be out of date.
                let linkable = LM unlinked_time this_mod
                               (hs_unlinked ++ stub_o)
                return (Just linkable)

   let -- runCompiler :: Compiler result -> (result -> Maybe Linkable)
       --            -> m HomeModInfo
       runCompiler compiler handle
           = do (result, iface, details)
                    <- compiler hsc_env' summary source_modified mb_old_iface
                                (Just (mod_index, nmods))
                linkable <- handle result
                return (HomeModInfo{ hm_details  = details,
                                     hm_iface    = iface,
                                     hm_linkable = linkable })
   -- run the compiler
   case hsc_lang of
      HscInterpreted -> runCompiler interactiveCompiler handleInterpreted
      HscNothing     -> runCompiler nothingCompiler     handleBatch
      _other         -> runCompiler batchCompiler       handleBatch

-----------------------------------------------------------------------------
-- stub .h and .c files (for foreign export support)

-- The _stub.c file is derived from the haskell source file, possibly taking
-- into account the -stubdir option.
--
-- The object file created by compiling the _stub.c file is put into a
-- temporary file, which will be later combined with the main .o file
-- (see the MergeStubs phase).

compileStub :: HscEnv -> FilePath -> IO FilePath
compileStub hsc_env stub_c = do
        (_, stub_o) <- runPipeline StopLn hsc_env (stub_c,Nothing)  Nothing
                                   Temporary Nothing{-no ModLocation-} Nothing

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

link LinkInMemory _ _ _
    = if cGhcWithInterpreter == "YES"
      then -- Not Linking...(demand linker will do the job)
           return Succeeded
      else panicBadLink LinkInMemory

link NoLink _ _ _
   = return Succeeded

link LinkBinary dflags batch_attempt_linking hpt
   = link' dflags batch_attempt_linking hpt

link LinkDynLib dflags batch_attempt_linking hpt
   = link' dflags batch_attempt_linking hpt

panicBadLink :: GhcLink -> a
panicBadLink other = panic ("link: GHC not built to link this way: " ++
                            show other)

link' :: DynFlags                -- dynamic flags
      -> Bool                    -- attempt linking in batch mode?
      -> HomePackageTable        -- what to link
      -> IO SuccessFlag

link' dflags batch_attempt_linking hpt
   | batch_attempt_linking
   = do
        let
            home_mod_infos = eltsUFM hpt

            -- the packages we depend on
            pkg_deps  = concatMap (map fst . dep_pkgs . mi_deps . hm_iface) home_mod_infos

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

        linking_needed <- linkingNeeded dflags linkables pkg_deps

        if not (gopt Opt_ForceRecomp dflags) && not linking_needed
           then do debugTraceMsg dflags 2 (text exe_file <+> ptext (sLit "is up to date, linking not required."))
                   return Succeeded
           else do

        compilationProgressMsg dflags ("Linking " ++ exe_file ++ " ...")

        -- Don't showPass in Batch mode; doLink will do that for us.
        let link = case ghcLink dflags of
                LinkBinary  -> linkBinary
                LinkDynLib  -> linkDynLibCheck
                other       -> panicBadLink other
        link dflags obj_files pkg_deps

        debugTraceMsg dflags 3 (text "link: done")

        -- linkBinary only returns if it succeeds
        return Succeeded

   | otherwise
   = do debugTraceMsg dflags 3 (text "link(batch): upsweep (partially) failed OR" $$
                                text "   Main.main not exported; not linking.")
        return Succeeded


linkingNeeded :: DynFlags -> [Linkable] -> [PackageId] -> IO Bool
linkingNeeded dflags linkables pkg_deps = do
        -- if the modification time on the executable is later than the
        -- modification times on all of the objects and libraries, then omit
        -- linking (unless the -fforce-recomp flag was given).
  let exe_file = exeFileName dflags
  e_exe_time <- tryIO $ getModificationUTCTime exe_file
  case e_exe_time of
    Left _  -> return True
    Right t -> do
        -- first check object files and extra_ld_inputs
        let extra_ld_inputs = ldInputs dflags
        e_extra_times <- mapM (tryIO . getModificationUTCTime) extra_ld_inputs
        let (errs,extra_times) = splitEithers e_extra_times
        let obj_times =  map linkableTime linkables ++ extra_times
        if not (null errs) || any (t <) obj_times
            then return True
            else do

        -- next, check libraries. XXX this only checks Haskell libraries,
        -- not extra_libraries or -l things from the command line.
        let pkg_map = pkgIdMap (pkgState dflags)
            pkg_hslibs  = [ (libraryDirs c, lib)
                          | Just c <- map (lookupPackage pkg_map) pkg_deps,
                            lib <- packageHsLibs dflags c ]

        pkg_libfiles <- mapM (uncurry (findHSLib dflags)) pkg_hslibs
        if any isNothing pkg_libfiles then return True else do
        e_lib_times <- mapM (tryIO . getModificationUTCTime)
                          (catMaybes pkg_libfiles)
        let (lib_errs,lib_times) = splitEithers e_lib_times
        if not (null lib_errs) || any (t <) lib_times
           then return True
           else checkLinkInfo dflags pkg_deps exe_file

-- Returns 'False' if it was, and we can avoid linking, because the
-- previous binary was linked with "the same options".
checkLinkInfo :: DynFlags -> [PackageId] -> FilePath -> IO Bool
checkLinkInfo dflags pkg_deps exe_file
 | not (platformSupportsSavingLinkOpts (platformOS (targetPlatform dflags)))
 -- ToDo: Windows and OS X do not use the ELF binary format, so
 -- readelf does not work there.  We need to find another way to do
 -- this.
 = return False -- conservatively we should return True, but not
                -- linking in this case was the behaviour for a long
                -- time so we leave it as-is.
 | otherwise
 = do
   link_info <- getLinkInfo dflags pkg_deps
   debugTraceMsg dflags 3 $ text ("Link info: " ++ link_info)
   m_exe_link_info <- readElfSection dflags ghcLinkInfoSectionName exe_file
   debugTraceMsg dflags 3 $ text ("Exe link info: " ++ show m_exe_link_info)
   return (Just link_info /= m_exe_link_info)

platformSupportsSavingLinkOpts :: OS -> Bool
platformSupportsSavingLinkOpts os
  | os == OSSolaris2 = False -- see #5382
  | otherwise        = osElfTarget os

ghcLinkInfoSectionName :: String
ghcLinkInfoSectionName = ".debug-ghc-link-info"
   -- if we use the ".debug" prefix, then strip will strip it by default

findHSLib :: DynFlags -> [String] -> String -> IO (Maybe FilePath)
findHSLib dflags dirs lib = do
  let batch_lib_file = if gopt Opt_Static dflags
                       then "lib" ++ lib <.> "a"
                       else mkSOName (targetPlatform dflags) lib
  found <- filterM doesFileExist (map (</> batch_lib_file) dirs)
  case found of
    [] -> return Nothing
    (x:_) -> return (Just x)

-- -----------------------------------------------------------------------------
-- Compile files in one-shot mode.

oneShot :: HscEnv -> Phase -> [(String, Maybe Phase)] -> IO ()
oneShot hsc_env stop_phase srcs = do
  o_files <- mapM (compileFile hsc_env stop_phase) srcs
  doLink (hsc_dflags hsc_env) stop_phase o_files

compileFile :: HscEnv -> Phase -> (FilePath, Maybe Phase) -> IO FilePath
compileFile hsc_env stop_phase (src, mb_phase) = do
   exists <- doesFileExist src
   when (not exists) $
        throwGhcException (CmdLineError ("does not exist: " ++ src))

   let
        dflags = hsc_dflags hsc_env
        split     = gopt Opt_SplitObjs dflags
        mb_o_file = outputFile dflags
        ghc_link  = ghcLink dflags      -- Set by -c or -no-link

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
                        _          -> stop_phase

   ( _, out_file) <- runPipeline stop_phase' hsc_env
                            (src, mb_phase) Nothing output
                            Nothing{-no ModLocation-} Nothing
   return out_file


doLink :: DynFlags -> Phase -> [FilePath] -> IO ()
doLink dflags stop_phase o_files
  | not (isStopLn stop_phase)
  = return ()           -- We stopped before the linking phase

  | otherwise
  = case ghcLink dflags of
        NoLink     -> return ()
        LinkBinary -> linkBinary      dflags o_files []
        LinkDynLib -> linkDynLibCheck dflags o_files []
        other      -> panicBadLink other


-- ---------------------------------------------------------------------------

data PipelineOutput
  = Temporary
        -- ^ Output should be to a temporary file: we're going to
        -- run more compilation steps on this output later.
  | Persistent
        -- ^ We want a persistent file, i.e. a file in the current directory
        -- derived from the input filename, but with the appropriate extension.
        -- eg. in "ghc -c Foo.hs" the output goes into ./Foo.o.
  | SpecificFile FilePath
        -- ^ The output must go into the specified file.

-- | Run a compilation pipeline, consisting of multiple phases.
--
-- This is the interface to the compilation pipeline, which runs
-- a series of compilation steps on a single source file, specifying
-- at which stage to stop.
--
-- The DynFlags can be modified by phases in the pipeline (eg. by
-- OPTIONS_GHC pragmas), and the changes affect later phases in the
-- pipeline.
runPipeline
  :: Phase                      -- ^ When to stop
  -> HscEnv                     -- ^ Compilation environment
  -> (FilePath,Maybe Phase)     -- ^ Input filename (and maybe -x suffix)
  -> Maybe FilePath             -- ^ original basename (if different from ^^^)
  -> PipelineOutput             -- ^ Output filename
  -> Maybe ModLocation          -- ^ A ModLocation, if this is a Haskell module
  -> Maybe FilePath             -- ^ stub object, if we have one
  -> IO (DynFlags, FilePath)     -- ^ (final flags, output filename)
runPipeline stop_phase hsc_env0 (input_fn, mb_phase)
             mb_basename output maybe_loc maybe_stub_o

    = do let
             dflags0 = hsc_dflags hsc_env0

             -- Decide where dump files should go based on the pipeline output
             dflags = dflags0 { dumpPrefix = Just (basename ++ ".") }
             hsc_env = hsc_env0 {hsc_dflags = dflags}

             (input_basename, suffix) = splitExtension input_fn
             suffix' = drop 1 suffix -- strip off the .
             basename | Just b <- mb_basename = b
                      | otherwise             = input_basename

             -- If we were given a -x flag, then use that phase to start from
             start_phase = fromMaybe (startPhase suffix') mb_phase

             isHaskell (Unlit _) = True
             isHaskell (Cpp   _) = True
             isHaskell (HsPp  _) = True
             isHaskell (Hsc   _) = True
             isHaskell _         = False

             isHaskellishFile = isHaskell start_phase

             env = PipeEnv{ pe_isHaskellishFile = isHaskellishFile,
                            stop_phase,
                            src_basename = basename,
                            src_suffix = suffix',
                            output_spec = output }

         -- We want to catch cases of "you can't get there from here" before
         -- we start the pipeline, because otherwise it will just run off the
         -- end.
         --
         -- There is a partial ordering on phases, where A < B iff A occurs
         -- before B in a normal compilation pipeline.

         let happensBefore' = happensBefore dflags
         when (not (start_phase `happensBefore'` stop_phase)) $
               throwGhcException (UsageError
                           ("cannot compile this file to desired target: "
                              ++ input_fn))

         debugTraceMsg dflags 4 (text "Running the pipeline")
         r <- runPipeline' start_phase stop_phase hsc_env env input_fn
                           output maybe_loc maybe_stub_o

         -- If we are compiling a Haskell module, and doing
         -- -dynamic-too, but couldn't do the -dynamic-too fast
         -- path, then rerun the pipeline for the dyn way
         let dflags = extractDynFlags hsc_env
         when isHaskellishFile $ whenCannotGenerateDynamicToo dflags $ do
             debugTraceMsg dflags 4
                 (text "Running the pipeline again for -dynamic-too")
             let dflags' = doDynamicToo dflags
                 -- TODO: This should use -dyno
                 output' = case output of
                           SpecificFile fn -> SpecificFile (replaceExtension fn (objectSuf dflags'))
                           Persistent -> Persistent
                           Temporary -> Temporary
             hsc_env' <- newHscEnv dflags'
             _ <- runPipeline' start_phase stop_phase hsc_env' env input_fn
                               output' maybe_loc maybe_stub_o
             return ()
         return r

runPipeline'
  :: Phase                      -- ^ When to start
  -> Phase                      -- ^ When to stop
  -> HscEnv                     -- ^ Compilation environment
  -> PipeEnv
  -> FilePath                   -- ^ Input filename
  -> PipelineOutput             -- ^ Output filename
  -> Maybe ModLocation          -- ^ A ModLocation, if this is a Haskell module
  -> Maybe FilePath             -- ^ stub object, if we have one
  -> IO (DynFlags, FilePath)    -- ^ (final flags, output filename)
runPipeline' start_phase stop_phase hsc_env env input_fn
             output maybe_loc maybe_stub_o
  = do
  -- this is a function which will be used to calculate output file names
  -- as we go along (we partially apply it to some of its inputs here)
  let get_output_fn = getOutputFilename stop_phase output (src_basename env)

  -- Execute the pipeline...
  let state = PipeState{ hsc_env, maybe_loc, maybe_stub_o = maybe_stub_o }

  (state', output_fn) <- unP (pipeLoop start_phase input_fn) env state

  let PipeState{ hsc_env=hsc_env', maybe_loc } = state'
      dflags = hsc_dflags hsc_env'

  -- Sometimes, a compilation phase doesn't actually generate any output
  -- (eg. the CPP phase when -fcpp is not turned on).  If we end on this
  -- stage, but we wanted to keep the output, then we have to explicitly
  -- copy the file, remembering to prepend a {-# LINE #-} pragma so that
  -- further compilation stages can tell what the original filename was.
  case output of
    Temporary ->
        return (dflags, output_fn)
    _ ->
        do final_fn <- get_output_fn dflags stop_phase maybe_loc
           when (final_fn /= output_fn) $ do
              let msg = ("Copying `" ++ output_fn ++"' to `" ++ final_fn ++ "'")
                  line_prag = Just ("{-# LINE 1 \"" ++ input_fn ++ "\" #-}\n")
              copyWithHeader dflags msg line_prag output_fn final_fn
           return (dflags, final_fn)

-- -----------------------------------------------------------------------------
-- The pipeline uses a monad to carry around various bits of information

-- PipeEnv: invariant information passed down
data PipeEnv = PipeEnv {
       pe_isHaskellishFile :: Bool,
       stop_phase   :: Phase,       -- ^ Stop just before this phase
       src_basename :: String,      -- ^ basename of original input source
       src_suffix   :: String,      -- ^ its extension
       output_spec  :: PipelineOutput -- ^ says where to put the pipeline output
  }

-- PipeState: information that might change during a pipeline run
data PipeState = PipeState {
       hsc_env   :: HscEnv,
          -- ^ only the DynFlags change in the HscEnv.  The DynFlags change
          -- at various points, for example when we read the OPTIONS_GHC
          -- pragmas in the Cpp phase.
       maybe_loc :: Maybe ModLocation,
          -- ^ the ModLocation.  This is discovered during compilation,
          -- in the Hsc phase where we read the module header.
       maybe_stub_o :: Maybe FilePath
          -- ^ the stub object.  This is set by the Hsc phase if a stub
          -- object was created.  The stub object will be joined with
          -- the main compilation object using "ld -r" at the end.
  }

getPipeEnv :: CompPipeline PipeEnv
getPipeEnv = P $ \env state -> return (state, env)

getPipeState :: CompPipeline PipeState
getPipeState = P $ \_env state -> return (state, state)

instance HasDynFlags CompPipeline where
    getDynFlags = P $ \_env state -> return (state, hsc_dflags (hsc_env state))

setDynFlags :: DynFlags -> CompPipeline ()
setDynFlags dflags = P $ \_env state ->
  return (state{hsc_env= (hsc_env state){ hsc_dflags = dflags }}, ())

setModLocation :: ModLocation -> CompPipeline ()
setModLocation loc = P $ \_env state ->
  return (state{ maybe_loc = Just loc }, ())

setStubO :: FilePath -> CompPipeline ()
setStubO stub_o = P $ \_env state ->
  return (state{ maybe_stub_o = Just stub_o }, ())

newtype CompPipeline a = P { unP :: PipeEnv -> PipeState -> IO (PipeState, a) }

instance Monad CompPipeline where
  return a = P $ \_env state -> return (state, a)
  P m >>= k = P $ \env state -> do (state',a) <- m env state
                                   unP (k a) env state'

instance MonadIO CompPipeline where
    liftIO m = P $ \_env state -> do a <- m; return (state, a)

phaseOutputFilename :: Phase{-next phase-} -> CompPipeline FilePath
phaseOutputFilename next_phase = do
  PipeEnv{stop_phase, src_basename, output_spec} <- getPipeEnv
  PipeState{maybe_loc, hsc_env} <- getPipeState
  let dflags = hsc_dflags hsc_env
  liftIO $ getOutputFilename stop_phase output_spec
                             src_basename dflags next_phase maybe_loc

-- ---------------------------------------------------------------------------
-- outer pipeline loop

-- | pipeLoop runs phases until we reach the stop phase
pipeLoop :: Phase -> FilePath -> CompPipeline FilePath
pipeLoop phase input_fn = do
  PipeEnv{stop_phase} <- getPipeEnv
  dflags <- getDynFlags
  let happensBefore' = happensBefore dflags
  case () of
   _ | phase `eqPhase` stop_phase            -- All done
     -> return input_fn

     | not (phase `happensBefore'` stop_phase)
        -- Something has gone wrong.  We'll try to cover all the cases when
        -- this could happen, so if we reach here it is a panic.
        -- eg. it might happen if the -C flag is used on a source file that
        -- has {-# OPTIONS -fasm #-}.
     -> panic ("pipeLoop: at phase " ++ show phase ++
           " but I wanted to stop at phase " ++ show stop_phase)

     | otherwise
     -> do liftIO $ debugTraceMsg dflags 4
                                  (ptext (sLit "Running phase") <+> ppr phase)
           (next_phase, output_fn) <- runPhase phase input_fn dflags
           pipeLoop next_phase output_fn

-- -----------------------------------------------------------------------------
-- In each phase, we need to know into what filename to generate the
-- output.  All the logic about which filenames we generate output
-- into is embodied in the following function.

getOutputFilename
  :: Phase -> PipelineOutput -> String
  -> DynFlags -> Phase{-next phase-} -> Maybe ModLocation -> IO FilePath
getOutputFilename stop_phase output basename
 = func
 where
        func dflags next_phase maybe_location
           | is_last_phase, Persistent <- output     = persistent_fn
           | is_last_phase, SpecificFile f <- output = return f
           | keep_this_output                        = persistent_fn
           | otherwise                               = newTempName dflags suffix
           where
                hcsuf      = hcSuf dflags
                odir       = objectDir dflags
                osuf       = objectSuf dflags
                keep_hc    = gopt Opt_KeepHcFiles dflags
                keep_s     = gopt Opt_KeepSFiles dflags
                keep_bc    = gopt Opt_KeepLlvmFiles dflags

                myPhaseInputExt HCc       = hcsuf
                myPhaseInputExt MergeStub = osuf
                myPhaseInputExt StopLn    = osuf
                myPhaseInputExt other     = phaseInputExt other

                is_last_phase = next_phase `eqPhase` stop_phase

                -- sometimes, we keep output from intermediate stages
                keep_this_output =
                     case next_phase of
                             As      | keep_s     -> True
                             LlvmOpt | keep_bc    -> True
                             HCc     | keep_hc    -> True
                             _other               -> False

                suffix = myPhaseInputExt next_phase

                -- persistent object files get put in odir
                persistent_fn
                   | StopLn <- next_phase = return odir_persistent
                   | otherwise            = return persistent

                persistent = basename <.> suffix

                odir_persistent
                   | Just loc <- maybe_location = ml_obj_file loc
                   | Just d <- odir = d </> persistent
                   | otherwise      = persistent


-- -----------------------------------------------------------------------------
-- | Each phase in the pipeline returns the next phase to execute, and the
-- name of the file in which the output was placed.
--
-- We must do things dynamically this way, because we often don't know
-- what the rest of the phases will be until part-way through the
-- compilation: for example, an {-# OPTIONS -fasm #-} at the beginning
-- of a source file can change the latter stages of the pipeline from
-- taking the via-C route to using the native code generator.
--
runPhase :: Phase       -- ^ Run this phase
         -> FilePath    -- ^ name of the input file
         -> DynFlags    -- ^ for convenience, we pass the current dflags in
         -> CompPipeline (Phase,               -- next phase to run
                          FilePath)            -- output filename

        -- Invariant: the output filename always contains the output
        -- Interesting case: Hsc when there is no recompilation to do
        --                   Then the output filename is still a .o file


-------------------------------------------------------------------------------
-- Unlit phase

runPhase (Unlit sf) input_fn dflags
  = do
       output_fn <- phaseOutputFilename (Cpp sf)

       let unlit_flags = getOpts dflags opt_L
           flags = map SysTools.Option unlit_flags ++
                   [ -- The -h option passes the file name for unlit to
                     -- put in a #line directive
                     SysTools.Option     "-h"
                   , SysTools.Option $ escape $ normalise input_fn
                   , SysTools.FileOption "" input_fn
                   , SysTools.FileOption "" output_fn
                   ]

       liftIO $ SysTools.runUnlit dflags flags

       return (Cpp sf, output_fn)
  where
       -- escape the characters \, ", and ', but don't try to escape
       -- Unicode or anything else (so we don't use Util.charToC
       -- here).  If we get this wrong, then in
       -- Coverage.addTicksToBinds where we check that the filename in
       -- a SrcLoc is the same as the source filenaame, the two will
       -- look bogusly different. See test:
       -- libraries/hpc/tests/function/subdir/tough2.lhs
       escape ('\\':cs) = '\\':'\\': escape cs
       escape ('\"':cs) = '\\':'\"': escape cs
       escape ('\'':cs) = '\\':'\'': escape cs
       escape (c:cs)    = c : escape cs
       escape []        = []

-------------------------------------------------------------------------------
-- Cpp phase : (a) gets OPTIONS out of file
--             (b) runs cpp if necessary

runPhase (Cpp sf) input_fn dflags0
  = do
       src_opts <- liftIO $ getOptionsFromFile dflags0 input_fn
       (dflags1, unhandled_flags, warns)
           <- liftIO $ parseDynamicFilePragma dflags0 src_opts
       setDynFlags dflags1
       liftIO $ checkProcessArgsResult dflags1 unhandled_flags

       if not (xopt Opt_Cpp dflags1) then do
           -- we have to be careful to emit warnings only once.
           unless (gopt Opt_Pp dflags1) $
               liftIO $ handleFlagWarnings dflags1 warns

           -- no need to preprocess CPP, just pass input file along
           -- to the next phase of the pipeline.
           return (HsPp sf, input_fn)
        else do
            output_fn <- phaseOutputFilename (HsPp sf)
            liftIO $ doCpp dflags1 True{-raw-} False{-no CC opts-}
                           input_fn output_fn
            -- re-read the pragmas now that we've preprocessed the file
            -- See #2464,#3457
            src_opts <- liftIO $ getOptionsFromFile dflags0 output_fn
            (dflags2, unhandled_flags, warns)
                <- liftIO $ parseDynamicFilePragma dflags0 src_opts
            liftIO $ checkProcessArgsResult dflags2 unhandled_flags
            unless (gopt Opt_Pp dflags2) $
                liftIO $ handleFlagWarnings dflags2 warns
            -- the HsPp pass below will emit warnings

            setDynFlags dflags2

            return (HsPp sf, output_fn)

-------------------------------------------------------------------------------
-- HsPp phase

runPhase (HsPp sf) input_fn dflags
  = do
       if not (gopt Opt_Pp dflags) then
           -- no need to preprocess, just pass input file along
           -- to the next phase of the pipeline.
          return (Hsc sf, input_fn)
        else do
            let hspp_opts = getOpts dflags opt_F
            PipeEnv{src_basename, src_suffix} <- getPipeEnv
            let orig_fn = src_basename <.> src_suffix
            output_fn <- phaseOutputFilename (Hsc sf)
            liftIO $ SysTools.runPp dflags
                           ( [ SysTools.Option     orig_fn
                             , SysTools.Option     input_fn
                             , SysTools.FileOption "" output_fn
                             ] ++
                             map SysTools.Option hspp_opts
                           )

            -- re-read pragmas now that we've parsed the file (see #3674)
            src_opts <- liftIO $ getOptionsFromFile dflags output_fn
            (dflags1, unhandled_flags, warns)
                <- liftIO $ parseDynamicFilePragma dflags src_opts
            setDynFlags dflags1
            liftIO $ checkProcessArgsResult dflags1 unhandled_flags
            liftIO $ handleFlagWarnings dflags1 warns

            return (Hsc sf, output_fn)

-----------------------------------------------------------------------------
-- Hsc phase

-- Compilation of a single module, in "legacy" mode (_not_ under
-- the direction of the compilation manager).
runPhase (Hsc src_flavour) input_fn dflags0
 = do   -- normal Hsc mode, not mkdependHS

        PipeEnv{ stop_phase=stop,
                 src_basename=basename,
                 src_suffix=suff } <- getPipeEnv

  -- we add the current directory (i.e. the directory in which
  -- the .hs files resides) to the include path, since this is
  -- what gcc does, and it's probably what you want.
        let current_dir = takeDirectory basename
            paths = includePaths dflags0
            dflags = dflags0 { includePaths = current_dir : paths }

        setDynFlags dflags

  -- gather the imports and module name
        (hspp_buf,mod_name,imps,src_imps) <- liftIO $
            case src_flavour of
                ExtCoreFile -> do  -- no explicit imports in ExtCore input.
                    m <- getCoreModuleName input_fn
                    return (Nothing, mkModuleName m, [], [])

                _           -> do
                    buf <- hGetStringBuffer input_fn
                    (src_imps,imps,L _ mod_name) <- getImports dflags buf input_fn (basename <.> suff)
                    return (Just buf, mod_name, imps, src_imps)

  -- Build a ModLocation to pass to hscMain.
  -- The source filename is rather irrelevant by now, but it's used
  -- by hscMain for messages.  hscMain also needs
  -- the .hi and .o filenames, and this is as good a way
  -- as any to generate them, and better than most. (e.g. takes
  -- into accout the -osuf flags)
        location1 <- liftIO $ mkHomeModLocation2 dflags mod_name basename suff

  -- Boot-ify it if necessary
        let location2 | isHsBoot src_flavour = addBootSuffixLocn location1
                      | otherwise            = location1


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

            o_file = ml_obj_file location4      -- The real object file

        setModLocation location4

  -- Figure out if the source has changed, for recompilation avoidance.
  --
  -- Setting source_unchanged to True means that M.o seems
  -- to be up to date wrt M.hs; so no need to recompile unless imports have
  -- changed (which the compiler itself figures out).
  -- Setting source_unchanged to False tells the compiler that M.o is out of
  -- date wrt M.hs (or M.o doesn't exist) so we must recompile regardless.
        src_timestamp <- liftIO $ getModificationUTCTime (basename <.> suff)

        let hsc_lang = hscTarget dflags
        source_unchanged <- liftIO $
          if not (isStopLn stop)
                -- SourceModified unconditionally if
                --      (a) recompilation checker is off, or
                --      (b) we aren't going all the way to .o file (e.g. ghc -S)
             then return SourceModified
                -- Otherwise look at file modification dates
             else do o_file_exists <- doesFileExist o_file
                     if not o_file_exists
                        then return SourceModified       -- Need to recompile
                        else do t2 <- getModificationUTCTime o_file
                                if t2 > src_timestamp
                                  then return SourceUnmodified
                                  else return SourceModified

  -- get the DynFlags
        let next_phase = hscPostBackendPhase dflags src_flavour hsc_lang
        output_fn  <- phaseOutputFilename next_phase

        let dflags' = dflags { hscTarget = hsc_lang,
                               hscOutName = output_fn,
                               extCoreName = basename ++ ".hcr" }

        setDynFlags dflags'
        PipeState{hsc_env=hsc_env'} <- getPipeState

  -- Tell the finder cache about this module
        mod <- liftIO $ addHomeModuleToFinder hsc_env' mod_name location4

  -- Make the ModSummary to hand to hscMain
        let
            mod_summary = ModSummary {  ms_mod       = mod,
                                        ms_hsc_src   = src_flavour,
                                        ms_hspp_file = input_fn,
                                        ms_hspp_opts = dflags,
                                        ms_hspp_buf  = hspp_buf,
                                        ms_location  = location4,
                                        ms_hs_date   = src_timestamp,
                                        ms_obj_date  = Nothing,
                                        ms_textual_imps = imps,
                                        ms_srcimps      = src_imps }

  -- run the compiler!
        result <- liftIO $ hscCompileOneShot hsc_env'
                               mod_summary source_unchanged
                               Nothing -- No iface
                               Nothing -- No "module i of n" progress info

        case result of
          HscNoRecomp
              -> do liftIO $ touchObjectFile dflags' o_file
                    -- The .o file must have a later modification date
                    -- than the source file (else we wouldn't be in HscNoRecomp)
                    -- but we touch it anyway, to keep 'make' happy (we think).
                    return (StopLn, o_file)
          (HscRecomp hasStub _)
              -> do case hasStub of
                      Nothing -> return ()
                      Just stub_c ->
                          do stub_o <- liftIO $ compileStub hsc_env' stub_c
                             setStubO stub_o
                    -- In the case of hs-boot files, generate a dummy .o-boot
                    -- stamp file for the benefit of Make
                    when (isHsBoot src_flavour) $
                        liftIO $ touchObjectFile dflags' o_file
                    return (next_phase, output_fn)

-----------------------------------------------------------------------------
-- Cmm phase

runPhase CmmCpp input_fn dflags
  = do
       output_fn <- phaseOutputFilename Cmm
       liftIO $ doCpp dflags False{-not raw-} True{-include CC opts-}
                      input_fn output_fn
       return (Cmm, output_fn)

runPhase Cmm input_fn dflags
  = do
        PipeEnv{src_basename} <- getPipeEnv
        let hsc_lang = hscTarget dflags

        let next_phase = hscPostBackendPhase dflags HsSrcFile hsc_lang

        output_fn <- phaseOutputFilename next_phase

        let dflags' = dflags { hscTarget = hsc_lang,
                               hscOutName = output_fn,
                               extCoreName = src_basename ++ ".hcr" }

        setDynFlags dflags'
        PipeState{hsc_env} <- getPipeState

        liftIO $ hscCompileCmmFile hsc_env input_fn

        return (next_phase, output_fn)

-----------------------------------------------------------------------------
-- Cc phase

-- we don't support preprocessing .c files (with -E) now.  Doing so introduces
-- way too many hacks, and I can't say I've ever used it anyway.

runPhase cc_phase input_fn dflags
   | any (cc_phase `eqPhase`) [Cc, Ccpp, HCc, Cobjc, Cobjcpp]
   = do
        let platform = targetPlatform dflags
            cc_opts = getOpts dflags opt_c
            hcc = cc_phase `eqPhase` HCc

        let cmdline_include_paths = includePaths dflags

        -- HC files have the dependent packages stamped into them
        pkgs <- if hcc then liftIO $ getHCFilePackages input_fn else return []

        -- add package include paths even if we're just compiling .c
        -- files; this is the Value Add(TM) that using ghc instead of
        -- gcc gives you :)
        pkg_include_dirs <- liftIO $ getPackageIncludePath dflags pkgs
        let include_paths = foldr (\ x xs -> "-I" : x : xs) []
                              (cmdline_include_paths ++ pkg_include_dirs)

        let gcc_extra_viac_flags = extraGccViaCFlags dflags
        let pic_c_flags = picCCOpts dflags

        let verbFlags = getVerbFlags dflags

        -- cc-options are not passed when compiling .hc files.  Our
        -- hc code doesn't not #include any header files anyway, so these
        -- options aren't necessary.
        pkg_extra_cc_opts <- liftIO $
          if cc_phase `eqPhase` HCc
             then return []
             else getPackageExtraCcOpts dflags pkgs

        framework_paths <-
            case platformOS platform of
            OSDarwin ->
                do pkgFrameworkPaths <- liftIO $ getPackageFrameworkPath dflags pkgs
                   let cmdlineFrameworkPaths = frameworkPaths dflags
                   return $ map ("-F"++)
                                (cmdlineFrameworkPaths ++ pkgFrameworkPaths)
            _ ->
                return []

        let split_objs = gopt Opt_SplitObjs dflags
            split_opt | hcc && split_objs = [ "-DUSE_SPLIT_MARKERS" ]
                      | otherwise         = [ ]

        let cc_opt | optLevel dflags >= 2 = "-O2"
                   | otherwise            = "-O"

        -- Decide next phase
        let next_phase = As
        output_fn <- phaseOutputFilename next_phase

        let
          more_hcc_opts =
                -- on x86 the floating point regs have greater precision
                -- than a double, which leads to unpredictable results.
                -- By default, we turn this off with -ffloat-store unless
                -- the user specified -fexcess-precision.
                (if platformArch platform == ArchX86 &&
                    not (gopt Opt_ExcessPrecision dflags)
                        then [ "-ffloat-store" ]
                        else []) ++

                -- gcc's -fstrict-aliasing allows two accesses to memory
                -- to be considered non-aliasing if they have different types.
                -- This interacts badly with the C code we generate, which is
                -- very weakly typed, being derived from C--.
                ["-fno-strict-aliasing"]

        let gcc_lang_opt | cc_phase `eqPhase` Ccpp  = "c++"
                         | cc_phase `eqPhase` Cobjc = "objective-c"
                         | cc_phase `eqPhase` Cobjcpp = "objective-c++"
                         | otherwise                = "c"
        liftIO $ SysTools.runCc dflags (
                -- force the C compiler to interpret this file as C when
                -- compiling .hc files, by adding the -x c option.
                -- Also useful for plain .c files, just in case GHC saw a
                -- -x c option.
                        [ SysTools.Option "-x", SysTools.Option gcc_lang_opt
                        , SysTools.FileOption "" input_fn
                        , SysTools.Option "-o"
                        , SysTools.FileOption "" output_fn
                        ]
                       ++ map SysTools.Option (
                          pic_c_flags

                -- Stub files generated for foreign exports references the runIO_closure
                -- and runNonIO_closure symbols, which are defined in the base package.
                -- These symbols are imported into the stub.c file via RtsAPI.h, and the
                -- way we do the import depends on whether we're currently compiling
                -- the base package or not.
                       ++ (if platformOS platform == OSMinGW32 &&
                              thisPackage dflags == basePackageId
                                then [ "-DCOMPILING_BASE_PACKAGE" ]
                                else [])

        -- We only support SparcV9 and better because V8 lacks an atomic CAS
        -- instruction. Note that the user can still override this
        -- (e.g., -mcpu=ultrasparc) as GCC picks the "best" -mcpu flag
        -- regardless of the ordering.
        --
        -- This is a temporary hack. See #2872, commit
        -- 5bd3072ac30216a505151601884ac88bf404c9f2
                       ++ (if platformArch platform == ArchSPARC
                           then ["-mcpu=v9"]
                           else [])

                       -- GCC 4.6+ doesn't like -Wimplicit when compiling C++.
                       ++ (if (cc_phase /= Ccpp && cc_phase /= Cobjcpp)
                             then ["-Wimplicit"]
                             else [])

                       ++ (if hcc
                             then gcc_extra_viac_flags ++ more_hcc_opts
                             else [])
                       ++ verbFlags
                       ++ [ "-S", cc_opt ]
                       ++ [ "-D__GLASGOW_HASKELL__="++cProjectVersionInt ]
                       ++ framework_paths
                       ++ cc_opts
                       ++ split_opt
                       ++ include_paths
                       ++ pkg_extra_cc_opts
                       ))

        return (next_phase, output_fn)

-----------------------------------------------------------------------------
-- Splitting phase

runPhase Splitter input_fn dflags
  = do  -- tmp_pfx is the prefix used for the split .s files

        split_s_prefix <- liftIO $ SysTools.newTempName dflags "split"
        let n_files_fn = split_s_prefix

        liftIO $ SysTools.runSplit dflags
                          [ SysTools.FileOption "" input_fn
                          , SysTools.FileOption "" split_s_prefix
                          , SysTools.FileOption "" n_files_fn
                          ]

        -- Save the number of split files for future references
        s <- liftIO $ readFile n_files_fn
        let n_files = read s :: Int
            dflags' = dflags { splitInfo = Just (split_s_prefix, n_files) }

        setDynFlags dflags'

        -- Remember to delete all these files
        liftIO $ addFilesToClean dflags'
                                 [ split_s_prefix ++ "__" ++ show n ++ ".s"
                                 | n <- [1..n_files]]

        return (SplitAs,
                "**splitter**") -- we don't use the filename in SplitAs

-----------------------------------------------------------------------------
-- As, SpitAs phase : Assembler

-- This is for calling the assembler on a regular assembly file (not split).
runPhase As input_fn dflags
  = do
        -- LLVM from version 3.0 onwards doesn't support the OS X system
        -- assembler, so we use clang as the assembler instead. (#5636)
        let whichAsProg | hscTarget dflags == HscLlvm &&
                          platformOS (targetPlatform dflags) == OSDarwin
                        = do
                            -- be careful what options we call clang with
                            -- see #5903 and #7617 for bugs caused by this.
                            llvmVer <- liftIO $ figureLlvmVersion dflags
                            return $ case llvmVer of
                                Just n | n >= 30 -> SysTools.runClang
                                _                -> SysTools.runAs

                        | otherwise = return SysTools.runAs

        as_prog <- whichAsProg
        let as_opts = getOpts dflags opt_a
            cmdline_include_paths = includePaths dflags

        next_phase <- maybeMergeStub
        output_fn <- phaseOutputFilename next_phase

        -- we create directories for the object file, because it
        -- might be a hierarchical module.
        liftIO $ createDirectoryIfMissing True (takeDirectory output_fn)

        let runAssembler inputFilename outputFilename
                = liftIO $ as_prog dflags
                       (map SysTools.Option as_opts
                       ++ [ SysTools.Option ("-I" ++ p) | p <- cmdline_include_paths ]

        -- We only support SparcV9 and better because V8 lacks an atomic CAS
        -- instruction so we have to make sure that the assembler accepts the
        -- instruction set. Note that the user can still override this
        -- (e.g., -mcpu=ultrasparc). GCC picks the "best" -mcpu flag
        -- regardless of the ordering.
        --
        -- This is a temporary hack.
                       ++ (if platformArch (targetPlatform dflags) == ArchSPARC
                           then [SysTools.Option "-mcpu=v9"]
                           else [])

                       ++ [ SysTools.Option "-x", SysTools.Option "assembler-with-cpp"
                          , SysTools.Option "-c"
                          , SysTools.FileOption "" inputFilename
                          , SysTools.Option "-o"
                          , SysTools.FileOption "" outputFilename
                          ])

        runAssembler input_fn output_fn
        whenGeneratingDynamicToo dflags $
            runAssembler (input_fn ++ "-dyn")
                         (replaceExtension output_fn (dynObjectSuf dflags))

        return (next_phase, output_fn)


-- This is for calling the assembler on a split assembly file (so a collection
-- of assembly files)
runPhase SplitAs _input_fn dflags
  = do
        -- we'll handle the stub_o file in this phase, so don't MergeStub,
        -- just jump straight to StopLn afterwards.
        let next_phase = StopLn
        output_fn <- phaseOutputFilename next_phase

        let base_o = dropExtension output_fn
            osuf = objectSuf dflags
            split_odir  = base_o ++ "_" ++ osuf ++ "_split"

        liftIO $ createDirectoryIfMissing True split_odir

        -- remove M_split/ *.o, because we're going to archive M_split/ *.o
        -- later and we don't want to pick up any old objects.
        fs <- liftIO $ getDirectoryContents split_odir
        liftIO $ mapM_ removeFile $
                map (split_odir </>) $ filter (osuf `isSuffixOf`) fs

        let as_opts = getOpts dflags opt_a

        let (split_s_prefix, n) = case splitInfo dflags of
                                  Nothing -> panic "No split info"
                                  Just x -> x

        let split_s   n = split_s_prefix ++ "__" ++ show n <.> "s"

            split_obj :: Int -> FilePath
            split_obj n = split_odir </>
                          takeFileName base_o ++ "__" ++ show n <.> osuf

        let assemble_file n
              = SysTools.runAs dflags
                         (map SysTools.Option as_opts ++

        -- We only support SparcV9 and better because V8 lacks an atomic CAS
        -- instruction so we have to make sure that the assembler accepts the
        -- instruction set. Note that the user can still override this
        -- (e.g., -mcpu=ultrasparc). GCC picks the "best" -mcpu flag
        -- regardless of the ordering.
        --
        -- This is a temporary hack.
                          (if platformArch (targetPlatform dflags) == ArchSPARC
                           then [SysTools.Option "-mcpu=v9"]
                           else []) ++

                          [ SysTools.Option "-c"
                          , SysTools.Option "-o"
                          , SysTools.FileOption "" (split_obj n)
                          , SysTools.FileOption "" (split_s n)
                          ])

        liftIO $ mapM_ assemble_file [1..n]

        -- Note [pipeline-split-init]
        -- If we have a stub file, it may contain constructor
        -- functions for initialisation of this module.  We can't
        -- simply leave the stub as a separate object file, because it
        -- will never be linked in: nothing refers to it.  We need to
        -- ensure that if we ever refer to the data in this module
        -- that needs initialisation, then we also pull in the
        -- initialisation routine.
        --
        -- To that end, we make a DANGEROUS ASSUMPTION here: the data
        -- that needs to be initialised is all in the FIRST split
        -- object.  See Note [codegen-split-init].

        PipeState{maybe_stub_o} <- getPipeState
        case maybe_stub_o of
            Nothing     -> return ()
            Just stub_o -> liftIO $ do
                     tmp_split_1 <- newTempName dflags osuf
                     let split_1 = split_obj 1
                     copyFile split_1 tmp_split_1
                     removeFile split_1
                     joinObjectFiles dflags [tmp_split_1, stub_o] split_1

        -- join them into a single .o file
        liftIO $ joinObjectFiles dflags (map split_obj [1..n]) output_fn

        return (next_phase, output_fn)

-----------------------------------------------------------------------------
-- LlvmOpt phase

runPhase LlvmOpt input_fn dflags
  = do
    ver <- liftIO $ readIORef (llvmVersion dflags)

    let lo_opts  = getOpts dflags opt_lo
        opt_lvl  = max 0 (min 2 $ optLevel dflags)
        -- don't specify anything if user has specified commands. We do this
        -- for opt but not llc since opt is very specifically for optimisation
        -- passes only, so if the user is passing us extra options we assume
        -- they know what they are doing and don't get in the way.
        optFlag  = if null lo_opts
                       then [SysTools.Option (llvmOpts !! opt_lvl)]
                       else []
        tbaa | ver < 29                 = "" -- no tbaa in 2.8 and earlier
             | gopt Opt_LlvmTBAA dflags = "--enable-tbaa=true"
             | otherwise                = "--enable-tbaa=false"


    output_fn <- phaseOutputFilename LlvmLlc

    liftIO $ SysTools.runLlvmOpt dflags
               ([ SysTools.FileOption "" input_fn,
                    SysTools.Option "-o",
                    SysTools.FileOption "" output_fn]
                ++ optFlag
                ++ [SysTools.Option tbaa]
                ++ map SysTools.Option lo_opts)

    return (LlvmLlc, output_fn)
  where 
        -- we always (unless -optlo specified) run Opt since we rely on it to
        -- fix up some pretty big deficiencies in the code we generate
        llvmOpts = ["-mem2reg", "-O1", "-O2"]

-----------------------------------------------------------------------------
-- LlvmLlc phase

runPhase LlvmLlc input_fn dflags
  = do
    ver <- liftIO $ readIORef (llvmVersion dflags)

    let lc_opts = getOpts dflags opt_lc
        opt_lvl = max 0 (min 2 $ optLevel dflags)
        rmodel | gopt Opt_PIC dflags          = "pic"
               | not (gopt Opt_Static dflags) = "dynamic-no-pic"
               | otherwise                    = "static"
        tbaa | ver < 29                 = "" -- no tbaa in 2.8 and earlier
             | gopt Opt_LlvmTBAA dflags = "--enable-tbaa=true"
             | otherwise                = "--enable-tbaa=false"

    -- hidden debugging flag '-dno-llvm-mangler' to skip mangling
    let next_phase = case gopt Opt_NoLlvmMangler dflags of
                         False                            -> LlvmMangle
                         True | gopt Opt_SplitObjs dflags -> Splitter
                         True                             -> As
                        
    output_fn <- phaseOutputFilename next_phase

    liftIO $ SysTools.runLlvmLlc dflags
                ([ SysTools.Option (llvmOpts !! opt_lvl),
                    SysTools.Option $ "-relocation-model=" ++ rmodel,
                    SysTools.FileOption "" input_fn,
                    SysTools.Option "-o", SysTools.FileOption "" output_fn]
                ++ map SysTools.Option lc_opts
                ++ [SysTools.Option tbaa]
                ++ map SysTools.Option fpOpts
                ++ map SysTools.Option abiOpts
                ++ map SysTools.Option sseOpts)

    return (next_phase, output_fn)
  where
        -- Bug in LLVM at O3 on OSX.
        llvmOpts = if platformOS (targetPlatform dflags) == OSDarwin
                   then ["-O1", "-O2", "-O2"]
                   else ["-O1", "-O2", "-O3"]
        -- On ARMv7 using LLVM, LLVM fails to allocate floating point registers
        -- while compiling GHC source code. It's probably due to fact that it
        -- does not enable VFP by default. Let's do this manually here
        fpOpts = case platformArch (targetPlatform dflags) of 
                   ArchARM ARMv7 ext _ -> if (elem VFPv3 ext)
                                      then ["-mattr=+v7,+vfp3"]
                                      else if (elem VFPv3D16 ext)
                                           then ["-mattr=+v7,+vfp3,+d16"]
                                           else []
                   _                 -> []
        -- On Ubuntu/Debian with ARM hard float ABI, LLVM's llc still
        -- compiles into soft-float ABI. We need to explicitly set abi
        -- to hard
        abiOpts = case platformArch (targetPlatform dflags) of
                    ArchARM ARMv7 _ HARD -> ["-float-abi=hard"]
                    ArchARM ARMv7 _ _    -> []
                    _                    -> []

        sseOpts | isSse4_2Enabled dflags = ["-mattr=+sse42"]
                | isSse2Enabled dflags   = ["-mattr=+sse2"]
                | otherwise              = []

-----------------------------------------------------------------------------
-- LlvmMangle phase

runPhase LlvmMangle input_fn dflags
  = do
      let next_phase = if gopt Opt_SplitObjs dflags then Splitter else As
      output_fn <- phaseOutputFilename next_phase
      liftIO $ llvmFixupAsm dflags input_fn output_fn
      return (next_phase, output_fn)

-----------------------------------------------------------------------------
-- merge in stub objects

runPhase MergeStub input_fn dflags
 = do
     PipeState{maybe_stub_o} <- getPipeState
     output_fn <- phaseOutputFilename StopLn
     case maybe_stub_o of
       Nothing ->
         panic "runPhase(MergeStub): no stub"
       Just stub_o -> do
         liftIO $ joinObjectFiles dflags [input_fn, stub_o] output_fn
         whenGeneratingDynamicToo dflags $ do
           liftIO $ debugTraceMsg dflags 4
                        (text "Merging stub again for -dynamic-too")
           let dyn_input_fn  = replaceExtension input_fn  (dynObjectSuf dflags)
               dyn_output_fn = replaceExtension output_fn (dynObjectSuf dflags)
           liftIO $ joinObjectFiles dflags [dyn_input_fn, stub_o] dyn_output_fn
         return (StopLn, output_fn)

-- warning suppression
runPhase other _input_fn _dflags =
   panic ("runPhase: don't know how to run phase " ++ show other)

maybeMergeStub :: CompPipeline Phase
maybeMergeStub
 = do
     PipeState{maybe_stub_o} <- getPipeState
     if isJust maybe_stub_o then return MergeStub else return StopLn

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

runPhase_MoveBinary :: DynFlags -> FilePath -> IO Bool
runPhase_MoveBinary dflags input_fn
    | WayPar `elem` ways dflags && not (gopt Opt_Static dflags) =
        panic ("Don't know how to combine PVM wrapper and dynamic wrapper")
    | WayPar `elem` ways dflags = do
        let sysMan = pgm_sysman dflags
        pvm_root <- getEnv "PVM_ROOT"
        pvm_arch <- getEnv "PVM_ARCH"
        let
           pvm_executable_base = "=" ++ input_fn
           pvm_executable = pvm_root ++ "/bin/" ++ pvm_arch ++ "/" ++ pvm_executable_base
        -- nuke old binary; maybe use configur'ed names for cp and rm?
        _ <- tryIO (removeFile pvm_executable)
        -- move the newly created binary into PVM land
        copy dflags "copying PVM executable" input_fn pvm_executable
        -- generate a wrapper script for running a parallel prg under PVM
        writeFile input_fn (mk_pvm_wrapper_script pvm_executable pvm_executable_base sysMan)
        return True
    | otherwise = return True

mkExtraObj :: DynFlags -> Suffix -> String -> IO FilePath
mkExtraObj dflags extn xs
 = do cFile <- newTempName dflags extn
      oFile <- newTempName dflags "o"
      writeFile cFile xs
      let rtsDetails = getPackageDetails (pkgState dflags) rtsPackageId
      SysTools.runCc dflags
                     ([Option        "-c",
                       FileOption "" cFile,
                       Option        "-o",
                       FileOption "" oFile]
                      ++ map SysTools.Option (getOpts dflags opt_c) -- see #5528
                      ++ map (FileOption "-I") (includeDirs rtsDetails))
      return oFile

-- When linking a binary, we need to create a C main() function that
-- starts everything off.  This used to be compiled statically as part
-- of the RTS, but that made it hard to change the -rtsopts setting,
-- so now we generate and compile a main() stub as part of every
-- binary and pass the -rtsopts setting directly to the RTS (#5373)
--
mkExtraObjToLinkIntoBinary :: DynFlags -> IO FilePath
mkExtraObjToLinkIntoBinary dflags = do
   when (gopt Opt_NoHsMain dflags && haveRtsOptsFlags dflags) $ do
      log_action dflags dflags SevInfo noSrcSpan defaultUserStyle
          (text "Warning: -rtsopts and -with-rtsopts have no effect with -no-hs-main." $$
           text "    Call hs_init_ghc() from your main() function to set these options.")

   mkExtraObj dflags "c" (showSDoc dflags main)

  where
    main
      | gopt Opt_NoHsMain dflags = empty
      | otherwise = vcat [
             ptext (sLit "#include \"Rts.h\""),
             ptext (sLit "extern StgClosure ZCMain_main_closure;"),
             ptext (sLit "int main(int argc, char *argv[])"),
             char '{',
             ptext (sLit "    RtsConfig __conf = defaultRtsConfig;"),
             ptext (sLit "    __conf.rts_opts_enabled = ")
                 <> text (show (rtsOptsEnabled dflags)) <> semi,
             case rtsOpts dflags of
                Nothing   -> empty
                Just opts -> ptext (sLit "    __conf.rts_opts= ") <>
                               text (show opts) <> semi,
             ptext (sLit "    return hs_main(argc, argv, &ZCMain_main_closure,__conf);"),
             char '}',
             char '\n' -- final newline, to keep gcc happy
           ]

-- Write out the link info section into a new assembly file. Previously
-- this was included as inline assembly in the main.c file but this
-- is pretty fragile. gas gets upset trying to calculate relative offsets
-- that span the .note section (notably .text) when debug info is present
mkNoteObjsToLinkIntoBinary :: DynFlags -> [PackageId] -> IO [FilePath]
mkNoteObjsToLinkIntoBinary dflags dep_packages = do
   link_info <- getLinkInfo dflags dep_packages

   if (platformSupportsSavingLinkOpts (platformOS (targetPlatform dflags)))
     then fmap (:[]) $ mkExtraObj dflags "s" (showSDoc dflags (link_opts link_info))
     else return []

  where
    link_opts info = hcat [
          text "\t.section ", text ghcLinkInfoSectionName,
                                   text ",\"\",",
                                   text elfSectionNote,
                                   text "\n",

          text "\t.ascii \"", info', text "\"\n" ]
          where
            info' = text $ escape info

            escape :: String -> String
            escape = concatMap (charToC.fromIntegral.ord)

            elfSectionNote :: String
            elfSectionNote = case platformArch (targetPlatform dflags) of
                               ArchARM _ _ _ -> "%note"
                               _             -> "@note"

-- The "link info" is a string representing the parameters of the
-- link.  We save this information in the binary, and the next time we
-- link, if nothing else has changed, we use the link info stored in
-- the existing binary to decide whether to re-link or not.
getLinkInfo :: DynFlags -> [PackageId] -> IO String
getLinkInfo dflags dep_packages = do
   package_link_opts <- getPackageLinkOpts dflags dep_packages
   pkg_frameworks <- case platformOS (targetPlatform dflags) of
                     OSDarwin -> getPackageFrameworks dflags dep_packages
                     _        -> return []
   let extra_ld_inputs = ldInputs dflags
   let
      link_info = (package_link_opts,
                   pkg_frameworks,
                   rtsOpts dflags,
                   rtsOptsEnabled dflags,
                   gopt Opt_NoHsMain dflags,
                   extra_ld_inputs,
                   getOpts dflags opt_l)
   --
   return (show link_info)

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
    let platform = targetPlatform dflags
        mySettings = settings dflags
        verbFlags = getVerbFlags dflags
        output_fn = exeFileName dflags

    -- get the full list of packages to link with, by combining the
    -- explicit packages with the auto packages and all of their
    -- dependencies, and eliminating duplicates.

    full_output_fn <- if isAbsolute output_fn
                      then return output_fn
                      else do d <- getCurrentDirectory
                              return $ normalise (d </> output_fn)
    pkg_lib_paths <- getPackageLibraryPath dflags dep_packages
    let pkg_lib_path_opts = concatMap get_pkg_lib_path_opts pkg_lib_paths
        get_pkg_lib_path_opts l
         | osElfTarget (platformOS platform) &&
           dynLibLoader dflags == SystemDependent &&
           not (gopt Opt_Static dflags)
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "$ORIGIN" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
                  rpath = if gopt Opt_RPath dflags
                          then ["-Wl,-rpath",      "-Wl," ++ libpath]
                          else []
                  -- Solaris 11's linker does not support -rpath-link option. It silently
                  -- ignores it and then complains about next option which is -l<some
                  -- dir> as being a directory and not expected object file, E.g
                  -- ld: elf error: file
                  -- /tmp/ghc-src/libraries/base/dist-install/build:
                  -- elf_begin: I/O error: region read: Is a directory
                  rpathlink = if (platformOS platform) == OSSolaris2
                              then []
                              else ["-Wl,-rpath-link", "-Wl," ++ l]
              in ["-L" ++ l] ++ rpathlink ++ rpath
         | otherwise = ["-L" ++ l]

    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    extraLinkObj <- mkExtraObjToLinkIntoBinary dflags
    noteLinkObjs <- mkNoteObjsToLinkIntoBinary dflags dep_packages

    pkg_link_opts <- getPackageLinkOpts dflags dep_packages

    pkg_framework_path_opts <-
        case platformOS platform of
        OSDarwin ->
            do pkg_framework_paths <- getPackageFrameworkPath dflags dep_packages
               return $ map ("-F" ++) pkg_framework_paths
        _ ->
            return []

    framework_path_opts <-
        case platformOS platform of
        OSDarwin ->
            do let framework_paths = frameworkPaths dflags
               return $ map ("-F" ++) framework_paths
        _ ->
            return []

    pkg_framework_opts <-
        case platformOS platform of
        OSDarwin ->
            do pkg_frameworks <- getPackageFrameworks dflags dep_packages
               return $ concat [ ["-framework", fw] | fw <- pkg_frameworks ]
        _ ->
            return []

    framework_opts <-
        case platformOS platform of
        OSDarwin ->
            do let frameworks = cmdlineFrameworks dflags
               -- reverse because they're added in reverse order from
               -- the cmd line:
               return $ concat [ ["-framework", fw] | fw <- reverse frameworks ]
        _ ->
            return []

        -- probably _stub.o files
    let extra_ld_inputs = ldInputs dflags

        -- opts from -optl-<blah> (including -l<blah> options)
    let extra_ld_opts = getOpts dflags opt_l

    -- Here are some libs that need to be linked at the *end* of
    -- the command line, because they contain symbols that are referred to
    -- by the RTS.  We can't therefore use the ordinary way opts for these.
    let
        debug_opts | WayDebug `elem` ways dflags = [
#if defined(HAVE_LIBBFD)
                        "-lbfd", "-liberty"
#endif
                         ]
                   | otherwise            = []

    let thread_opts
         | WayThreaded `elem` ways dflags =
            let os = platformOS (targetPlatform dflags)
            in if os == OSOsf3 then ["-lpthread", "-lexc"]
               else if os `elem` [OSMinGW32, OSFreeBSD, OSOpenBSD,
                                  OSNetBSD, OSHaiku, OSQNXNTO]
               then []
               else ["-lpthread"]
         | otherwise               = []

    rc_objs <- maybeCreateManifest dflags output_fn

    SysTools.runLink dflags (
                       map SysTools.Option verbFlags
                      ++ [ SysTools.Option "-o"
                         , SysTools.FileOption "" output_fn
                         ]
                      ++ map SysTools.Option (
                         []

                      -- Permit the linker to auto link _symbol to _imp_symbol.
                      -- This lets us link against DLLs without needing an "import library".
                      ++ (if platformOS platform == OSMinGW32
                          then ["-Wl,--enable-auto-import"]
                          else [])

                      -- '-no_compact_unwind'
                      -- C++/Objective-C exceptions cannot use optimised
                      -- stack unwinding code. The optimised form is the
                      -- default in Xcode 4 on at least x86_64, and
                      -- without this flag we're also seeing warnings
                      -- like
                      --     ld: warning: could not create compact unwind for .LFB3: non-standard register 5 being saved in prolog
                      -- on x86.
                      ++ (if sLdSupportsCompactUnwind mySettings &&
                             platformOS   platform == OSDarwin &&
                             platformArch platform `elem` [ArchX86, ArchX86_64]
                          then ["-Wl,-no_compact_unwind"]
                          else [])

                      -- '-Wl,-read_only_relocs,suppress'
                      -- ld gives loads of warnings like:
                      --     ld: warning: text reloc in _base_GHCziArr_unsafeArray_info to _base_GHCziArr_unsafeArray_closure
                      -- when linking any program. We're not sure
                      -- whether this is something we ought to fix, but
                      -- for now this flags silences them.
                      ++ (if platformOS   platform == OSDarwin &&
                             platformArch platform == ArchX86
                          then ["-Wl,-read_only_relocs,suppress"]
                          else [])

                      ++ o_files
                      ++ extra_ld_inputs
                      ++ lib_path_opts
                      ++ extra_ld_opts
                      ++ rc_objs
                      ++ framework_path_opts
                      ++ framework_opts
                      ++ pkg_lib_path_opts
                      ++ extraLinkObj:noteLinkObjs
                      ++ pkg_link_opts
                      ++ pkg_framework_path_opts
                      ++ pkg_framework_opts
                      ++ debug_opts
                      ++ thread_opts
                    ))

    -- parallel only: move binary to another dir -- HWL
    success <- runPhase_MoveBinary dflags output_fn
    if success then return ()
               else throwGhcException (InstallationError ("cannot move binary"))


exeFileName :: DynFlags -> FilePath
exeFileName dflags
  | Just s <- outputFile dflags =
      if platformOS (targetPlatform dflags) == OSMinGW32
      then if null (takeExtension s)
           then s <.> "exe"
           else s
      else s
  | otherwise =
      if platformOS (targetPlatform dflags) == OSMinGW32
      then "main.exe"
      else "a.out"

maybeCreateManifest
   :: DynFlags
   -> FilePath                          -- filename of executable
   -> IO [FilePath]                     -- extra objects to embed, maybe
maybeCreateManifest dflags exe_filename
 | platformOS (targetPlatform dflags) == OSMinGW32 &&
   gopt Opt_GenManifest dflags
    = do let manifest_filename = exe_filename <.> "manifest"

         writeFile manifest_filename $
             "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"++
             "  <assembly xmlns=\"urn:schemas-microsoft-com:asm.v1\" manifestVersion=\"1.0\">\n"++
             "  <assemblyIdentity version=\"1.0.0.0\"\n"++
             "     processorArchitecture=\"X86\"\n"++
             "     name=\"" ++ dropExtension exe_filename ++ "\"\n"++
             "     type=\"win32\"/>\n\n"++
             "  <trustInfo xmlns=\"urn:schemas-microsoft-com:asm.v3\">\n"++
             "    <security>\n"++
             "      <requestedPrivileges>\n"++
             "        <requestedExecutionLevel level=\"asInvoker\" uiAccess=\"false\"/>\n"++
             "        </requestedPrivileges>\n"++
             "       </security>\n"++
             "  </trustInfo>\n"++
             "</assembly>\n"

         -- Windows will find the manifest file if it is named
         -- foo.exe.manifest. However, for extra robustness, and so that
         -- we can move the binary around, we can embed the manifest in
         -- the binary itself using windres:
         if not (gopt Opt_EmbedManifest dflags) then return [] else do

         rc_filename <- newTempName dflags "rc"
         rc_obj_filename <- newTempName dflags (objectSuf dflags)

         writeFile rc_filename $
             "1 24 MOVEABLE PURE " ++ show manifest_filename ++ "\n"
               -- magic numbers :-)
               -- show is a bit hackish above, but we need to escape the
               -- backslashes in the path.

         let wr_opts = getOpts dflags opt_windres
         runWindres dflags $ map SysTools.Option $
               ["--input="++rc_filename,
                "--output="++rc_obj_filename,
                "--output-format=coff"]
               ++ wr_opts
               -- no FileOptions here: windres doesn't like seeing
               -- backslashes, apparently

         removeFile manifest_filename

         return [rc_obj_filename]
 | otherwise = return []


linkDynLibCheck :: DynFlags -> [String] -> [PackageId] -> IO ()
linkDynLibCheck dflags o_files dep_packages
 = do
    when (haveRtsOptsFlags dflags) $ do
      log_action dflags dflags SevInfo noSrcSpan defaultUserStyle
          (text "Warning: -rtsopts and -with-rtsopts have no effect with -shared." $$
           text "    Call hs_init_ghc() from your main() function to set these options.")

    linkDynLib dflags o_files dep_packages

-- -----------------------------------------------------------------------------
-- Running CPP

doCpp :: DynFlags -> Bool -> Bool -> FilePath -> FilePath -> IO ()
doCpp dflags raw include_cc_opts input_fn output_fn = do
    let hscpp_opts = getOpts dflags opt_P ++ picPOpts dflags
    let cmdline_include_paths = includePaths dflags

    pkg_include_dirs <- getPackageIncludePath dflags []
    let include_paths = foldr (\ x xs -> "-I" : x : xs) []
                          (cmdline_include_paths ++ pkg_include_dirs)

    let verbFlags = getVerbFlags dflags

    let cc_opts
          | include_cc_opts = getOpts dflags opt_c
          | otherwise       = []

    let cpp_prog args | raw       = SysTools.runCpp dflags args
                      | otherwise = SysTools.runCc dflags (SysTools.Option "-E" : args)

    let target_defs =
          [ "-D" ++ HOST_OS     ++ "_BUILD_OS=1",
            "-D" ++ HOST_ARCH   ++ "_BUILD_ARCH=1",
            "-D" ++ TARGET_OS   ++ "_HOST_OS=1",
            "-D" ++ TARGET_ARCH ++ "_HOST_ARCH=1" ]
        -- remember, in code we *compile*, the HOST is the same our TARGET,
        -- and BUILD is the same as our HOST.

    let sse2 = isSse2Enabled dflags
        sse4_2 = isSse4_2Enabled dflags
        sse_defs =
          [ "-D__SSE__=1" | sse2 || sse4_2 ] ++
          [ "-D__SSE2__=1" | sse2 || sse4_2 ] ++
          [ "-D__SSE4_2__=1" | sse4_2 ]

    cpp_prog       (   map SysTools.Option verbFlags
                    ++ map SysTools.Option include_paths
                    ++ map SysTools.Option hsSourceCppOpts
                    ++ map SysTools.Option target_defs
                    ++ map SysTools.Option hscpp_opts
                    ++ map SysTools.Option cc_opts
                    ++ map SysTools.Option sse_defs
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

hsSourceCppOpts :: [String]
-- Default CPP defines in Haskell source
hsSourceCppOpts =
        [ "-D__GLASGOW_HASKELL__="++cProjectVersionInt ]

-- ---------------------------------------------------------------------------
-- join object files into a single relocatable object file, using ld -r

joinObjectFiles :: DynFlags -> [FilePath] -> FilePath -> IO ()
joinObjectFiles dflags o_files output_fn = do
  let mySettings = settings dflags
      ldIsGnuLd = sLdIsGnuLd mySettings
      ld_r args = SysTools.runLink dflags ([
                            SysTools.Option "-nostdlib",
                            SysTools.Option "-nodefaultlibs",
                            SysTools.Option "-Wl,-r"
                            ]
                            -- gcc on sparc sets -Wl,--relax implicitly, but
                            -- -r and --relax are incompatible for ld, so
                            -- disable --relax explicitly.
                         ++ (if platformArch (targetPlatform dflags) == ArchSPARC
                             && ldIsGnuLd
                                then [SysTools.Option "-Wl,-no-relax"]
                                else [])
                         ++ map SysTools.Option ld_build_id
                         ++ [ SysTools.Option "-o",
                              SysTools.FileOption "" output_fn ]
                         ++ args)

      -- suppress the generation of the .note.gnu.build-id section,
      -- which we don't need and sometimes causes ld to emit a
      -- warning:
      ld_build_id | sLdSupportsBuildId mySettings = ["-Wl,--build-id=none"]
                  | otherwise                     = []

  if ldIsGnuLd
     then do
          script <- newTempName dflags "ldscript"
          writeFile script $ "INPUT(" ++ unwords o_files ++ ")"
          ld_r [SysTools.FileOption "" script]
     else do
          ld_r (map (SysTools.FileOption "") o_files)

-- -----------------------------------------------------------------------------
-- Misc.

-- | What phase to run after one of the backend code generators has run
hscPostBackendPhase :: DynFlags -> HscSource -> HscTarget -> Phase
hscPostBackendPhase _ HsBootFile _    =  StopLn
hscPostBackendPhase dflags _ hsc_lang =
  case hsc_lang of
        HscC -> HCc
        HscAsm | gopt Opt_SplitObjs dflags -> Splitter
               | otherwise                 -> As
        HscLlvm        -> LlvmOpt
        HscNothing     -> StopLn
        HscInterpreted -> StopLn

touchObjectFile :: DynFlags -> FilePath -> IO ()
touchObjectFile dflags path = do
  createDirectoryIfMissing True $ takeDirectory path
  SysTools.touch dflags "Touching object file" path

haveRtsOptsFlags :: DynFlags -> Bool
haveRtsOptsFlags dflags =
         isJust (rtsOpts dflags) || case rtsOptsEnabled dflags of
                                        RtsOptsSafeOnly -> False
                                        _ -> True
