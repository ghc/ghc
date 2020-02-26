{-# LANGUAGE CPP, NamedFieldPuns, NondecreasingIndentation, BangPatterns, MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------
--
-- GHC Driver
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module GHC.Driver.Pipeline (
        -- Run a series of compilation steps in a pipeline, for a
        -- collection of source files.
   oneShot, compileFile,

        -- Interfaces for the batch-mode driver
   linkBinary,

        -- Interfaces for the compilation manager (interpreted/batch-mode)
   preprocess,
   compileOne, compileOne',
   link,

        -- Exports for hooks to override runPhase and link
   PhasePlus(..), CompPipeline(..), PipeEnv(..), PipeState(..),
   phaseOutputFilename, getOutputFilename, getPipeState, getPipeEnv,
   hscPostBackendPhase, getLocation, setModLocation, setDynFlags,
   runPhase, exeFileName,
   maybeCreateManifest,
   doCpp,
   linkingNeeded, checkLinkInfo, writeInterfaceOnlyMode
  ) where

#include <ghcplatform.h>
#include "HsVersions.h"

import GhcPrelude

import GHC.Driver.Pipeline.Monad
import GHC.Driver.Packages
import GHC.Driver.Ways
import HeaderInfo
import GHC.Driver.Phases
import SysTools
import SysTools.ExtraObj
import GHC.Driver.Main
import GHC.Driver.Finder
import GHC.Driver.Types hiding ( Hsc )
import Outputable
import GHC.Types.Module
import ErrUtils
import GHC.Driver.Session
import Panic
import Util
import StringBuffer     ( hGetStringBuffer, hPutStringBuffer )
import GHC.Types.Basic  ( SuccessFlag(..) )
import Maybes           ( expectJust )
import GHC.Types.SrcLoc
import GHC.CmmToLlvm    ( llvmFixupAsm, llvmVersionList )
import MonadUtils
import GHC.Platform
import TcRnTypes
import ToolSettings
import GHC.Driver.Hooks
import qualified GHC.LanguageExtensions as LangExt
import FileCleanup
import Ar
import Bag              ( unitBag )
import FastString       ( mkFastString )
import GHC.Iface.Make   ( mkFullIface )
import UpdateIdInfos    ( updateModDetailsIdInfos )

import Exception
import System.Directory
import System.FilePath
import System.IO
import Control.Monad
import Data.List        ( isInfixOf, intercalate )
import Data.Maybe
import Data.Version
import Data.Either      ( partitionEithers )

import Data.Time        ( UTCTime )

-- ---------------------------------------------------------------------------
-- Pre-process

-- | Just preprocess a file, put the result in a temp. file (used by the
-- compilation manager during the summary phase).
--
-- We return the augmented DynFlags, because they contain the result
-- of slurping in the OPTIONS pragmas

preprocess :: HscEnv
           -> FilePath -- ^ input filename
           -> Maybe InputFileBuffer
           -- ^ optional buffer to use instead of reading the input file
           -> Maybe Phase -- ^ starting phase
           -> IO (Either ErrorMessages (DynFlags, FilePath))
preprocess hsc_env input_fn mb_input_buf mb_phase =
  handleSourceError (\err -> return (Left (srcErrorMessages err))) $
  ghandle handler $
  fmap Right $ do
  MASSERT2(isJust mb_phase || isHaskellSrcFilename input_fn, text input_fn)
  (dflags, fp, mb_iface) <- runPipeline anyHsc hsc_env (input_fn, mb_input_buf, fmap RealPhase mb_phase)
        Nothing
        -- We keep the processed file for the whole session to save on
        -- duplicated work in ghci.
        (Temporary TFL_GhcSession)
        Nothing{-no ModLocation-}
        []{-no foreign objects-}
  -- We stop before Hsc phase so we shouldn't generate an interface
  MASSERT(isNothing mb_iface)
  return (dflags, fp)
  where
    srcspan = srcLocSpan $ mkSrcLoc (mkFastString input_fn) 1 1
    handler (ProgramError msg) = return $ Left $ unitBag $
        mkPlainErrMsg (hsc_dflags hsc_env) srcspan $ text msg
    handler ex = throwGhcExceptionIO ex

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

compileOne :: HscEnv
           -> ModSummary      -- ^ summary for module being compiled
           -> Int             -- ^ module N ...
           -> Int             -- ^ ... of M
           -> Maybe ModIface  -- ^ old interface, if we have one
           -> Maybe Linkable  -- ^ old linkable, if we have one
           -> SourceModified
           -> IO HomeModInfo   -- ^ the complete HomeModInfo, if successful

compileOne = compileOne' Nothing (Just batchMsg)

compileOne' :: Maybe TcGblEnv
            -> Maybe Messager
            -> HscEnv
            -> ModSummary      -- ^ summary for module being compiled
            -> Int             -- ^ module N ...
            -> Int             -- ^ ... of M
            -> Maybe ModIface  -- ^ old interface, if we have one
            -> Maybe Linkable  -- ^ old linkable, if we have one
            -> SourceModified
            -> IO HomeModInfo   -- ^ the complete HomeModInfo, if successful

compileOne' m_tc_result mHscMessage
            hsc_env0 summary mod_index nmods mb_old_iface mb_old_linkable
            source_modified0
 = do

   debugTraceMsg dflags1 2 (text "compile: input file" <+> text input_fnpp)

   -- Run the pipeline up to codeGen (so everything up to, but not including, STG)
   (status, plugin_dflags) <- hscIncrementalCompile
                        always_do_basic_recompilation_check
                        m_tc_result mHscMessage
                        hsc_env summary source_modified mb_old_iface (mod_index, nmods)

   let flags = hsc_dflags hsc_env0
     in do unless (gopt Opt_KeepHiFiles flags) $
               addFilesToClean flags TFL_CurrentModule $
                   [ml_hi_file $ ms_location summary]
           unless (gopt Opt_KeepOFiles flags) $
               addFilesToClean flags TFL_GhcSession $
                   [ml_obj_file $ ms_location summary]

   -- Use an HscEnv with DynFlags updated with the plugin info (returned from
   -- hscIncrementalCompile)
   let hsc_env' = hsc_env{ hsc_dflags = plugin_dflags }

   case (status, hsc_lang) of
        (HscUpToDate iface hmi_details, _) ->
            -- TODO recomp014 triggers this assert. What's going on?!
            -- ASSERT( isJust mb_old_linkable || isNoLink (ghcLink dflags) )
            return $! HomeModInfo iface hmi_details mb_old_linkable
        (HscNotGeneratingCode iface hmi_details, HscNothing) ->
            let mb_linkable = if isHsBootOrSig src_flavour
                                then Nothing
                                -- TODO: Questionable.
                                else Just (LM (ms_hs_date summary) this_mod [])
            in return $! HomeModInfo iface hmi_details mb_linkable
        (HscNotGeneratingCode _ _, _) -> panic "compileOne HscNotGeneratingCode"
        (_, HscNothing) -> panic "compileOne HscNothing"
        (HscUpdateBoot iface hmi_details, HscInterpreted) -> do
            return $! HomeModInfo iface hmi_details Nothing
        (HscUpdateBoot iface hmi_details, _) -> do
            touchObjectFile dflags object_filename
            return $! HomeModInfo iface hmi_details Nothing
        (HscUpdateSig iface hmi_details, HscInterpreted) -> do
            let !linkable = LM (ms_hs_date summary) this_mod []
            return $! HomeModInfo iface hmi_details (Just linkable)
        (HscUpdateSig iface hmi_details, _) -> do
            output_fn <- getOutputFilename next_phase
                            (Temporary TFL_CurrentModule) basename dflags
                            next_phase (Just location)

            -- #10660: Use the pipeline instead of calling
            -- compileEmptyStub directly, so -dynamic-too gets
            -- handled properly
            _ <- runPipeline StopLn hsc_env'
                              (output_fn,
                               Nothing,
                               Just (HscOut src_flavour
                                            mod_name (HscUpdateSig iface hmi_details)))
                              (Just basename)
                              Persistent
                              (Just location)
                              []
            o_time <- getModificationUTCTime object_filename
            let !linkable = LM o_time this_mod [DotO object_filename]
            return $! HomeModInfo iface hmi_details (Just linkable)
        (HscRecomp { hscs_guts = cgguts,
                     hscs_mod_location = mod_location,
                     hscs_mod_details = hmi_details,
                     hscs_partial_iface = partial_iface,
                     hscs_old_iface_hash = mb_old_iface_hash,
                     hscs_iface_dflags = iface_dflags }, HscInterpreted) -> do
            -- In interpreted mode the regular codeGen backend is not run so we
            -- generate a interface without codeGen info.
            final_iface <- mkFullIface hsc_env'{hsc_dflags=iface_dflags} partial_iface Nothing
            liftIO $ hscMaybeWriteIface dflags final_iface mb_old_iface_hash (ms_location summary)

            (hasStub, comp_bc, spt_entries) <- hscInteractive hsc_env' cgguts mod_location

            stub_o <- case hasStub of
                      Nothing -> return []
                      Just stub_c -> do
                          stub_o <- compileStub hsc_env' stub_c
                          return [DotO stub_o]

            let hs_unlinked = [BCOs comp_bc spt_entries]
                unlinked_time = ms_hs_date summary
              -- Why do we use the timestamp of the source file here,
              -- rather than the current time?  This works better in
              -- the case where the local clock is out of sync
              -- with the filesystem's clock.  It's just as accurate:
              -- if the source is modified, then the linkable will
              -- be out of date.
            let !linkable = LM unlinked_time (ms_mod summary)
                           (hs_unlinked ++ stub_o)
            return $! HomeModInfo final_iface hmi_details (Just linkable)
        (HscRecomp{}, _) -> do
            output_fn <- getOutputFilename next_phase
                            (Temporary TFL_CurrentModule)
                            basename dflags next_phase (Just location)
            -- We're in --make mode: finish the compilation pipeline.
            (_, _, Just (iface, details)) <- runPipeline StopLn hsc_env'
                              (output_fn,
                               Nothing,
                               Just (HscOut src_flavour mod_name status))
                              (Just basename)
                              Persistent
                              (Just location)
                              []
                  -- The object filename comes from the ModLocation
            o_time <- getModificationUTCTime object_filename
            let !linkable = LM o_time this_mod [DotO object_filename]
            return $! HomeModInfo iface details (Just linkable)

 where dflags0     = ms_hspp_opts summary
       this_mod    = ms_mod summary
       location    = ms_location summary
       input_fn    = expectJust "compile:hs" (ml_hs_file location)
       input_fnpp  = ms_hspp_file summary
       mod_graph   = hsc_mod_graph hsc_env0
       needsLinker = needsTemplateHaskellOrQQ mod_graph
       isDynWay    = any (== WayDyn) (ways dflags0)
       isProfWay   = any (== WayProf) (ways dflags0)
       internalInterpreter = not (gopt Opt_ExternalInterpreter dflags0)

       src_flavour = ms_hsc_src summary
       mod_name = ms_mod_name summary
       next_phase = hscPostBackendPhase src_flavour hsc_lang
       object_filename = ml_obj_file location

       -- #8180 - when using TemplateHaskell, switch on -dynamic-too so
       -- the linker can correctly load the object files.  This isn't necessary
       -- when using -fexternal-interpreter.
       dflags1 = if hostIsDynamic && internalInterpreter &&
                    not isDynWay && not isProfWay && needsLinker
                  then gopt_set dflags0 Opt_BuildDynamicToo
                  else dflags0

       -- #16331 - when no "internal interpreter" is available but we
       -- need to process some TemplateHaskell or QuasiQuotes, we automatically
       -- turn on -fexternal-interpreter.
       dflags2 = if not internalInterpreter && needsLinker
                 then gopt_set dflags1 Opt_ExternalInterpreter
                 else dflags1

       basename = dropExtension input_fn

       -- We add the directory in which the .hs files resides) to the import
       -- path.  This is needed when we try to compile the .hc file later, if it
       -- imports a _stub.h file that we created here.
       current_dir = takeDirectory basename
       old_paths   = includePaths dflags2
       !prevailing_dflags = hsc_dflags hsc_env0
       dflags =
          dflags2 { includePaths = addQuoteInclude old_paths [current_dir]
                  , log_action = log_action prevailing_dflags }
                  -- use the prevailing log_action / log_finaliser,
                  -- not the one cached in the summary.  This is so
                  -- that we can change the log_action without having
                  -- to re-summarize all the source files.
       hsc_env     = hsc_env0 {hsc_dflags = dflags}

       -- Figure out what lang we're generating
       hsc_lang = hscTarget dflags

       -- -fforce-recomp should also work with --make
       force_recomp = gopt Opt_ForceRecomp dflags
       source_modified
         | force_recomp = SourceModified
         | otherwise = source_modified0

       always_do_basic_recompilation_check = case hsc_lang of
                                             HscInterpreted -> True
                                             _ -> False

-----------------------------------------------------------------------------
-- stub .h and .c files (for foreign export support), and cc files.

-- The _stub.c file is derived from the haskell source file, possibly taking
-- into account the -stubdir option.
--
-- The object file created by compiling the _stub.c file is put into a
-- temporary file, which will be later combined with the main .o file
-- (see the MergeForeigns phase).
--
-- Moreover, we also let the user emit arbitrary C/C++/ObjC/ObjC++ files
-- from TH, that are then compiled and linked to the module. This is
-- useful to implement facilities such as inline-c.

compileForeign :: HscEnv -> ForeignSrcLang -> FilePath -> IO FilePath
compileForeign _ RawObject object_file = return object_file
compileForeign hsc_env lang stub_c = do
        let phase = case lang of
              LangC      -> Cc
              LangCxx    -> Ccxx
              LangObjc   -> Cobjc
              LangObjcxx -> Cobjcxx
              LangAsm    -> As True -- allow CPP
#if __GLASGOW_HASKELL__ < 811
              RawObject  -> panic "compileForeign: should be unreachable"
#endif
        (_, stub_o, _) <- runPipeline StopLn hsc_env
                       (stub_c, Nothing, Just (RealPhase phase))
                       Nothing (Temporary TFL_GhcSession)
                       Nothing{-no ModLocation-}
                       []
        return stub_o

compileStub :: HscEnv -> FilePath -> IO FilePath
compileStub hsc_env stub_c = compileForeign hsc_env LangC stub_c

compileEmptyStub :: DynFlags -> HscEnv -> FilePath -> ModLocation -> ModuleName -> IO ()
compileEmptyStub dflags hsc_env basename location mod_name = do
  -- To maintain the invariant that every Haskell file
  -- compiles to object code, we make an empty (but
  -- valid) stub object file for signatures.  However,
  -- we make sure this object file has a unique symbol,
  -- so that ranlib on OS X doesn't complain, see
  -- https://gitlab.haskell.org/ghc/ghc/issues/12673
  -- and https://github.com/haskell/cabal/issues/2257
  empty_stub <- newTempName dflags TFL_CurrentModule "c"
  let src = text "int" <+> ppr (mkModule (thisPackage dflags) mod_name) <+> text "= 0;"
  writeFile empty_stub (showSDoc dflags (pprCode CStyle src))
  _ <- runPipeline StopLn hsc_env
                  (empty_stub, Nothing, Nothing)
                  (Just basename)
                  Persistent
                  (Just location)
                  []
  return ()

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

link ghcLink dflags
  = lookupHook linkHook l dflags ghcLink dflags
  where
    l LinkInMemory _ _ _
      = if platformMisc_ghcWithInterpreter $ platformMisc dflags
        then -- Not Linking...(demand linker will do the job)
             return Succeeded
        else panicBadLink LinkInMemory

    l NoLink _ _ _
      = return Succeeded

    l LinkBinary dflags batch_attempt_linking hpt
      = link' dflags batch_attempt_linking hpt

    l LinkStaticLib dflags batch_attempt_linking hpt
      = link' dflags batch_attempt_linking hpt

    l LinkDynLib dflags batch_attempt_linking hpt
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
            staticLink = case ghcLink dflags of
                          LinkStaticLib -> True
                          _ -> False

            home_mod_infos = eltsHpt hpt

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

            exe_file = exeFileName staticLink dflags

        linking_needed <- linkingNeeded dflags staticLink linkables pkg_deps

        if not (gopt Opt_ForceRecomp dflags) && not linking_needed
           then do debugTraceMsg dflags 2 (text exe_file <+> text "is up to date, linking not required.")
                   return Succeeded
           else do

        compilationProgressMsg dflags ("Linking " ++ exe_file ++ " ...")

        -- Don't showPass in Batch mode; doLink will do that for us.
        let link = case ghcLink dflags of
                LinkBinary    -> linkBinary
                LinkStaticLib -> linkStaticLib
                LinkDynLib    -> linkDynLibCheck
                other         -> panicBadLink other
        link dflags obj_files pkg_deps

        debugTraceMsg dflags 3 (text "link: done")

        -- linkBinary only returns if it succeeds
        return Succeeded

   | otherwise
   = do debugTraceMsg dflags 3 (text "link(batch): upsweep (partially) failed OR" $$
                                text "   Main.main not exported; not linking.")
        return Succeeded


linkingNeeded :: DynFlags -> Bool -> [Linkable] -> [InstalledUnitId] -> IO Bool
linkingNeeded dflags staticLink linkables pkg_deps = do
        -- if the modification time on the executable is later than the
        -- modification times on all of the objects and libraries, then omit
        -- linking (unless the -fforce-recomp flag was given).
  let exe_file = exeFileName staticLink dflags
  e_exe_time <- tryIO $ getModificationUTCTime exe_file
  case e_exe_time of
    Left _  -> return True
    Right t -> do
        -- first check object files and extra_ld_inputs
        let extra_ld_inputs = [ f | FileOption _ f <- ldInputs dflags ]
        e_extra_times <- mapM (tryIO . getModificationUTCTime) extra_ld_inputs
        let (errs,extra_times) = partitionEithers e_extra_times
        let obj_times =  map linkableTime linkables ++ extra_times
        if not (null errs) || any (t <) obj_times
            then return True
            else do

        -- next, check libraries. XXX this only checks Haskell libraries,
        -- not extra_libraries or -l things from the command line.
        let pkgstate = pkgState dflags
        let pkg_hslibs  = [ (collectLibraryPaths dflags [c], lib)
                          | Just c <- map (lookupInstalledPackage pkgstate) pkg_deps,
                            lib <- packageHsLibs dflags c ]

        pkg_libfiles <- mapM (uncurry (findHSLib dflags)) pkg_hslibs
        if any isNothing pkg_libfiles then return True else do
        e_lib_times <- mapM (tryIO . getModificationUTCTime)
                          (catMaybes pkg_libfiles)
        let (lib_errs,lib_times) = partitionEithers e_lib_times
        if not (null lib_errs) || any (t <) lib_times
           then return True
           else checkLinkInfo dflags pkg_deps exe_file

findHSLib :: DynFlags -> [String] -> String -> IO (Maybe FilePath)
findHSLib dflags dirs lib = do
  let batch_lib_file = if WayDyn `notElem` ways dflags
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
        throwGhcExceptionIO (CmdLineError ("does not exist: " ++ src))

   let
        dflags    = hsc_dflags hsc_env
        mb_o_file = outputFile dflags
        ghc_link  = ghcLink dflags      -- Set by -c or -no-link

        -- When linking, the -o argument refers to the linker's output.
        -- otherwise, we use it as the name for the pipeline's output.
        output
         -- If we are doing -fno-code, then act as if the output is
         -- 'Temporary'. This stops GHC trying to copy files to their
         -- final location.
         | HscNothing <- hscTarget dflags = Temporary TFL_CurrentModule
         | StopLn <- stop_phase, not (isNoLink ghc_link) = Persistent
                -- -o foo applies to linker
         | isJust mb_o_file = SpecificFile
                -- -o foo applies to the file we are compiling now
         | otherwise = Persistent

   ( _, out_file, _) <- runPipeline stop_phase hsc_env
                            (src, Nothing, fmap RealPhase mb_phase)
                            Nothing
                            output
                            Nothing{-no ModLocation-} []
   return out_file


doLink :: DynFlags -> Phase -> [FilePath] -> IO ()
doLink dflags stop_phase o_files
  | not (isStopLn stop_phase)
  = return ()           -- We stopped before the linking phase

  | otherwise
  = case ghcLink dflags of
        NoLink        -> return ()
        LinkBinary    -> linkBinary         dflags o_files []
        LinkStaticLib -> linkStaticLib      dflags o_files []
        LinkDynLib    -> linkDynLibCheck    dflags o_files []
        other         -> panicBadLink other


-- ---------------------------------------------------------------------------

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
  -> (FilePath, Maybe InputFileBuffer, Maybe PhasePlus)
                                -- ^ Pipeline input file name, optional
                                -- buffer and maybe -x suffix
  -> Maybe FilePath             -- ^ original basename (if different from ^^^)
  -> PipelineOutput             -- ^ Output filename
  -> Maybe ModLocation          -- ^ A ModLocation, if this is a Haskell module
  -> [FilePath]                 -- ^ foreign objects
  -> IO (DynFlags, FilePath, Maybe (ModIface, ModDetails))
                                -- ^ (final flags, output filename, interface)
runPipeline stop_phase hsc_env0 (input_fn, mb_input_buf, mb_phase)
             mb_basename output maybe_loc foreign_os

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
             start_phase = fromMaybe (RealPhase (startPhase suffix')) mb_phase

             isHaskell (RealPhase (Unlit _)) = True
             isHaskell (RealPhase (Cpp   _)) = True
             isHaskell (RealPhase (HsPp  _)) = True
             isHaskell (RealPhase (Hsc   _)) = True
             isHaskell (HscOut {})           = True
             isHaskell _                     = False

             isHaskellishFile = isHaskell start_phase

             env = PipeEnv{ stop_phase,
                            src_filename = input_fn,
                            src_basename = basename,
                            src_suffix = suffix',
                            output_spec = output }

         when (isBackpackishSuffix suffix') $
           throwGhcExceptionIO (UsageError
                       ("use --backpack to process " ++ input_fn))

         -- We want to catch cases of "you can't get there from here" before
         -- we start the pipeline, because otherwise it will just run off the
         -- end.
         let happensBefore' = happensBefore (targetPlatform dflags)
         case start_phase of
             RealPhase start_phase' ->
                 -- See Note [Partial ordering on phases]
                 -- Not the same as: (stop_phase `happensBefore` start_phase')
                 when (not (start_phase' `happensBefore'` stop_phase ||
                            start_phase' `eqPhase` stop_phase)) $
                       throwGhcExceptionIO (UsageError
                                   ("cannot compile this file to desired target: "
                                      ++ input_fn))
             HscOut {} -> return ()

         -- Write input buffer to temp file if requested
         input_fn' <- case (start_phase, mb_input_buf) of
             (RealPhase real_start_phase, Just input_buf) -> do
                 let suffix = phaseInputExt real_start_phase
                 fn <- newTempName dflags TFL_CurrentModule suffix
                 hdl <- openBinaryFile fn WriteMode
                 -- Add a LINE pragma so reported source locations will
                 -- mention the real input file, not this temp file.
                 hPutStrLn hdl $ "{-# LINE 1 \""++ input_fn ++ "\"#-}"
                 hPutStringBuffer hdl input_buf
                 hClose hdl
                 return fn
             (_, _) -> return input_fn

         debugTraceMsg dflags 4 (text "Running the pipeline")
         r <- runPipeline' start_phase hsc_env env input_fn'
                           maybe_loc foreign_os

         -- If we are compiling a Haskell module, and doing
         -- -dynamic-too, but couldn't do the -dynamic-too fast
         -- path, then rerun the pipeline for the dyn way
         let dflags = hsc_dflags hsc_env
         -- NB: Currently disabled on Windows (ref #7134, #8228, and #5987)
         when (not $ platformOS (targetPlatform dflags) == OSMinGW32) $ do
           when isHaskellishFile $ whenCannotGenerateDynamicToo dflags $ do
               debugTraceMsg dflags 4
                   (text "Running the pipeline again for -dynamic-too")
               let dflags' = dynamicTooMkDynamicDynFlags dflags
               hsc_env' <- newHscEnv dflags'
               _ <- runPipeline' start_phase hsc_env' env input_fn'
                                 maybe_loc foreign_os
               return ()
         return r

runPipeline'
  :: PhasePlus                  -- ^ When to start
  -> HscEnv                     -- ^ Compilation environment
  -> PipeEnv
  -> FilePath                   -- ^ Input filename
  -> Maybe ModLocation          -- ^ A ModLocation, if this is a Haskell module
  -> [FilePath]                 -- ^ foreign objects, if we have one
  -> IO (DynFlags, FilePath, Maybe (ModIface, ModDetails))
                                -- ^ (final flags, output filename, interface)
runPipeline' start_phase hsc_env env input_fn
             maybe_loc foreign_os
  = do
  -- Execute the pipeline...
  let state = PipeState{ hsc_env, maybe_loc, foreign_os = foreign_os, iface = Nothing }
  (pipe_state, fp) <- evalP (pipeLoop start_phase input_fn) env state
  return (pipeStateDynFlags pipe_state, fp, pipeStateModIface pipe_state)

-- ---------------------------------------------------------------------------
-- outer pipeline loop

-- | pipeLoop runs phases until we reach the stop phase
pipeLoop :: PhasePlus -> FilePath -> CompPipeline FilePath
pipeLoop phase input_fn = do
  env <- getPipeEnv
  dflags <- getDynFlags
  -- See Note [Partial ordering on phases]
  let happensBefore' = happensBefore (targetPlatform dflags)
      stopPhase = stop_phase env
  case phase of
   RealPhase realPhase | realPhase `eqPhase` stopPhase            -- All done
     -> -- Sometimes, a compilation phase doesn't actually generate any output
        -- (eg. the CPP phase when -fcpp is not turned on).  If we end on this
        -- stage, but we wanted to keep the output, then we have to explicitly
        -- copy the file, remembering to prepend a {-# LINE #-} pragma so that
        -- further compilation stages can tell what the original filename was.
        case output_spec env of
        Temporary _ ->
            return input_fn
        output ->
            do pst <- getPipeState
               final_fn <- liftIO $ getOutputFilename
                                        stopPhase output (src_basename env)
                                        dflags stopPhase (maybe_loc pst)
               when (final_fn /= input_fn) $ do
                  let msg = ("Copying `" ++ input_fn ++"' to `" ++ final_fn ++ "'")
                      line_prag = Just ("{-# LINE 1 \"" ++ src_filename env ++ "\" #-}\n")
                  liftIO $ copyWithHeader dflags msg line_prag input_fn final_fn
               return final_fn


     | not (realPhase `happensBefore'` stopPhase)
        -- Something has gone wrong.  We'll try to cover all the cases when
        -- this could happen, so if we reach here it is a panic.
        -- eg. it might happen if the -C flag is used on a source file that
        -- has {-# OPTIONS -fasm #-}.
     -> panic ("pipeLoop: at phase " ++ show realPhase ++
           " but I wanted to stop at phase " ++ show stopPhase)

   _
     -> do liftIO $ debugTraceMsg dflags 4
                                  (text "Running phase" <+> ppr phase)
           (next_phase, output_fn) <- runHookedPhase phase input_fn dflags
           case phase of
               HscOut {} -> do
                   -- We don't pass Opt_BuildDynamicToo to the backend
                   -- in DynFlags.
                   -- Instead it's run twice with flags accordingly set
                   -- per run.
                   let noDynToo = pipeLoop next_phase output_fn
                   let dynToo = do
                          setDynFlags $ gopt_unset dflags Opt_BuildDynamicToo
                          r <- pipeLoop next_phase output_fn
                          setDynFlags $ dynamicTooMkDynamicDynFlags dflags
                          -- TODO shouldn't ignore result:
                          _ <- pipeLoop phase input_fn
                          return r
                   ifGeneratingDynamicToo dflags dynToo noDynToo
               _ -> pipeLoop next_phase output_fn

runHookedPhase :: PhasePlus -> FilePath -> DynFlags
               -> CompPipeline (PhasePlus, FilePath)
runHookedPhase pp input dflags =
  lookupHook runPhaseHook runPhase dflags pp input dflags

-- -----------------------------------------------------------------------------
-- In each phase, we need to know into what filename to generate the
-- output.  All the logic about which filenames we generate output
-- into is embodied in the following function.

-- | Computes the next output filename after we run @next_phase@.
-- Like 'getOutputFilename', but it operates in the 'CompPipeline' monad
-- (which specifies all of the ambient information.)
phaseOutputFilename :: Phase{-next phase-} -> CompPipeline FilePath
phaseOutputFilename next_phase = do
  PipeEnv{stop_phase, src_basename, output_spec} <- getPipeEnv
  PipeState{maybe_loc, hsc_env} <- getPipeState
  let dflags = hsc_dflags hsc_env
  liftIO $ getOutputFilename stop_phase output_spec
                             src_basename dflags next_phase maybe_loc

-- | Computes the next output filename for something in the compilation
-- pipeline.  This is controlled by several variables:
--
--      1. 'Phase': the last phase to be run (e.g. 'stopPhase').  This
--         is used to tell if we're in the last phase or not, because
--         in that case flags like @-o@ may be important.
--      2. 'PipelineOutput': is this intended to be a 'Temporary' or
--         'Persistent' build output?  Temporary files just go in
--         a fresh temporary name.
--      3. 'String': what was the basename of the original input file?
--      4. 'DynFlags': the obvious thing
--      5. 'Phase': the phase we want to determine the output filename of.
--      6. @Maybe ModLocation@: the 'ModLocation' of the module we're
--         compiling; this can be used to override the default output
--         of an object file.  (TODO: do we actually need this?)
getOutputFilename
  :: Phase -> PipelineOutput -> String
  -> DynFlags -> Phase{-next phase-} -> Maybe ModLocation -> IO FilePath
getOutputFilename stop_phase output basename dflags next_phase maybe_location
 | is_last_phase, Persistent   <- output = persistent_fn
 | is_last_phase, SpecificFile <- output = case outputFile dflags of
                                           Just f -> return f
                                           Nothing ->
                                               panic "SpecificFile: No filename"
 | keep_this_output                      = persistent_fn
 | Temporary lifetime <- output          = newTempName dflags lifetime suffix
 | otherwise                             = newTempName dflags TFL_CurrentModule
   suffix
    where
          hcsuf      = hcSuf dflags
          odir       = objectDir dflags
          osuf       = objectSuf dflags
          keep_hc    = gopt Opt_KeepHcFiles dflags
          keep_hscpp = gopt Opt_KeepHscppFiles dflags
          keep_s     = gopt Opt_KeepSFiles dflags
          keep_bc    = gopt Opt_KeepLlvmFiles dflags

          myPhaseInputExt HCc       = hcsuf
          myPhaseInputExt MergeForeign = osuf
          myPhaseInputExt StopLn    = osuf
          myPhaseInputExt other     = phaseInputExt other

          is_last_phase = next_phase `eqPhase` stop_phase

          -- sometimes, we keep output from intermediate stages
          keep_this_output =
               case next_phase of
                       As _    | keep_s     -> True
                       LlvmOpt | keep_bc    -> True
                       HCc     | keep_hc    -> True
                       HsPp _  | keep_hscpp -> True   -- See #10869
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


-- | The fast LLVM Pipeline skips the mangler and assembler,
-- emitting object code directly from llc.
--
-- slow: opt -> llc -> .s -> mangler -> as -> .o
-- fast: opt -> llc -> .o
--
-- hidden flag: -ffast-llvm
--
-- if keep-s-files is specified, we need to go through
-- the slow pipeline (Kavon Farvardin requested this).
fastLlvmPipeline :: DynFlags -> Bool
fastLlvmPipeline dflags
  = not (gopt Opt_KeepSFiles dflags) && gopt Opt_FastLlvm dflags

-- | LLVM Options. These are flags to be passed to opt and llc, to ensure
-- consistency we list them in pairs, so that they form groups.
llvmOptions :: DynFlags
            -> [(String, String)]  -- ^ pairs of (opt, llc) arguments
llvmOptions dflags =
       [("-enable-tbaa -tbaa",  "-enable-tbaa") | gopt Opt_LlvmTBAA dflags ]
    ++ [("-relocation-model=" ++ rmodel
        ,"-relocation-model=" ++ rmodel) | not (null rmodel)]
    ++ [("-stack-alignment=" ++ (show align)
        ,"-stack-alignment=" ++ (show align)) | align > 0 ]
    ++ [("", "-filetype=obj") | fastLlvmPipeline dflags ]

    -- Additional llc flags
    ++ [("", "-mcpu=" ++ mcpu)   | not (null mcpu)
                                 , not (any (isInfixOf "-mcpu") (getOpts dflags opt_lc)) ]
    ++ [("", "-mattr=" ++ attrs) | not (null attrs) ]

  where target = platformMisc_llvmTarget $ platformMisc dflags
        Just (LlvmTarget _ mcpu mattr) = lookup target (llvmTargets $ llvmConfig dflags)

        -- Relocation models
        rmodel | gopt Opt_PIC dflags        = "pic"
               | positionIndependent dflags = "pic"
               | WayDyn `elem` ways dflags  = "dynamic-no-pic"
               | otherwise                  = "static"

        align :: Int
        align = case platformArch (targetPlatform dflags) of
                  ArchX86_64 | isAvxEnabled dflags -> 32
                  _                                -> 0

        attrs :: String
        attrs = intercalate "," $ mattr
              ++ ["+sse42"   | isSse4_2Enabled dflags   ]
              ++ ["+sse2"    | isSse2Enabled dflags     ]
              ++ ["+sse"     | isSseEnabled dflags      ]
              ++ ["+avx512f" | isAvx512fEnabled dflags  ]
              ++ ["+avx2"    | isAvx2Enabled dflags     ]
              ++ ["+avx"     | isAvxEnabled dflags      ]
              ++ ["+avx512cd"| isAvx512cdEnabled dflags ]
              ++ ["+avx512er"| isAvx512erEnabled dflags ]
              ++ ["+avx512pf"| isAvx512pfEnabled dflags ]
              ++ ["+bmi"     | isBmiEnabled dflags      ]
              ++ ["+bmi2"    | isBmi2Enabled dflags     ]

-- -----------------------------------------------------------------------------
-- | Each phase in the pipeline returns the next phase to execute, and the
-- name of the file in which the output was placed.
--
-- We must do things dynamically this way, because we often don't know
-- what the rest of the phases will be until part-way through the
-- compilation: for example, an {-# OPTIONS -fasm #-} at the beginning
-- of a source file can change the latter stages of the pipeline from
-- taking the LLVM route to using the native code generator.
--
runPhase :: PhasePlus   -- ^ Run this phase
         -> FilePath    -- ^ name of the input file
         -> DynFlags    -- ^ for convenience, we pass the current dflags in
         -> CompPipeline (PhasePlus,           -- next phase to run
                          FilePath)            -- output filename

        -- Invariant: the output filename always contains the output
        -- Interesting case: Hsc when there is no recompilation to do
        --                   Then the output filename is still a .o file


-------------------------------------------------------------------------------
-- Unlit phase

runPhase (RealPhase (Unlit sf)) input_fn dflags
  = do
       output_fn <- phaseOutputFilename (Cpp sf)

       let flags = [ -- The -h option passes the file name for unlit to
                     -- put in a #line directive
                     SysTools.Option     "-h"
                     -- See Note [Don't normalise input filenames].
                   , SysTools.Option $ escape input_fn
                   , SysTools.FileOption "" input_fn
                   , SysTools.FileOption "" output_fn
                   ]

       liftIO $ SysTools.runUnlit dflags flags

       return (RealPhase (Cpp sf), output_fn)
  where
       -- escape the characters \, ", and ', but don't try to escape
       -- Unicode or anything else (so we don't use Util.charToC
       -- here).  If we get this wrong, then in
       -- GHC.HsToCore.Coverage.isGoodTickSrcSpan where we check that the filename in
       -- a SrcLoc is the same as the source filenaame, the two will
       -- look bogusly different. See test:
       -- libraries/hpc/tests/function/subdir/tough2.hs
       escape ('\\':cs) = '\\':'\\': escape cs
       escape ('\"':cs) = '\\':'\"': escape cs
       escape ('\'':cs) = '\\':'\'': escape cs
       escape (c:cs)    = c : escape cs
       escape []        = []

-------------------------------------------------------------------------------
-- Cpp phase : (a) gets OPTIONS out of file
--             (b) runs cpp if necessary

runPhase (RealPhase (Cpp sf)) input_fn dflags0
  = do
       src_opts <- liftIO $ getOptionsFromFile dflags0 input_fn
       (dflags1, unhandled_flags, warns)
           <- liftIO $ parseDynamicFilePragma dflags0 src_opts
       setDynFlags dflags1
       liftIO $ checkProcessArgsResult dflags1 unhandled_flags

       if not (xopt LangExt.Cpp dflags1) then do
           -- we have to be careful to emit warnings only once.
           unless (gopt Opt_Pp dflags1) $
               liftIO $ handleFlagWarnings dflags1 warns

           -- no need to preprocess CPP, just pass input file along
           -- to the next phase of the pipeline.
           return (RealPhase (HsPp sf), input_fn)
        else do
            output_fn <- phaseOutputFilename (HsPp sf)
            liftIO $ doCpp dflags1 True{-raw-}
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

            return (RealPhase (HsPp sf), output_fn)

-------------------------------------------------------------------------------
-- HsPp phase

runPhase (RealPhase (HsPp sf)) input_fn dflags
  = do
       if not (gopt Opt_Pp dflags) then
           -- no need to preprocess, just pass input file along
           -- to the next phase of the pipeline.
          return (RealPhase (Hsc sf), input_fn)
        else do
            PipeEnv{src_basename, src_suffix} <- getPipeEnv
            let orig_fn = src_basename <.> src_suffix
            output_fn <- phaseOutputFilename (Hsc sf)
            liftIO $ SysTools.runPp dflags
                           ( [ SysTools.Option     orig_fn
                             , SysTools.Option     input_fn
                             , SysTools.FileOption "" output_fn
                             ]
                           )

            -- re-read pragmas now that we've parsed the file (see #3674)
            src_opts <- liftIO $ getOptionsFromFile dflags output_fn
            (dflags1, unhandled_flags, warns)
                <- liftIO $ parseDynamicFilePragma dflags src_opts
            setDynFlags dflags1
            liftIO $ checkProcessArgsResult dflags1 unhandled_flags
            liftIO $ handleFlagWarnings dflags1 warns

            return (RealPhase (Hsc sf), output_fn)

-----------------------------------------------------------------------------
-- Hsc phase

-- Compilation of a single module, in "legacy" mode (_not_ under
-- the direction of the compilation manager).
runPhase (RealPhase (Hsc src_flavour)) input_fn dflags0
 = do   -- normal Hsc mode, not mkdependHS

        PipeEnv{ stop_phase=stop,
                 src_basename=basename,
                 src_suffix=suff } <- getPipeEnv

  -- we add the current directory (i.e. the directory in which
  -- the .hs files resides) to the include path, since this is
  -- what gcc does, and it's probably what you want.
        let current_dir = takeDirectory basename
            new_includes = addQuoteInclude paths [current_dir]
            paths = includePaths dflags0
            dflags = dflags0 { includePaths = new_includes }

        setDynFlags dflags

  -- gather the imports and module name
        (hspp_buf,mod_name,imps,src_imps) <- liftIO $ do
          do
            buf <- hGetStringBuffer input_fn
            eimps <- getImports dflags buf input_fn (basename <.> suff)
            case eimps of
              Left errs -> throwErrors errs
              Right (src_imps,imps,L _ mod_name) -> return
                  (Just buf, mod_name, imps, src_imps)

  -- Take -o into account if present
  -- Very like -ohi, but we must *only* do this if we aren't linking
  -- (If we're linking then the -o applies to the linked thing, not to
  -- the object file for one module.)
  -- Note the nasty duplication with the same computation in compileFile above
        location <- getLocation src_flavour mod_name

        let o_file = ml_obj_file location -- The real object file
            hi_file = ml_hi_file location
            hie_file = ml_hie_file location
            dest_file | writeInterfaceOnlyMode dflags
                            = hi_file
                      | otherwise
                            = o_file

  -- Figure out if the source has changed, for recompilation avoidance.
  --
  -- Setting source_unchanged to True means that M.o (or M.hie) seems
  -- to be up to date wrt M.hs; so no need to recompile unless imports have
  -- changed (which the compiler itself figures out).
  -- Setting source_unchanged to False tells the compiler that M.o is out of
  -- date wrt M.hs (or M.o doesn't exist) so we must recompile regardless.
        src_timestamp <- liftIO $ getModificationUTCTime (basename <.> suff)

        source_unchanged <- liftIO $
          if not (isStopLn stop)
                -- SourceModified unconditionally if
                --      (a) recompilation checker is off, or
                --      (b) we aren't going all the way to .o file (e.g. ghc -S)
             then return SourceModified
                -- Otherwise look at file modification dates
             else do dest_file_mod <- sourceModified dest_file src_timestamp
                     hie_file_mod <- if gopt Opt_WriteHie dflags
                                        then sourceModified hie_file
                                                            src_timestamp
                                        else pure False
                     if dest_file_mod || hie_file_mod
                        then return SourceModified
                        else return SourceUnmodified

        PipeState{hsc_env=hsc_env'} <- getPipeState

  -- Tell the finder cache about this module
        mod <- liftIO $ addHomeModuleToFinder hsc_env' mod_name location

  -- Make the ModSummary to hand to hscMain
        let
            mod_summary = ModSummary {  ms_mod       = mod,
                                        ms_hsc_src   = src_flavour,
                                        ms_hspp_file = input_fn,
                                        ms_hspp_opts = dflags,
                                        ms_hspp_buf  = hspp_buf,
                                        ms_location  = location,
                                        ms_hs_date   = src_timestamp,
                                        ms_obj_date  = Nothing,
                                        ms_parsed_mod   = Nothing,
                                        ms_iface_date   = Nothing,
                                        ms_hie_date     = Nothing,
                                        ms_textual_imps = imps,
                                        ms_srcimps      = src_imps }

  -- run the compiler!
        let msg hsc_env _ what _ = oneShotMsg hsc_env what
        (result, plugin_dflags) <-
          liftIO $ hscIncrementalCompile True Nothing (Just msg) hsc_env'
                            mod_summary source_unchanged Nothing (1,1)

        -- In the rest of the pipeline use the dflags with plugin info
        setDynFlags plugin_dflags

        return (HscOut src_flavour mod_name result,
                panic "HscOut doesn't have an input filename")

runPhase (HscOut src_flavour mod_name result) _ dflags = do
        location <- getLocation src_flavour mod_name
        setModLocation location

        let o_file = ml_obj_file location -- The real object file
            hsc_lang = hscTarget dflags
            next_phase = hscPostBackendPhase src_flavour hsc_lang

        case result of
            HscNotGeneratingCode _ _ ->
                return (RealPhase StopLn,
                        panic "No output filename from Hsc when no-code")
            HscUpToDate _ _ ->
                do liftIO $ touchObjectFile dflags o_file
                   -- The .o file must have a later modification date
                   -- than the source file (else we wouldn't get Nothing)
                   -- but we touch it anyway, to keep 'make' happy (we think).
                   return (RealPhase StopLn, o_file)
            HscUpdateBoot _ _ ->
                do -- In the case of hs-boot files, generate a dummy .o-boot
                   -- stamp file for the benefit of Make
                   liftIO $ touchObjectFile dflags o_file
                   return (RealPhase StopLn, o_file)
            HscUpdateSig _ _ ->
                do -- We need to create a REAL but empty .o file
                   -- because we are going to attempt to put it in a library
                   PipeState{hsc_env=hsc_env'} <- getPipeState
                   let input_fn = expectJust "runPhase" (ml_hs_file location)
                       basename = dropExtension input_fn
                   liftIO $ compileEmptyStub dflags hsc_env' basename location mod_name
                   return (RealPhase StopLn, o_file)
            HscRecomp { hscs_guts = cgguts,
                        hscs_mod_location = mod_location,
                        hscs_mod_details = mod_details,
                        hscs_partial_iface = partial_iface,
                        hscs_old_iface_hash = mb_old_iface_hash,
                        hscs_iface_dflags = iface_dflags }
              -> do output_fn <- phaseOutputFilename next_phase

                    PipeState{hsc_env=hsc_env'} <- getPipeState

                    (outputFilename, mStub, foreign_files, cg_infos) <- liftIO $
                      hscGenHardCode hsc_env' cgguts mod_location output_fn

                    final_iface <- liftIO (mkFullIface hsc_env'{hsc_dflags=iface_dflags} partial_iface (Just cg_infos))
                    let final_mod_details = {-# SCC updateModDetailsIdInfos #-}
                                            updateModDetailsIdInfos iface_dflags cg_infos mod_details
                    setIface final_iface final_mod_details

                    -- See Note [Writing interface files]
                    let if_dflags = dflags `gopt_unset` Opt_BuildDynamicToo
                    liftIO $ hscMaybeWriteIface if_dflags final_iface mb_old_iface_hash mod_location

                    stub_o <- liftIO (mapM (compileStub hsc_env') mStub)
                    foreign_os <- liftIO $
                      mapM (uncurry (compileForeign hsc_env')) foreign_files
                    setForeignOs (maybe [] return stub_o ++ foreign_os)

                    return (RealPhase next_phase, outputFilename)

-----------------------------------------------------------------------------
-- Cmm phase

runPhase (RealPhase CmmCpp) input_fn dflags
  = do output_fn <- phaseOutputFilename Cmm
       liftIO $ doCpp dflags False{-not raw-}
                      input_fn output_fn
       return (RealPhase Cmm, output_fn)

runPhase (RealPhase Cmm) input_fn dflags
  = do let hsc_lang = hscTarget dflags
       let next_phase = hscPostBackendPhase HsSrcFile hsc_lang
       output_fn <- phaseOutputFilename next_phase
       PipeState{hsc_env} <- getPipeState
       liftIO $ hscCompileCmmFile hsc_env input_fn output_fn
       return (RealPhase next_phase, output_fn)

-----------------------------------------------------------------------------
-- Cc phase

runPhase (RealPhase cc_phase) input_fn dflags
   | any (cc_phase `eqPhase`) [Cc, Ccxx, HCc, Cobjc, Cobjcxx]
   = do
        let platform = targetPlatform dflags
            hcc = cc_phase `eqPhase` HCc

        let cmdline_include_paths = includePaths dflags

        -- HC files have the dependent packages stamped into them
        pkgs <- if hcc then liftIO $ getHCFilePackages input_fn else return []

        -- add package include paths even if we're just compiling .c
        -- files; this is the Value Add(TM) that using ghc instead of
        -- gcc gives you :)
        pkg_include_dirs <- liftIO $ getPackageIncludePath dflags pkgs
        let include_paths_global = foldr (\ x xs -> ("-I" ++ x) : xs) []
              (includePathsGlobal cmdline_include_paths ++ pkg_include_dirs)
        let include_paths_quote = foldr (\ x xs -> ("-iquote" ++ x) : xs) []
              (includePathsQuote cmdline_include_paths)
        let include_paths = include_paths_quote ++ include_paths_global

        -- pass -D or -optP to preprocessor when compiling foreign C files
        -- (#16737). Doing it in this way is simpler and also enable the C
        -- compiler to perform preprocessing and parsing in a single pass,
        -- but it may introduce inconsistency if a different pgm_P is specified.
        let more_preprocessor_opts = concat
              [ ["-Xpreprocessor", i]
              | not hcc
              , i <- getOpts dflags opt_P
              ]

        let gcc_extra_viac_flags = extraGccViaCFlags dflags
        let pic_c_flags = picCCOpts dflags

        let verbFlags = getVerbFlags dflags

        -- cc-options are not passed when compiling .hc files.  Our
        -- hc code doesn't not #include any header files anyway, so these
        -- options aren't necessary.
        pkg_extra_cc_opts <- liftIO $
          if hcc
             then return []
             else getPackageExtraCcOpts dflags pkgs

        framework_paths <-
            if platformUsesFrameworks platform
            then do pkgFrameworkPaths <- liftIO $ getPackageFrameworkPath dflags pkgs
                    let cmdlineFrameworkPaths = frameworkPaths dflags
                    return $ map ("-F"++)
                                 (cmdlineFrameworkPaths ++ pkgFrameworkPaths)
            else return []

        let cc_opt | optLevel dflags >= 2 = [ "-O2" ]
                   | optLevel dflags >= 1 = [ "-O" ]
                   | otherwise            = []

        -- Decide next phase
        let next_phase = As False
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

        ghcVersionH <- liftIO $ getGhcVersionPathName dflags

        liftIO $ SysTools.runCc (phaseForeignLanguage cc_phase) dflags (
                        [ SysTools.FileOption "" input_fn
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
                              thisPackage dflags == baseUnitId
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
                       ++ (if (cc_phase /= Ccxx && cc_phase /= Cobjcxx)
                             then ["-Wimplicit"]
                             else [])

                       ++ (if hcc
                             then gcc_extra_viac_flags ++ more_hcc_opts
                             else [])
                       ++ verbFlags
                       ++ [ "-S" ]
                       ++ cc_opt
                       ++ [ "-include", ghcVersionH ]
                       ++ framework_paths
                       ++ include_paths
                       ++ more_preprocessor_opts
                       ++ pkg_extra_cc_opts
                       ))

        return (RealPhase next_phase, output_fn)

-----------------------------------------------------------------------------
-- As, SpitAs phase : Assembler

-- This is for calling the assembler on a regular assembly file
runPhase (RealPhase (As with_cpp)) input_fn dflags
  = do
        -- LLVM from version 3.0 onwards doesn't support the OS X system
        -- assembler, so we use clang as the assembler instead. (#5636)
        let as_prog | hscTarget dflags == HscLlvm &&
                      platformOS (targetPlatform dflags) == OSDarwin
                    = SysTools.runClang
                    | otherwise = SysTools.runAs

        let cmdline_include_paths = includePaths dflags
        let pic_c_flags = picCCOpts dflags

        next_phase <- maybeMergeForeign
        output_fn <- phaseOutputFilename next_phase

        -- we create directories for the object file, because it
        -- might be a hierarchical module.
        liftIO $ createDirectoryIfMissing True (takeDirectory output_fn)

        ccInfo <- liftIO $ getCompilerInfo dflags
        let global_includes = [ SysTools.Option ("-I" ++ p)
                              | p <- includePathsGlobal cmdline_include_paths ]
        let local_includes = [ SysTools.Option ("-iquote" ++ p)
                             | p <- includePathsQuote cmdline_include_paths ]
        let runAssembler inputFilename outputFilename
              = liftIO $ do
                  withAtomicRename outputFilename $ \temp_outputFilename -> do
                    as_prog
                       dflags
                       (local_includes ++ global_includes
                       -- See Note [-fPIC for assembler]
                       ++ map SysTools.Option pic_c_flags
                       -- See Note [Produce big objects on Windows]
                       ++ [ SysTools.Option "-Wa,-mbig-obj"
                          | platformOS (targetPlatform dflags) == OSMinGW32
                          , not $ target32Bit (targetPlatform dflags)
                          ]

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
                       ++ (if any (ccInfo ==) [Clang, AppleClang, AppleClang51]
                            then [SysTools.Option "-Qunused-arguments"]
                            else [])
                       ++ [ SysTools.Option "-x"
                          , if with_cpp
                              then SysTools.Option "assembler-with-cpp"
                              else SysTools.Option "assembler"
                          , SysTools.Option "-c"
                          , SysTools.FileOption "" inputFilename
                          , SysTools.Option "-o"
                          , SysTools.FileOption "" temp_outputFilename
                          ])

        liftIO $ debugTraceMsg dflags 4 (text "Running the assembler")
        runAssembler input_fn output_fn

        return (RealPhase next_phase, output_fn)


-----------------------------------------------------------------------------
-- LlvmOpt phase
runPhase (RealPhase LlvmOpt) input_fn dflags
  = do
    output_fn <- phaseOutputFilename LlvmLlc

    liftIO $ SysTools.runLlvmOpt dflags
               (   optFlag
                ++ defaultOptions ++
                [ SysTools.FileOption "" input_fn
                , SysTools.Option "-o"
                , SysTools.FileOption "" output_fn]
                )

    return (RealPhase LlvmLlc, output_fn)
  where
        -- we always (unless -optlo specified) run Opt since we rely on it to
        -- fix up some pretty big deficiencies in the code we generate
        optIdx = max 0 $ min 2 $ optLevel dflags  -- ensure we're in [0,2]
        llvmOpts = case lookup optIdx $ llvmPasses $ llvmConfig dflags of
                    Just passes -> passes
                    Nothing -> panic ("runPhase LlvmOpt: llvm-passes file "
                                      ++ "is missing passes for level "
                                      ++ show optIdx)

        -- don't specify anything if user has specified commands. We do this
        -- for opt but not llc since opt is very specifically for optimisation
        -- passes only, so if the user is passing us extra options we assume
        -- they know what they are doing and don't get in the way.
        optFlag = if null (getOpts dflags opt_lo)
                  then map SysTools.Option $ words llvmOpts
                  else []

        defaultOptions = map SysTools.Option . concat . fmap words . fst
                       $ unzip (llvmOptions dflags)

-----------------------------------------------------------------------------
-- LlvmLlc phase

runPhase (RealPhase LlvmLlc) input_fn dflags
  = do
    next_phase <- if | fastLlvmPipeline dflags -> maybeMergeForeign
                     -- hidden debugging flag '-dno-llvm-mangler' to skip mangling
                     | gopt Opt_NoLlvmMangler dflags -> return (As False)
                     | otherwise -> return LlvmMangle

    output_fn <- phaseOutputFilename next_phase

    liftIO $ SysTools.runLlvmLlc dflags
                (  optFlag
                ++ defaultOptions
                ++ [ SysTools.FileOption "" input_fn
                   , SysTools.Option "-o"
                   , SysTools.FileOption "" output_fn
                   ]
                )

    return (RealPhase next_phase, output_fn)
  where
    -- Note [Clamping of llc optimizations]
    --
    -- See #13724
    --
    -- we clamp the llc optimization between [1,2]. This is because passing -O0
    -- to llc 3.9 or llc 4.0, the naive register allocator can fail with
    --
    --   Error while trying to spill R1 from class GPR: Cannot scavenge register
    --   without an emergency spill slot!
    --
    -- Observed at least with target 'arm-unknown-linux-gnueabihf'.
    --
    --
    -- With LLVM4, llc -O3 crashes when ghc-stage1 tries to compile
    --   rts/HeapStackCheck.cmm
    --
    -- llc -O3 '-mtriple=arm-unknown-linux-gnueabihf' -enable-tbaa /var/folders/fv/xqjrpfj516n5xq_m_ljpsjx00000gn/T/ghc33674_0/ghc_6.bc -o /var/folders/fv/xqjrpfj516n5xq_m_ljpsjx00000gn/T/ghc33674_0/ghc_7.lm_s
    -- 0  llc                      0x0000000102ae63e8 llvm::sys::PrintStackTrace(llvm::raw_ostream&) + 40
    -- 1  llc                      0x0000000102ae69a6 SignalHandler(int) + 358
    -- 2  libsystem_platform.dylib 0x00007fffc23f4b3a _sigtramp + 26
    -- 3  libsystem_c.dylib        0x00007fffc226498b __vfprintf + 17876
    -- 4  llc                      0x00000001029d5123 llvm::SelectionDAGISel::LowerArguments(llvm::Function const&) + 5699
    -- 5  llc                      0x0000000102a21a35 llvm::SelectionDAGISel::SelectAllBasicBlocks(llvm::Function const&) + 3381
    -- 6  llc                      0x0000000102a202b1 llvm::SelectionDAGISel::runOnMachineFunction(llvm::MachineFunction&) + 1457
    -- 7  llc                      0x0000000101bdc474 (anonymous namespace)::ARMDAGToDAGISel::runOnMachineFunction(llvm::MachineFunction&) + 20
    -- 8  llc                      0x00000001025573a6 llvm::MachineFunctionPass::runOnFunction(llvm::Function&) + 134
    -- 9  llc                      0x000000010274fb12 llvm::FPPassManager::runOnFunction(llvm::Function&) + 498
    -- 10 llc                      0x000000010274fd23 llvm::FPPassManager::runOnModule(llvm::Module&) + 67
    -- 11 llc                      0x00000001027501b8 llvm::legacy::PassManagerImpl::run(llvm::Module&) + 920
    -- 12 llc                      0x000000010195f075 compileModule(char**, llvm::LLVMContext&) + 12133
    -- 13 llc                      0x000000010195bf0b main + 491
    -- 14 libdyld.dylib            0x00007fffc21e5235 start + 1
    -- Stack dump:
    -- 0.  Program arguments: llc -O3 -mtriple=arm-unknown-linux-gnueabihf -enable-tbaa /var/folders/fv/xqjrpfj516n5xq_m_ljpsjx00000gn/T/ghc33674_0/ghc_6.bc -o /var/folders/fv/xqjrpfj516n5xq_m_ljpsjx00000gn/T/ghc33674_0/ghc_7.lm_s
    -- 1.  Running pass 'Function Pass Manager' on module '/var/folders/fv/xqjrpfj516n5xq_m_ljpsjx00000gn/T/ghc33674_0/ghc_6.bc'.
    -- 2.  Running pass 'ARM Instruction Selection' on function '@"stg_gc_f1$def"'
    --
    -- Observed at least with -mtriple=arm-unknown-linux-gnueabihf -enable-tbaa
    --
    llvmOpts = case optLevel dflags of
      0 -> "-O1" -- required to get the non-naive reg allocator. Passing -regalloc=greedy is not sufficient.
      1 -> "-O1"
      _ -> "-O2"

    optFlag = if null (getOpts dflags opt_lc)
              then map SysTools.Option $ words llvmOpts
              else []

    defaultOptions = map SysTools.Option . concatMap words . snd
                   $ unzip (llvmOptions dflags)


-----------------------------------------------------------------------------
-- LlvmMangle phase

runPhase (RealPhase LlvmMangle) input_fn dflags
  = do
      let next_phase = As False
      output_fn <- phaseOutputFilename next_phase
      liftIO $ llvmFixupAsm dflags input_fn output_fn
      return (RealPhase next_phase, output_fn)

-----------------------------------------------------------------------------
-- merge in stub objects

runPhase (RealPhase MergeForeign) input_fn dflags
 = do
     PipeState{foreign_os} <- getPipeState
     output_fn <- phaseOutputFilename StopLn
     liftIO $ createDirectoryIfMissing True (takeDirectory output_fn)
     if null foreign_os
       then panic "runPhase(MergeForeign): no foreign objects"
       else do
         liftIO $ joinObjectFiles dflags (input_fn : foreign_os) output_fn
         return (RealPhase StopLn, output_fn)

-- warning suppression
runPhase (RealPhase other) _input_fn _dflags =
   panic ("runPhase: don't know how to run phase " ++ show other)

maybeMergeForeign :: CompPipeline Phase
maybeMergeForeign
 = do
     PipeState{foreign_os} <- getPipeState
     if null foreign_os then return StopLn else return MergeForeign

getLocation :: HscSource -> ModuleName -> CompPipeline ModLocation
getLocation src_flavour mod_name = do
    dflags <- getDynFlags

    PipeEnv{ src_basename=basename,
             src_suffix=suff } <- getPipeEnv
    PipeState { maybe_loc=maybe_loc} <- getPipeState
    case maybe_loc of
        -- Build a ModLocation to pass to hscMain.
        -- The source filename is rather irrelevant by now, but it's used
        -- by hscMain for messages.  hscMain also needs
        -- the .hi and .o filenames. If we already have a ModLocation
        -- then simply update the extensions of the interface and object
        -- files to match the DynFlags, otherwise use the logic in Finder.
      Just l -> return $ l
        { ml_hs_file = Just $ basename <.> suff
        , ml_hi_file = ml_hi_file l -<.> hiSuf dflags
        , ml_obj_file = ml_obj_file l -<.> objectSuf dflags
        }
      _ -> do
        location1 <- liftIO $ mkHomeModLocation2 dflags mod_name basename suff

        -- Boot-ify it if necessary
        let location2
              | HsBootFile <- src_flavour = addBootSuffixLocnOut location1
              | otherwise                 = location1


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
        -- Note the nasty duplication with the same computation in compileFile
        -- above
        let expl_o_file = outputFile dflags
            location4 | Just ofile <- expl_o_file
                      , isNoLink (ghcLink dflags)
                      = location3 { ml_obj_file = ofile }
                      | otherwise = location3
        return location4

-----------------------------------------------------------------------------
-- Look for the /* GHC_PACKAGES ... */ comment at the top of a .hc file

getHCFilePackages :: FilePath -> IO [InstalledUnitId]
getHCFilePackages filename =
  Exception.bracket (openFile filename ReadMode) hClose $ \h -> do
    l <- hGetLine h
    case l of
      '/':'*':' ':'G':'H':'C':'_':'P':'A':'C':'K':'A':'G':'E':'S':rest ->
          return (map stringToInstalledUnitId (words rest))
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

{-
Note [-Xlinker -rpath vs -Wl,-rpath]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-Wl takes a comma-separated list of options which in the case of
-Wl,-rpath -Wl,some,path,with,commas parses the path with commas
as separate options.
Buck, the build system, produces paths with commas in them.

-Xlinker doesn't have this disadvantage and as far as I can tell
it is supported by both gcc and clang. Anecdotally nvcc supports
-Xlinker, but not -Wl.
-}

linkBinary :: DynFlags -> [FilePath] -> [InstalledUnitId] -> IO ()
linkBinary = linkBinary' False

linkBinary' :: Bool -> DynFlags -> [FilePath] -> [InstalledUnitId] -> IO ()
linkBinary' staticLink dflags o_files dep_packages = do
    let platform = targetPlatform dflags
        toolSettings' = toolSettings dflags
        verbFlags = getVerbFlags dflags
        output_fn = exeFileName staticLink dflags

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
           WayDyn `elem` ways dflags
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "$ORIGIN" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
                  -- See Note [-Xlinker -rpath vs -Wl,-rpath]
                  rpath = if gopt Opt_RPath dflags
                          then ["-Xlinker", "-rpath", "-Xlinker", libpath]
                          else []
                  -- Solaris 11's linker does not support -rpath-link option. It silently
                  -- ignores it and then complains about next option which is -l<some
                  -- dir> as being a directory and not expected object file, E.g
                  -- ld: elf error: file
                  -- /tmp/ghc-src/libraries/base/dist-install/build:
                  -- elf_begin: I/O error: region read: Is a directory
                  rpathlink = if (platformOS platform) == OSSolaris2
                              then []
                              else ["-Xlinker", "-rpath-link", "-Xlinker", l]
              in ["-L" ++ l] ++ rpathlink ++ rpath
         | osMachOTarget (platformOS platform) &&
           dynLibLoader dflags == SystemDependent &&
           WayDyn `elem` ways dflags &&
           gopt Opt_RPath dflags
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "@loader_path" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
              in ["-L" ++ l] ++ ["-Xlinker", "-rpath", "-Xlinker", libpath]
         | otherwise = ["-L" ++ l]

    pkg_lib_path_opts <-
      if gopt Opt_SingleLibFolder dflags
      then do
        libs <- getLibs dflags dep_packages
        tmpDir <- newTempDir dflags
        sequence_ [ copyFile lib (tmpDir </> basename)
                  | (lib, basename) <- libs]
        return [ "-L" ++ tmpDir ]
      else pure pkg_lib_path_opts

    let
      dead_strip
        | gopt Opt_WholeArchiveHsLibs dflags = []
        | otherwise = if osSubsectionsViaSymbols (platformOS platform)
                        then ["-Wl,-dead_strip"]
                        else []
    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    extraLinkObj <- mkExtraObjToLinkIntoBinary dflags
    noteLinkObjs <- mkNoteObjsToLinkIntoBinary dflags dep_packages

    let
      (pre_hs_libs, post_hs_libs)
        | gopt Opt_WholeArchiveHsLibs dflags
        = if platformOS platform == OSDarwin
            then (["-Wl,-all_load"], [])
              -- OS X does not have a flag to turn off -all_load
            else (["-Wl,--whole-archive"], ["-Wl,--no-whole-archive"])
        | otherwise
        = ([],[])

    pkg_link_opts <- do
        (package_hs_libs, extra_libs, other_flags) <- getPackageLinkOpts dflags dep_packages
        return $ if staticLink
            then package_hs_libs -- If building an executable really means making a static
                                 -- library (e.g. iOS), then we only keep the -l options for
                                 -- HS packages, because libtool doesn't accept other options.
                                 -- In the case of iOS these need to be added by hand to the
                                 -- final link in Xcode.
            else other_flags ++ dead_strip
                  ++ pre_hs_libs ++ package_hs_libs ++ post_hs_libs
                  ++ extra_libs
                 -- -Wl,-u,<sym> contained in other_flags
                 -- needs to be put before -l<package>,
                 -- otherwise Solaris linker fails linking
                 -- a binary with unresolved symbols in RTS
                 -- which are defined in base package
                 -- the reason for this is a note in ld(1) about
                 -- '-u' option: "The placement of this option
                 -- on the command line is significant.
                 -- This option must be placed before the library
                 -- that defines the symbol."

    -- frameworks
    pkg_framework_opts <- getPkgFrameworkOpts dflags platform dep_packages
    let framework_opts = getFrameworkOpts dflags platform

        -- probably _stub.o files
    let extra_ld_inputs = ldInputs dflags

    rc_objs <- maybeCreateManifest dflags output_fn

    let link = if staticLink
                   then SysTools.runLibtool
                   else SysTools.runLink
    link dflags (
                       map SysTools.Option verbFlags
                      ++ [ SysTools.Option "-o"
                         , SysTools.FileOption "" output_fn
                         ]
                      ++ libmLinkOpts
                      ++ map SysTools.Option (
                         []

                      -- See Note [No PIE when linking]
                      ++ picCCOpts dflags

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
                      ++ (if toolSettings_ldSupportsCompactUnwind toolSettings' &&
                             not staticLink &&
                             (platformOS platform == OSDarwin) &&
                             case platformArch platform of
                               ArchX86 -> True
                               ArchX86_64 -> True
                               ArchARM {} -> True
                               ArchARM64  -> True
                               _ -> False
                          then ["-Wl,-no_compact_unwind"]
                          else [])

                      -- '-Wl,-read_only_relocs,suppress'
                      -- ld gives loads of warnings like:
                      --     ld: warning: text reloc in _base_GHCziArr_unsafeArray_info to _base_GHCziArr_unsafeArray_closure
                      -- when linking any program. We're not sure
                      -- whether this is something we ought to fix, but
                      -- for now this flags silences them.
                      ++ (if platformOS   platform == OSDarwin &&
                             platformArch platform == ArchX86 &&
                             not staticLink
                          then ["-Wl,-read_only_relocs,suppress"]
                          else [])

                      ++ (if toolSettings_ldIsGnuLd toolSettings' &&
                             not (gopt Opt_WholeArchiveHsLibs dflags)
                          then ["-Wl,--gc-sections"]
                          else [])

                      ++ o_files
                      ++ lib_path_opts)
                      ++ extra_ld_inputs
                      ++ map SysTools.Option (
                         rc_objs
                      ++ framework_opts
                      ++ pkg_lib_path_opts
                      ++ extraLinkObj:noteLinkObjs
                      ++ pkg_link_opts
                      ++ pkg_framework_opts
                      ++ (if platformOS platform == OSDarwin
                          then [ "-Wl,-dead_strip_dylibs" ]
                          else [])
                    ))

exeFileName :: Bool -> DynFlags -> FilePath
exeFileName staticLink dflags
  | Just s <- outputFile dflags =
      case platformOS (targetPlatform dflags) of
          OSMinGW32 -> s <?.> "exe"
          _         -> if staticLink
                         then s <?.> "a"
                         else s
  | otherwise =
      if platformOS (targetPlatform dflags) == OSMinGW32
      then "main.exe"
      else if staticLink
           then "liba.a"
           else "a.out"
 where s <?.> ext | null (takeExtension s) = s <.> ext
                  | otherwise              = s

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

         rc_filename <- newTempName dflags TFL_CurrentModule "rc"
         rc_obj_filename <-
           newTempName dflags TFL_GhcSession (objectSuf dflags)

         writeFile rc_filename $
             "1 24 MOVEABLE PURE " ++ show manifest_filename ++ "\n"
               -- magic numbers :-)
               -- show is a bit hackish above, but we need to escape the
               -- backslashes in the path.

         runWindres dflags $ map SysTools.Option $
               ["--input="++rc_filename,
                "--output="++rc_obj_filename,
                "--output-format=coff"]
               -- no FileOptions here: windres doesn't like seeing
               -- backslashes, apparently

         removeFile manifest_filename

         return [rc_obj_filename]
 | otherwise = return []


linkDynLibCheck :: DynFlags -> [String] -> [InstalledUnitId] -> IO ()
linkDynLibCheck dflags o_files dep_packages
 = do
    when (haveRtsOptsFlags dflags) $ do
      putLogMsg dflags NoReason SevInfo noSrcSpan
          (defaultUserStyle dflags)
          (text "Warning: -rtsopts and -with-rtsopts have no effect with -shared." $$
           text "    Call hs_init_ghc() from your main() function to set these options.")

    linkDynLib dflags o_files dep_packages

-- | Linking a static lib will not really link anything. It will merely produce
-- a static archive of all dependent static libraries. The resulting library
-- will still need to be linked with any remaining link flags.
linkStaticLib :: DynFlags -> [String] -> [InstalledUnitId] -> IO ()
linkStaticLib dflags o_files dep_packages = do
  let extra_ld_inputs = [ f | FileOption _ f <- ldInputs dflags ]
      modules = o_files ++ extra_ld_inputs
      output_fn = exeFileName True dflags

  full_output_fn <- if isAbsolute output_fn
                    then return output_fn
                    else do d <- getCurrentDirectory
                            return $ normalise (d </> output_fn)
  output_exists <- doesFileExist full_output_fn
  (when output_exists) $ removeFile full_output_fn

  pkg_cfgs <- getPreloadPackagesAnd dflags dep_packages
  archives <- concatMapM (collectArchives dflags) pkg_cfgs

  ar <- foldl mappend
        <$> (Archive <$> mapM loadObj modules)
        <*> mapM loadAr archives

  if toolSettings_ldIsGnuLd (toolSettings dflags)
    then writeGNUAr output_fn $ afilter (not . isGNUSymdef) ar
    else writeBSDAr output_fn $ afilter (not . isBSDSymdef) ar

  -- run ranlib over the archive. write*Ar does *not* create the symbol index.
  runRanlib dflags [SysTools.FileOption "" output_fn]

-- -----------------------------------------------------------------------------
-- Running CPP

doCpp :: DynFlags -> Bool -> FilePath -> FilePath -> IO ()
doCpp dflags raw input_fn output_fn = do
    let hscpp_opts = picPOpts dflags
    let cmdline_include_paths = includePaths dflags

    pkg_include_dirs <- getPackageIncludePath dflags []
    let include_paths_global = foldr (\ x xs -> ("-I" ++ x) : xs) []
          (includePathsGlobal cmdline_include_paths ++ pkg_include_dirs)
    let include_paths_quote = foldr (\ x xs -> ("-iquote" ++ x) : xs) []
          (includePathsQuote cmdline_include_paths)
    let include_paths = include_paths_quote ++ include_paths_global

    let verbFlags = getVerbFlags dflags

    let cpp_prog args | raw       = SysTools.runCpp dflags args
                      | otherwise = SysTools.runCc Nothing dflags (SysTools.Option "-E" : args)

    let targetArch = stringEncodeArch $ platformArch $ targetPlatform dflags
        targetOS = stringEncodeOS $ platformOS $ targetPlatform dflags
    let target_defs =
          [ "-D" ++ HOST_OS     ++ "_BUILD_OS",
            "-D" ++ HOST_ARCH   ++ "_BUILD_ARCH",
            "-D" ++ targetOS    ++ "_HOST_OS",
            "-D" ++ targetArch  ++ "_HOST_ARCH" ]
        -- remember, in code we *compile*, the HOST is the same our TARGET,
        -- and BUILD is the same as our HOST.

    let sse_defs =
          [ "-D__SSE__"      | isSseEnabled      dflags ] ++
          [ "-D__SSE2__"     | isSse2Enabled     dflags ] ++
          [ "-D__SSE4_2__"   | isSse4_2Enabled   dflags ]

    let avx_defs =
          [ "-D__AVX__"      | isAvxEnabled      dflags ] ++
          [ "-D__AVX2__"     | isAvx2Enabled     dflags ] ++
          [ "-D__AVX512CD__" | isAvx512cdEnabled dflags ] ++
          [ "-D__AVX512ER__" | isAvx512erEnabled dflags ] ++
          [ "-D__AVX512F__"  | isAvx512fEnabled  dflags ] ++
          [ "-D__AVX512PF__" | isAvx512pfEnabled dflags ]

    backend_defs <- getBackendDefs dflags

    let th_defs = [ "-D__GLASGOW_HASKELL_TH__" ]
    -- Default CPP defines in Haskell source
    ghcVersionH <- getGhcVersionPathName dflags
    let hsSourceCppOpts = [ "-include", ghcVersionH ]

    -- MIN_VERSION macros
    let uids = explicitPackages (pkgState dflags)
        pkgs = catMaybes (map (lookupUnit dflags) uids)
    mb_macro_include <-
        if not (null pkgs) && gopt Opt_VersionMacros dflags
            then do macro_stub <- newTempName dflags TFL_CurrentModule "h"
                    writeFile macro_stub (generatePackageVersionMacros pkgs)
                    -- Include version macros for every *exposed* package.
                    -- Without -hide-all-packages and with a package database
                    -- size of 1000 packages, it takes cpp an estimated 2
                    -- milliseconds to process this file. See #10970
                    -- comment 8.
                    return [SysTools.FileOption "-include" macro_stub]
            else return []

    cpp_prog       (   map SysTools.Option verbFlags
                    ++ map SysTools.Option include_paths
                    ++ map SysTools.Option hsSourceCppOpts
                    ++ map SysTools.Option target_defs
                    ++ map SysTools.Option backend_defs
                    ++ map SysTools.Option th_defs
                    ++ map SysTools.Option hscpp_opts
                    ++ map SysTools.Option sse_defs
                    ++ map SysTools.Option avx_defs
                    ++ mb_macro_include
        -- Set the language mode to assembler-with-cpp when preprocessing. This
        -- alleviates some of the C99 macro rules relating to whitespace and the hash
        -- operator, which we tend to abuse. Clang in particular is not very happy
        -- about this.
                    ++ [ SysTools.Option     "-x"
                       , SysTools.Option     "assembler-with-cpp"
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

getBackendDefs :: DynFlags -> IO [String]
getBackendDefs dflags | hscTarget dflags == HscLlvm = do
    llvmVer <- figureLlvmVersion dflags
    return $ case fmap llvmVersionList llvmVer of
               Just [m] -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (m,0) ]
               Just (m:n:_) -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (m,n) ]
               _ -> []
  where
    format (major, minor)
      | minor >= 100 = error "getBackendDefs: Unsupported minor version"
      | otherwise = show $ (100 * major + minor :: Int) -- Contract is Int

getBackendDefs _ =
    return []

-- ---------------------------------------------------------------------------
-- Macros (cribbed from Cabal)

generatePackageVersionMacros :: [UnitInfo] -> String
generatePackageVersionMacros pkgs = concat
  -- Do not add any C-style comments. See #3389.
  [ generateMacros "" pkgname version
  | pkg <- pkgs
  , let version = packageVersion pkg
        pkgname = map fixchar (packageNameString pkg)
  ]

fixchar :: Char -> Char
fixchar '-' = '_'
fixchar c   = c

generateMacros :: String -> String -> Version -> String
generateMacros prefix name version =
  concat
  ["#define ", prefix, "VERSION_",name," ",show (showVersion version),"\n"
  ,"#define MIN_", prefix, "VERSION_",name,"(major1,major2,minor) (\\\n"
  ,"  (major1) <  ",major1," || \\\n"
  ,"  (major1) == ",major1," && (major2) <  ",major2," || \\\n"
  ,"  (major1) == ",major1," && (major2) == ",major2," && (minor) <= ",minor,")"
  ,"\n\n"
  ]
  where
    (major1:major2:minor:_) = map show (versionBranch version ++ repeat 0)

-- ---------------------------------------------------------------------------
-- join object files into a single relocatable object file, using ld -r

{-
Note [Produce big objects on Windows]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Windows Portable Executable object format has a limit of 32k sections, which
we tend to blow through pretty easily. Thankfully, there is a "big object"
extension, which raises this limit to 2^32. However, it must be explicitly
enabled in the toolchain:

 * the assembler accepts the -mbig-obj flag, which causes it to produce a
   bigobj-enabled COFF object.

 * the linker accepts the --oformat pe-bigobj-x86-64 flag. Despite what the name
   suggests, this tells the linker to produce a bigobj-enabled COFF object, no a
   PE executable.

We must enable bigobj output in a few places:

 * When merging object files (GHC.Driver.Pipeline.joinObjectFiles)

 * When assembling (GHC.Driver.Pipeline.runPhase (RealPhase As ...))

Unfortunately the big object format is not supported on 32-bit targets so
none of this can be used in that case.
-}

joinObjectFiles :: DynFlags -> [FilePath] -> FilePath -> IO ()
joinObjectFiles dflags o_files output_fn = do
  let toolSettings' = toolSettings dflags
      ldIsGnuLd = toolSettings_ldIsGnuLd toolSettings'
      osInfo = platformOS (targetPlatform dflags)
      ld_r args cc = SysTools.runLink dflags ([
                       SysTools.Option "-nostdlib",
                       SysTools.Option "-Wl,-r"
                     ]
                        -- See Note [No PIE while linking] in GHC.Driver.Session
                     ++ (if toolSettings_ccSupportsNoPie toolSettings'
                          then [SysTools.Option "-no-pie"]
                          else [])

                     ++ (if any (cc ==) [Clang, AppleClang, AppleClang51]
                          then []
                          else [SysTools.Option "-nodefaultlibs"])
                     ++ (if osInfo == OSFreeBSD
                          then [SysTools.Option "-L/usr/lib"]
                          else [])
                        -- gcc on sparc sets -Wl,--relax implicitly, but
                        -- -r and --relax are incompatible for ld, so
                        -- disable --relax explicitly.
                     ++ (if platformArch (targetPlatform dflags)
                                `elem` [ArchSPARC, ArchSPARC64]
                         && ldIsGnuLd
                            then [SysTools.Option "-Wl,-no-relax"]
                            else [])
                        -- See Note [Produce big objects on Windows]
                     ++ [ SysTools.Option "-Wl,--oformat,pe-bigobj-x86-64"
                        | OSMinGW32 == osInfo
                        , not $ target32Bit (targetPlatform dflags)
                        ]
                     ++ map SysTools.Option ld_build_id
                     ++ [ SysTools.Option "-o",
                          SysTools.FileOption "" output_fn ]
                     ++ args)

      -- suppress the generation of the .note.gnu.build-id section,
      -- which we don't need and sometimes causes ld to emit a
      -- warning:
      ld_build_id | toolSettings_ldSupportsBuildId toolSettings' = ["-Wl,--build-id=none"]
                  | otherwise                     = []

  ccInfo <- getCompilerInfo dflags
  if ldIsGnuLd
     then do
          script <- newTempName dflags TFL_CurrentModule "ldscript"
          cwd <- getCurrentDirectory
          let o_files_abs = map (\x -> "\"" ++ (cwd </> x) ++ "\"") o_files
          writeFile script $ "INPUT(" ++ unwords o_files_abs ++ ")"
          ld_r [SysTools.FileOption "" script] ccInfo
     else if toolSettings_ldSupportsFilelist toolSettings'
     then do
          filelist <- newTempName dflags TFL_CurrentModule "filelist"
          writeFile filelist $ unlines o_files
          ld_r [SysTools.Option "-Wl,-filelist",
                SysTools.FileOption "-Wl," filelist] ccInfo
     else do
          ld_r (map (SysTools.FileOption "") o_files) ccInfo

-- -----------------------------------------------------------------------------
-- Misc.

writeInterfaceOnlyMode :: DynFlags -> Bool
writeInterfaceOnlyMode dflags =
 gopt Opt_WriteInterface dflags &&
 HscNothing == hscTarget dflags

-- | Figure out if a source file was modified after an output file (or if we
-- anyways need to consider the source file modified since the output is gone).
sourceModified :: FilePath -- ^ destination file we are looking for
               -> UTCTime  -- ^ last time of modification of source file
               -> IO Bool  -- ^ do we need to regenerate the output?
sourceModified dest_file src_timestamp = do
  dest_file_exists <- doesFileExist dest_file
  if not dest_file_exists
    then return True       -- Need to recompile
     else do t2 <- getModificationUTCTime dest_file
             return (t2 <= src_timestamp)

-- | What phase to run after one of the backend code generators has run
hscPostBackendPhase :: HscSource -> HscTarget -> Phase
hscPostBackendPhase HsBootFile _    =  StopLn
hscPostBackendPhase HsigFile _      =  StopLn
hscPostBackendPhase _ hsc_lang =
  case hsc_lang of
        HscC           -> HCc
        HscAsm         -> As False
        HscLlvm        -> LlvmOpt
        HscNothing     -> StopLn
        HscInterpreted -> StopLn

touchObjectFile :: DynFlags -> FilePath -> IO ()
touchObjectFile dflags path = do
  createDirectoryIfMissing True $ takeDirectory path
  SysTools.touch dflags "Touching object file" path

-- | Find out path to @ghcversion.h@ file
getGhcVersionPathName :: DynFlags -> IO FilePath
getGhcVersionPathName dflags = do
  candidates <- case ghcVersionFile dflags of
    Just path -> return [path]
    Nothing -> (map (</> "ghcversion.h")) <$>
               (getPackageIncludePath dflags [toInstalledUnitId rtsUnitId])

  found <- filterM doesFileExist candidates
  case found of
      []    -> throwGhcExceptionIO (InstallationError
                                    ("ghcversion.h missing; tried: "
                                      ++ intercalate ", " candidates))
      (x:_) -> return x

-- Note [-fPIC for assembler]
-- When compiling .c source file GHC's driver pipeline basically
-- does the following two things:
--   1. ${CC}              -S 'PIC_CFLAGS' source.c
--   2. ${CC} -x assembler -c 'PIC_CFLAGS' source.S
--
-- Why do we need to pass 'PIC_CFLAGS' both to C compiler and assembler?
-- Because on some architectures (at least sparc32) assembler also chooses
-- the relocation type!
-- Consider the following C module:
--
--     /* pic-sample.c */
--     int v;
--     void set_v (int n) { v = n; }
--     int  get_v (void)  { return v; }
--
--     $ gcc -S -fPIC pic-sample.c
--     $ gcc -c       pic-sample.s -o pic-sample.no-pic.o # incorrect binary
--     $ gcc -c -fPIC pic-sample.s -o pic-sample.pic.o    # correct binary
--
--     $ objdump -r -d pic-sample.pic.o    > pic-sample.pic.o.od
--     $ objdump -r -d pic-sample.no-pic.o > pic-sample.no-pic.o.od
--     $ diff -u pic-sample.pic.o.od pic-sample.no-pic.o.od
--
-- Most of architectures won't show any difference in this test, but on sparc32
-- the following assembly snippet:
--
--    sethi   %hi(_GLOBAL_OFFSET_TABLE_-8), %l7
--
-- generates two kinds or relocations, only 'R_SPARC_PC22' is correct:
--
--       3c:  2f 00 00 00     sethi  %hi(0), %l7
--    -                       3c: R_SPARC_PC22        _GLOBAL_OFFSET_TABLE_-0x8
--    +                       3c: R_SPARC_HI22        _GLOBAL_OFFSET_TABLE_-0x8

{- Note [Don't normalise input filenames]

Summary
  We used to normalise input filenames when starting the unlit phase. This
  broke hpc in `--make` mode with imported literate modules (#2991).

Introduction
  1) --main
  When compiling a module with --main, GHC scans its imports to find out which
  other modules it needs to compile too. It turns out that there is a small
  difference between saying `ghc --make A.hs`, when `A` imports `B`, and
  specifying both modules on the command line with `ghc --make A.hs B.hs`. In
  the former case, the filename for B is inferred to be './B.hs' instead of
  'B.hs'.

  2) unlit
  When GHC compiles a literate haskell file, the source code first needs to go
  through unlit, which turns it into normal Haskell source code. At the start
  of the unlit phase, in `Driver.Pipeline.runPhase`, we call unlit with the
  option `-h` and the name of the original file. We used to normalise this
  filename using System.FilePath.normalise, which among other things removes
  an initial './'. unlit then uses that filename in #line directives that it
  inserts in the transformed source code.

  3) SrcSpan
  A SrcSpan represents a portion of a source code file. It has fields
  linenumber, start column, end column, and also a reference to the file it
  originated from. The SrcSpans for a literate haskell file refer to the
  filename that was passed to unlit -h.

  4) -fhpc
  At some point during compilation with -fhpc, in the function
  `GHC.HsToCore.Coverage.isGoodTickSrcSpan`, we compare the filename that a
  `SrcSpan` refers to with the name of the file we are currently compiling.
  For some reason I don't yet understand, they can sometimes legitimally be
  different, and then hpc ignores that SrcSpan.

Problem
  When running `ghc --make -fhpc A.hs`, where `A.hs` imports the literate
  module `B.lhs`, `B` is inferred to be in the file `./B.lhs` (1). At the
  start of the unlit phase, the name `./B.lhs` is normalised to `B.lhs` (2).
  Therefore the SrcSpans of `B` refer to the file `B.lhs` (3), but we are
  still compiling `./B.lhs`. Hpc thinks these two filenames are different (4),
  doesn't include ticks for B, and we have unhappy customers (#2991).

Solution
  Do not normalise `input_fn` when starting the unlit phase.

Alternative solution
  Another option would be to not compare the two filenames on equality, but to
  use System.FilePath.equalFilePath. That function first normalises its
  arguments. The problem is that by the time we need to do the comparison, the
  filenames have been turned into FastStrings, probably for performance
  reasons, so System.FilePath.equalFilePath can not be used directly.

Archeology
  The call to `normalise` was added in a commit called "Fix slash
  direction on Windows with the new filePath code" (c9b6b5e8). The problem
  that commit was addressing has since been solved in a different manner, in a
  commit called "Fix the filename passed to unlit" (1eedbc6b). So the
  `normalise` is no longer necessary.
-}
