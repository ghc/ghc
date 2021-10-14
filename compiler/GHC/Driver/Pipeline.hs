{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

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

        -- Interfaces for the compilation manager (interpreted/batch-mode)
   preprocess,
   compileOne, compileOne',
   link,

        -- Exports for hooks to override runPhase and link
   PhasePlus(..), CompPipeline(..), PipeEnv(..), PipeState(..),
   phaseOutputFilename, getOutputFilename, getPipeState, getPipeEnv,
   hscPostBackendPhase, getLocation, setModLocation, setDynFlags,
   runPhase,
   doCpp,
   linkingNeeded, checkLinkInfo, writeInterfaceOnlyMode
  ) where

#include <ghcplatform.h>
#include "HsVersions.h"

import GHC.Prelude

import GHC.Platform

import GHC.Tc.Types

import GHC.Driver.Main
import GHC.Driver.Env hiding ( Hsc )
import GHC.Driver.Errors
import GHC.Driver.Pipeline.Monad
import GHC.Driver.Config
import GHC.Driver.Phases
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Ppr
import GHC.Driver.Hooks

import GHC.Platform.Ways
import GHC.Platform.ArchOS

import GHC.Parser.Header
import GHC.Parser.Errors.Ppr

import GHC.SysTools
import GHC.Utils.TmpFs

import GHC.Linker.ExtraObj
import GHC.Linker.Dynamic
import GHC.Linker.Static
import GHC.Linker.Types

import GHC.Utils.Outputable
import GHC.Utils.Error
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Monad
import GHC.Utils.Exception as Exception
import GHC.Utils.Logger

import GHC.CmmToLlvm         ( llvmFixupAsm, llvmVersionList )
import qualified GHC.LanguageExtensions as LangExt
import GHC.Settings

import GHC.Data.Bag            ( unitBag )
import GHC.Data.FastString     ( mkFastString )
import GHC.Data.StringBuffer   ( hGetStringBuffer, hPutStringBuffer )
import GHC.Data.Maybe          ( expectJust )

import GHC.Iface.Make          ( mkFullIface )

import GHC.Types.Basic       ( SuccessFlag(..) )
import GHC.Types.Target
import GHC.Types.SrcLoc
import GHC.Types.SourceFile
import GHC.Types.SourceError

import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.State
import GHC.Unit.Finder
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Graph (needsTemplateHaskellOrQQ)
import GHC.Unit.Module.Deps
import GHC.Unit.Home.ModInfo

import System.Directory
import System.FilePath
import System.IO
import Control.Monad
import qualified Control.Monad.Catch as MC (handle)
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
  MC.handle handler $
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
        mkPlainMsgEnvelope srcspan $ text msg
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

   let logger = hsc_logger hsc_env0
   let tmpfs  = hsc_tmpfs hsc_env0
   debugTraceMsg logger dflags1 2 (text "compile: input file" <+> text input_fnpp)

   -- Run the pipeline up to codeGen (so everything up to, but not including, STG)
   (status, plugin_hsc_env) <- hscIncrementalCompile
                        always_do_basic_recompilation_check
                        m_tc_result mHscMessage
                        hsc_env summary source_modified mb_old_iface (mod_index, nmods)
   -- Use an HscEnv updated with the plugin info
   let hsc_env' = plugin_hsc_env

   let flags = hsc_dflags hsc_env0
     in do unless (gopt Opt_KeepHiFiles flags) $
               addFilesToClean tmpfs TFL_CurrentModule $
                   [ml_hi_file $ ms_location summary]
           unless (gopt Opt_KeepOFiles flags) $
               addFilesToClean tmpfs TFL_GhcSession $
                   [ml_obj_file $ ms_location summary]

   case (status, bcknd) of
        (HscUpToDate iface hmi_details, _) ->
            -- TODO recomp014 triggers this assert. What's going on?!
            -- ASSERT( isJust mb_old_linkable || isNoLink (ghcLink dflags) )
            return $! HomeModInfo iface hmi_details mb_old_linkable
        (HscNotGeneratingCode iface hmi_details, NoBackend) ->
            let mb_linkable = if isHsBootOrSig src_flavour
                                then Nothing
                                -- TODO: Questionable.
                                else Just (LM (ms_hs_date summary) this_mod [])
            in return $! HomeModInfo iface hmi_details mb_linkable
        (HscNotGeneratingCode _ _, _) -> panic "compileOne HscNotGeneratingCode"
        (_, NoBackend) -> panic "compileOne NoBackend"
        (HscUpdateBoot iface hmi_details, Interpreter) ->
            return $! HomeModInfo iface hmi_details Nothing
        (HscUpdateBoot iface hmi_details, _) -> do
            touchObjectFile logger dflags object_filename
            return $! HomeModInfo iface hmi_details Nothing
        (HscUpdateSig iface hmi_details, Interpreter) -> do
            let !linkable = LM (ms_hs_date summary) this_mod []
            return $! HomeModInfo iface hmi_details (Just linkable)
        (HscUpdateSig iface hmi_details, _) -> do
            output_fn <- getOutputFilename logger tmpfs next_phase
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
                     hscs_partial_iface = partial_iface,
                     hscs_old_iface_hash = mb_old_iface_hash
                   }, Interpreter) -> do
            -- In interpreted mode the regular codeGen backend is not run so we
            -- generate a interface without codeGen info.
            final_iface <- mkFullIface hsc_env' partial_iface Nothing
            -- Reconstruct the `ModDetails` from the just-constructed `ModIface`
            -- See Note [ModDetails and --make mode]
            hmi_details <- liftIO $ initModDetails hsc_env' summary final_iface
            liftIO $ hscMaybeWriteIface logger dflags True final_iface mb_old_iface_hash (ms_location summary)

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
            output_fn <- getOutputFilename logger tmpfs next_phase
                            (Temporary TFL_CurrentModule)
                            basename dflags next_phase (Just location)
            -- We're in --make mode: finish the compilation pipeline.
            (_, _, Just iface) <- runPipeline StopLn hsc_env'
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
            -- See Note [ModDetails and --make mode]
            details <- initModDetails hsc_env' summary iface
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
       next_phase = hscPostBackendPhase src_flavour bcknd
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
       loadAsByteCode
         | Just (Target _ obj _) <- findTarget summary (hsc_targets hsc_env0)
         , not obj
         = True
         | otherwise = False
       -- Figure out which backend we're using
       (bcknd, dflags3)
         -- #8042: When module was loaded with `*` prefix in ghci, but DynFlags
         -- suggest to generate object code (which may happen in case -fobject-code
         -- was set), force it to generate byte-code. This is NOT transitive and
         -- only applies to direct targets.
         | loadAsByteCode
         = (Interpreter, dflags2 { backend = Interpreter })
         | otherwise
         = (backend dflags, dflags2)
       dflags  = dflags3 { includePaths = addImplicitQuoteInclude old_paths [current_dir] }
       hsc_env = hsc_env0 {hsc_dflags = dflags}

       -- -fforce-recomp should also work with --make
       force_recomp = gopt Opt_ForceRecomp dflags
       source_modified
         -- #8042: Usually pre-compiled code is preferred to be loaded in ghci
         -- if available. So, if the "*" prefix was used, force recompilation
         -- to make sure byte-code is loaded.
         | force_recomp || loadAsByteCode = SourceModified
         | otherwise = source_modified0

       always_do_basic_recompilation_check = case bcknd of
                                             Interpreter -> True
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
  let logger = hsc_logger hsc_env
  let tmpfs  = hsc_tmpfs hsc_env
  empty_stub <- newTempName logger tmpfs dflags TFL_CurrentModule "c"
  let home_unit = hsc_home_unit hsc_env
      src = text "int" <+> ppr (mkHomeModule home_unit mod_name) <+> text "= 0;"
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
--
-- Note [Dynamic linking on macOS]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Since macOS Sierra (10.14), the dynamic system linker enforces
-- a limit on the Load Commands.  Specifically the Load Command Size
-- Limit is at 32K (32768).  The Load Commands contain the install
-- name, dependencies, runpaths, and a few other commands.  We however
-- only have control over the install name, dependencies and runpaths.
--
-- The install name is the name by which this library will be
-- referenced.  This is such that we do not need to bake in the full
-- absolute location of the library, and can move the library around.
--
-- The dependency commands contain the install names from of referenced
-- libraries.  Thus if a libraries install name is @rpath/libHS...dylib,
-- that will end up as the dependency.
--
-- Finally we have the runpaths, which informs the linker about the
-- directories to search for the referenced dependencies.
--
-- The system linker can do recursive linking, however using only the
-- direct dependencies conflicts with ghc's ability to inline across
-- packages, and as such would end up with unresolved symbols.
--
-- Thus we will pass the full dependency closure to the linker, and then
-- ask the linker to remove any unused dynamic libraries (-dead_strip_dylibs).
--
-- We still need to add the relevant runpaths, for the dynamic linker to
-- lookup the referenced libraries though.  The linker (ld64) does not
-- have any option to dead strip runpaths; which makes sense as runpaths
-- can be used for dependencies of dependencies as well.
--
-- The solution we then take in GHC is to not pass any runpaths to the
-- linker at link time, but inject them after the linking.  For this to
-- work we'll need to ask the linker to create enough space in the header
-- to add more runpaths after the linking (-headerpad 8000).
--
-- After the library has been linked by $LD (usually ld64), we will use
-- otool to inspect the libraries left over after dead stripping, compute
-- the relevant runpaths, and inject them into the linked product using
-- the install_name_tool command.
--
-- This strategy should produce the smallest possible set of load commands
-- while still retaining some form of relocatability via runpaths.
--
-- The only way I can see to reduce the load command size further would be
-- by shortening the library names, or start putting libraries into the same
-- folders, such that one runpath would be sufficient for multiple/all
-- libraries.
link :: GhcLink                 -- ^ interactive or batch
     -> Logger                  -- ^ Logger
     -> TmpFs
     -> Hooks
     -> DynFlags                -- ^ dynamic flags
     -> UnitEnv                 -- ^ unit environment
     -> Bool                    -- ^ attempt linking in batch mode?
     -> HomePackageTable        -- ^ what to link
     -> IO SuccessFlag

-- For the moment, in the batch linker, we don't bother to tell doLink
-- which packages to link -- it just tries all that are available.
-- batch_attempt_linking should only be *looked at* in batch mode.  It
-- should only be True if the upsweep was successful and someone
-- exports main, i.e., we have good reason to believe that linking
-- will succeed.

link ghcLink logger tmpfs hooks dflags unit_env batch_attempt_linking hpt =
  case linkHook hooks of
      Nothing -> case ghcLink of
          NoLink        -> return Succeeded
          LinkBinary    -> link' logger tmpfs dflags unit_env batch_attempt_linking hpt
          LinkStaticLib -> link' logger tmpfs dflags unit_env batch_attempt_linking hpt
          LinkDynLib    -> link' logger tmpfs dflags unit_env batch_attempt_linking hpt
          LinkInMemory
              | platformMisc_ghcWithInterpreter $ platformMisc dflags
              -> -- Not Linking...(demand linker will do the job)
                 return Succeeded
              | otherwise
              -> panicBadLink LinkInMemory
      Just h  -> h ghcLink dflags batch_attempt_linking hpt


panicBadLink :: GhcLink -> a
panicBadLink other = panic ("link: GHC not built to link this way: " ++
                            show other)

link' :: Logger
      -> TmpFs
      -> DynFlags                -- ^ dynamic flags
      -> UnitEnv                 -- ^ unit environment
      -> Bool                    -- ^ attempt linking in batch mode?
      -> HomePackageTable        -- ^ what to link
      -> IO SuccessFlag

link' logger tmpfs dflags unit_env batch_attempt_linking hpt
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

        debugTraceMsg logger dflags 3 (text "link: linkables are ..." $$ vcat (map ppr linkables))

        -- check for the -no-link flag
        if isNoLink (ghcLink dflags)
          then do debugTraceMsg logger dflags 3 (text "link(batch): linking omitted (-c flag given).")
                  return Succeeded
          else do

        let getOfiles (LM _ _ us) = map nameOfObject (filter isObject us)
            obj_files = concatMap getOfiles linkables
            platform  = targetPlatform dflags
            exe_file  = exeFileName platform staticLink (outputFile dflags)

        linking_needed <- linkingNeeded logger dflags unit_env staticLink linkables pkg_deps

        if not (gopt Opt_ForceRecomp dflags) && not linking_needed
           then do debugTraceMsg logger dflags 2 (text exe_file <+> text "is up to date, linking not required.")
                   return Succeeded
           else do

        compilationProgressMsg logger dflags (text "Linking " <> text exe_file <> text " ...")

        -- Don't showPass in Batch mode; doLink will do that for us.
        let link = case ghcLink dflags of
                LinkBinary    -> linkBinary logger tmpfs
                LinkStaticLib -> linkStaticLib logger
                LinkDynLib    -> linkDynLibCheck logger tmpfs
                other         -> panicBadLink other
        link dflags unit_env obj_files pkg_deps

        debugTraceMsg logger dflags 3 (text "link: done")

        -- linkBinary only returns if it succeeds
        return Succeeded

   | otherwise
   = do debugTraceMsg logger dflags 3 (text "link(batch): upsweep (partially) failed OR" $$
                                text "   Main.main not exported; not linking.")
        return Succeeded


linkingNeeded :: Logger -> DynFlags -> UnitEnv -> Bool -> [Linkable] -> [UnitId] -> IO Bool
linkingNeeded logger dflags unit_env staticLink linkables pkg_deps = do
        -- if the modification time on the executable is later than the
        -- modification times on all of the objects and libraries, then omit
        -- linking (unless the -fforce-recomp flag was given).
  let platform   = ue_platform unit_env
      unit_state = ue_units unit_env
      exe_file   = exeFileName platform staticLink (outputFile dflags)
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
        let pkg_hslibs  = [ (collectLibraryDirs (ways dflags) [c], lib)
                          | Just c <- map (lookupUnitId unit_state) pkg_deps,
                            lib <- unitHsLibs (ghcNameVersion dflags) (ways dflags) c ]

        pkg_libfiles <- mapM (uncurry (findHSLib platform (ways dflags))) pkg_hslibs
        if any isNothing pkg_libfiles then return True else do
        e_lib_times <- mapM (tryIO . getModificationUTCTime)
                          (catMaybes pkg_libfiles)
        let (lib_errs,lib_times) = partitionEithers e_lib_times
        if not (null lib_errs) || any (t <) lib_times
           then return True
           else checkLinkInfo logger dflags unit_env pkg_deps exe_file

findHSLib :: Platform -> Ways -> [String] -> String -> IO (Maybe FilePath)
findHSLib platform ws dirs lib = do
  let batch_lib_file = if WayDyn `notElem` ws
                      then "lib" ++ lib <.> "a"
                      else platformSOName platform lib
  found <- filterM doesFileExist (map (</> batch_lib_file) dirs)
  case found of
    [] -> return Nothing
    (x:_) -> return (Just x)

-- -----------------------------------------------------------------------------
-- Compile files in one-shot mode.

oneShot :: HscEnv -> Phase -> [(String, Maybe Phase)] -> IO ()
oneShot hsc_env stop_phase srcs = do
  o_files <- mapM (compileFile hsc_env stop_phase) srcs
  doLink hsc_env stop_phase o_files

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
         | NoBackend <- backend dflags = Temporary TFL_CurrentModule
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


doLink :: HscEnv -> Phase -> [FilePath] -> IO ()
doLink hsc_env stop_phase o_files
  | not (isStopLn stop_phase)
  = return ()           -- We stopped before the linking phase

  | otherwise
  = let
        dflags   = hsc_dflags   hsc_env
        logger   = hsc_logger   hsc_env
        unit_env = hsc_unit_env hsc_env
        tmpfs    = hsc_tmpfs    hsc_env
    in case ghcLink dflags of
        NoLink        -> return ()
        LinkBinary    -> linkBinary         logger tmpfs dflags unit_env o_files []
        LinkStaticLib -> linkStaticLib      logger       dflags unit_env o_files []
        LinkDynLib    -> linkDynLibCheck    logger tmpfs dflags unit_env o_files []
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
  -> IO (DynFlags, FilePath, Maybe ModIface)
                                -- ^ (final flags, output filename, interface)
runPipeline stop_phase hsc_env0 (input_fn, mb_input_buf, mb_phase)
             mb_basename output maybe_loc foreign_os

    = do let
             dflags0 = hsc_dflags hsc_env0

             -- Decide where dump files should go based on the pipeline output
             dflags = dflags0 { dumpPrefix = Just (basename ++ ".") }
             hsc_env = hsc_env0 {hsc_dflags = dflags}
             logger = hsc_logger hsc_env
             tmpfs  = hsc_tmpfs  hsc_env

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
                 fn <- newTempName logger tmpfs dflags TFL_CurrentModule suffix
                 hdl <- openBinaryFile fn WriteMode
                 -- Add a LINE pragma so reported source locations will
                 -- mention the real input file, not this temp file.
                 hPutStrLn hdl $ "{-# LINE 1 \""++ input_fn ++ "\"#-}"
                 hPutStringBuffer hdl input_buf
                 hClose hdl
                 return fn
             (_, _) -> return input_fn

         debugTraceMsg logger dflags 4 (text "Running the pipeline")
         r <- runPipeline' start_phase hsc_env env input_fn'
                           maybe_loc foreign_os

         let dflags = hsc_dflags hsc_env
         when isHaskellishFile $
           dynamicTooState dflags >>= \case
               DT_Dont   -> return ()
               DT_Dyn    -> return ()
               DT_OK     -> return ()
               -- If we are compiling a Haskell module with -dynamic-too, we
               -- first try the "fast path": that is we compile the non-dynamic
               -- version and at the same time we check that interfaces depended
               -- on exist both for the non-dynamic AND the dynamic way. We also
               -- check that they have the same hash.
               --    If they don't, dynamicTooState is set to DT_Failed.
               --       See GHC.Iface.Load.checkBuildDynamicToo
               --    If they do, in the end we produce both the non-dynamic and
               --    dynamic outputs.
               --
               -- If this "fast path" failed, we execute the whole pipeline
               -- again, this time for the dynamic way *only*. To do that we
               -- just set the dynamicNow bit from the start to ensure that the
               -- dynamic DynFlags fields are used and we disable -dynamic-too
               -- (its state is already set to DT_Failed so it wouldn't do much
               -- anyway).
               DT_Failed
                   -- NB: Currently disabled on Windows (ref #7134, #8228, and #5987)
                   | OSMinGW32 <- platformOS (targetPlatform dflags) -> return ()
                   | otherwise -> do
                       debugTraceMsg logger dflags 4
                           (text "Running the full pipeline again for -dynamic-too")
                       let dflags0 = flip gopt_unset Opt_BuildDynamicToo
                                      $ setDynamicNow
                                      $ dflags
                       hsc_env' <- newHscEnv dflags0
                       (dbs,unit_state,home_unit,mconstants) <- initUnits logger dflags0 Nothing
                       dflags1 <- updatePlatformConstants dflags0 mconstants
                       let unit_env = UnitEnv
                             { ue_platform  = targetPlatform dflags1
                             , ue_namever   = ghcNameVersion dflags1
                             , ue_home_unit = home_unit
                             , ue_units     = unit_state
                             }
                       let hsc_env'' = hsc_env'
                            { hsc_dflags   = dflags1
                            , hsc_unit_env = unit_env
                            , hsc_unit_dbs = Just dbs
                            }
                       _ <- runPipeline' start_phase hsc_env'' env input_fn'
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
  -> IO (DynFlags, FilePath, Maybe ModIface)
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
  logger <- getLogger
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
               tmpfs <- hsc_tmpfs <$> getPipeSession
               final_fn <- liftIO $ getOutputFilename logger tmpfs
                                        stopPhase output (src_basename env)
                                        dflags stopPhase (maybe_loc pst)
               when (final_fn /= input_fn) $ do
                  let msg = ("Copying `" ++ input_fn ++"' to `" ++ final_fn ++ "'")
                      line_prag = Just ("{-# LINE 1 \"" ++ src_filename env ++ "\" #-}\n")
                  liftIO $ copyWithHeader logger dflags msg line_prag input_fn final_fn
               return final_fn


     | not (realPhase `happensBefore'` stopPhase)
        -- Something has gone wrong.  We'll try to cover all the cases when
        -- this could happen, so if we reach here it is a panic.
        -- eg. it might happen if the -C flag is used on a source file that
        -- has {-# OPTIONS -fasm #-}.
     -> panic ("pipeLoop: at phase " ++ show realPhase ++
           " but I wanted to stop at phase " ++ show stopPhase)

   _
     -> do liftIO $ debugTraceMsg logger dflags 4
                                  (text "Running phase" <+> ppr phase)

           case phase of
               HscOut {} -> do
                   -- Depending on the dynamic-too state, we first run the
                   -- backend to generate the non-dynamic objects and then
                   -- re-run it to generate the dynamic ones.
                   let noDynToo = do
                        (next_phase, output_fn) <- runHookedPhase phase input_fn
                        pipeLoop next_phase output_fn
                   let dynToo = do
                          -- we must run the non-dynamic way before the dynamic
                          -- one because there may be interfaces loaded only in
                          -- the backend (e.g., in CorePrep). See #19264
                          r <- noDynToo

                          -- we must check the dynamic-too state again, because
                          -- we may have failed to load a dynamic interface in
                          -- the backend.
                          dynamicTooState dflags >>= \case
                            DT_OK -> do
                                let dflags' = setDynamicNow dflags -- set "dynamicNow"
                                setDynFlags dflags'
                                (next_phase, output_fn) <- runHookedPhase phase input_fn
                                _ <- pipeLoop next_phase output_fn
                                -- TODO: we probably shouldn't ignore the result of
                                -- the dynamic compilation
                                setDynFlags dflags -- restore flags without "dynamicNow" set
                                return r
                            _ -> return r

                   dynamicTooState dflags >>= \case
                     DT_Dont   -> noDynToo
                     DT_Failed -> noDynToo
                     DT_OK     -> dynToo
                     DT_Dyn    -> noDynToo
                        -- it shouldn't be possible to be in this last case
                        -- here. It would mean that we executed the whole
                        -- pipeline with DynamicNow and Opt_BuildDynamicToo set.
                        --
                        -- When we restart the whole pipeline for -dynamic-too
                        -- we set DynamicNow but we unset Opt_BuildDynamicToo so
                        -- it's weird.
               _ -> do
                  (next_phase, output_fn) <- runHookedPhase phase input_fn
                  pipeLoop next_phase output_fn

runHookedPhase :: PhasePlus -> FilePath -> CompPipeline (PhasePlus, FilePath)
runHookedPhase pp input = do
  hooks <- hsc_hooks <$> getPipeSession
  case runPhaseHook hooks of
    Nothing -> runPhase pp input
    Just h  -> h pp input

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
  PipeState{maybe_loc,hsc_env} <- getPipeState
  dflags <- getDynFlags
  logger <- getLogger
  let tmpfs = hsc_tmpfs hsc_env
  liftIO $ getOutputFilename logger tmpfs stop_phase output_spec
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
  :: Logger
  -> TmpFs
  -> Phase
  -> PipelineOutput
  -> String
  -> DynFlags
  -> Phase -- next phase
  -> Maybe ModLocation
  -> IO FilePath
getOutputFilename logger tmpfs stop_phase output basename dflags next_phase maybe_location
 | is_last_phase, Persistent   <- output = persistent_fn
 | is_last_phase, SpecificFile <- output = case outputFile dflags of
                                           Just f -> return f
                                           Nothing ->
                                               panic "SpecificFile: No filename"
 | keep_this_output                      = persistent_fn
 | Temporary lifetime <- output          = newTempName logger tmpfs dflags lifetime suffix
 | otherwise                             = newTempName logger tmpfs dflags TFL_CurrentModule
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

    -- Additional llc flags
    ++ [("", "-mcpu=" ++ mcpu)   | not (null mcpu)
                                 , not (any (isInfixOf "-mcpu") (getOpts dflags opt_lc)) ]
    ++ [("", "-mattr=" ++ attrs) | not (null attrs) ]
    ++ [("", "-target-abi=" ++ abi) | not (null abi) ]

  where target = platformMisc_llvmTarget $ platformMisc dflags
        Just (LlvmTarget _ mcpu mattr) = lookup target (llvmTargets $ llvmConfig dflags)

        -- Relocation models
        rmodel | gopt Opt_PIC dflags        = "pic"
               | positionIndependent dflags = "pic"
               | WayDyn `elem` ways dflags  = "dynamic-no-pic"
               | otherwise                  = "static"

        platform = targetPlatform dflags

        align :: Int
        align = case platformArch platform of
                  ArchX86_64 | isAvxEnabled dflags -> 32
                  _                                -> 0

        attrs :: String
        attrs = intercalate "," $ mattr
              ++ ["+sse42"   | isSse4_2Enabled dflags   ]
              ++ ["+sse2"    | isSse2Enabled platform   ]
              ++ ["+sse"     | isSseEnabled platform    ]
              ++ ["+avx512f" | isAvx512fEnabled dflags  ]
              ++ ["+avx2"    | isAvx2Enabled dflags     ]
              ++ ["+avx"     | isAvxEnabled dflags      ]
              ++ ["+avx512cd"| isAvx512cdEnabled dflags ]
              ++ ["+avx512er"| isAvx512erEnabled dflags ]
              ++ ["+avx512pf"| isAvx512pfEnabled dflags ]
              ++ ["+bmi"     | isBmiEnabled dflags      ]
              ++ ["+bmi2"    | isBmi2Enabled dflags     ]

        abi :: String
        abi = case platformArch (targetPlatform dflags) of
                ArchRISCV64 -> "lp64d"
                _           -> ""

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
         -> CompPipeline (PhasePlus,           -- next phase to run
                          FilePath)            -- output filename

        -- Invariant: the output filename always contains the output
        -- Interesting case: Hsc when there is no recompilation to do
        --                   Then the output filename is still a .o file


-------------------------------------------------------------------------------
-- Unlit phase

runPhase (RealPhase (Unlit sf)) input_fn = do
    let
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

    output_fn <- phaseOutputFilename (Cpp sf)

    let flags = [ -- The -h option passes the file name for unlit to
                  -- put in a #line directive
                  GHC.SysTools.Option     "-h"
                  -- See Note [Don't normalise input filenames].
                , GHC.SysTools.Option $ escape input_fn
                , GHC.SysTools.FileOption "" input_fn
                , GHC.SysTools.FileOption "" output_fn
                ]

    dflags <- getDynFlags
    logger <- getLogger
    liftIO $ GHC.SysTools.runUnlit logger dflags flags

    return (RealPhase (Cpp sf), output_fn)

-------------------------------------------------------------------------------
-- Cpp phase : (a) gets OPTIONS out of file
--             (b) runs cpp if necessary

runPhase (RealPhase (Cpp sf)) input_fn
  = do
       dflags0 <- getDynFlags
       logger <- getLogger
       src_opts <- liftIO $ getOptionsFromFile dflags0 input_fn
       (dflags1, unhandled_flags, warns)
           <- liftIO $ parseDynamicFilePragma dflags0 src_opts
       setDynFlags dflags1
       liftIO $ checkProcessArgsResult unhandled_flags

       if not (xopt LangExt.Cpp dflags1) then do
           -- we have to be careful to emit warnings only once.
           unless (gopt Opt_Pp dflags1) $
               liftIO $ handleFlagWarnings logger dflags1 warns

           -- no need to preprocess CPP, just pass input file along
           -- to the next phase of the pipeline.
           return (RealPhase (HsPp sf), input_fn)
        else do
            output_fn <- phaseOutputFilename (HsPp sf)
            hsc_env <- getPipeSession
            liftIO $ doCpp logger
                           (hsc_tmpfs hsc_env)
                           (hsc_dflags hsc_env)
                           (hsc_unit_env hsc_env)
                           True{-raw-}
                           input_fn output_fn
            -- re-read the pragmas now that we've preprocessed the file
            -- See #2464,#3457
            src_opts <- liftIO $ getOptionsFromFile dflags0 output_fn
            (dflags2, unhandled_flags, warns)
                <- liftIO $ parseDynamicFilePragma dflags0 src_opts
            liftIO $ checkProcessArgsResult unhandled_flags
            unless (gopt Opt_Pp dflags2) $
                liftIO $ handleFlagWarnings logger dflags2 warns
            -- the HsPp pass below will emit warnings

            setDynFlags dflags2

            return (RealPhase (HsPp sf), output_fn)

-------------------------------------------------------------------------------
-- HsPp phase

runPhase (RealPhase (HsPp sf)) input_fn = do
    dflags <- getDynFlags
    logger <- getLogger
    if not (gopt Opt_Pp dflags) then
      -- no need to preprocess, just pass input file along
      -- to the next phase of the pipeline.
       return (RealPhase (Hsc sf), input_fn)
    else do
        PipeEnv{src_basename, src_suffix} <- getPipeEnv
        let orig_fn = src_basename <.> src_suffix
        output_fn <- phaseOutputFilename (Hsc sf)
        liftIO $ GHC.SysTools.runPp logger dflags
                       ( [ GHC.SysTools.Option     orig_fn
                         , GHC.SysTools.Option     input_fn
                         , GHC.SysTools.FileOption "" output_fn
                         ]
                       )

        -- re-read pragmas now that we've parsed the file (see #3674)
        src_opts <- liftIO $ getOptionsFromFile dflags output_fn
        (dflags1, unhandled_flags, warns)
            <- liftIO $ parseDynamicFilePragma dflags src_opts
        setDynFlags dflags1
        liftIO $ checkProcessArgsResult unhandled_flags
        liftIO $ handleFlagWarnings logger dflags1 warns

        return (RealPhase (Hsc sf), output_fn)

-----------------------------------------------------------------------------
-- Hsc phase

-- Compilation of a single module, in "legacy" mode (_not_ under
-- the direction of the compilation manager).
runPhase (RealPhase (Hsc src_flavour)) input_fn
 = do   -- normal Hsc mode, not mkdependHS
        dflags0 <- getDynFlags

        PipeEnv{ stop_phase=stop,
                 src_basename=basename,
                 src_suffix=suff } <- getPipeEnv

  -- we add the current directory (i.e. the directory in which
  -- the .hs files resides) to the include path, since this is
  -- what gcc does, and it's probably what you want.
        let current_dir = takeDirectory basename
            new_includes = addImplicitQuoteInclude paths [current_dir]
            paths = includePaths dflags0
            dflags = dflags0 { includePaths = new_includes }

        setDynFlags dflags

  -- gather the imports and module name
        (hspp_buf,mod_name,imps,src_imps) <- liftIO $ do
            buf <- hGetStringBuffer input_fn
            let imp_prelude = xopt LangExt.ImplicitPrelude dflags
                popts = initParserOpts dflags
            eimps <- getImports popts imp_prelude buf input_fn (basename <.> suff)
            case eimps of
              Left errs -> throwErrors (fmap pprError errs)
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
        (result, plugin_hsc_env) <-
          liftIO $ hscIncrementalCompile True Nothing (Just msg) hsc_env'
                            mod_summary source_unchanged Nothing (1,1)

        -- In the rest of the pipeline use the loaded plugins
        setPlugins (hsc_plugins        plugin_hsc_env)
                   (hsc_static_plugins plugin_hsc_env)
        -- "driver" plugins may have modified the DynFlags so we update them
        setDynFlags (hsc_dflags plugin_hsc_env)

        return (HscOut src_flavour mod_name result,
                panic "HscOut doesn't have an input filename")

runPhase (HscOut src_flavour mod_name result) _ = do
        dflags <- getDynFlags
        logger <- getLogger
        location <- getLocation src_flavour mod_name
        setModLocation location

        let o_file = ml_obj_file location -- The real object file
            next_phase = hscPostBackendPhase src_flavour (backend dflags)

        case result of
            HscNotGeneratingCode _ _ ->
                return (RealPhase StopLn,
                        panic "No output filename from Hsc when no-code")
            HscUpToDate _ _ ->
                do liftIO $ touchObjectFile logger dflags o_file
                   -- The .o file must have a later modification date
                   -- than the source file (else we wouldn't get Nothing)
                   -- but we touch it anyway, to keep 'make' happy (we think).
                   return (RealPhase StopLn, o_file)
            HscUpdateBoot _ _ ->
                do -- In the case of hs-boot files, generate a dummy .o-boot
                   -- stamp file for the benefit of Make
                   liftIO $ touchObjectFile logger dflags o_file
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
                        hscs_partial_iface = partial_iface,
                        hscs_old_iface_hash = mb_old_iface_hash
                      }
              -> do output_fn <- phaseOutputFilename next_phase

                    PipeState{hsc_env=hsc_env'} <- getPipeState

                    (outputFilename, mStub, foreign_files, cg_infos) <- liftIO $
                      hscGenHardCode hsc_env' cgguts mod_location output_fn

                    let dflags = hsc_dflags hsc_env'
                    final_iface <- liftIO (mkFullIface hsc_env' partial_iface (Just cg_infos))
                    setIface final_iface

                    -- See Note [Writing interface files]
                    liftIO $ hscMaybeWriteIface logger dflags False final_iface mb_old_iface_hash mod_location

                    stub_o <- liftIO (mapM (compileStub hsc_env') mStub)
                    foreign_os <- liftIO $
                      mapM (uncurry (compileForeign hsc_env')) foreign_files
                    setForeignOs (maybe [] return stub_o ++ foreign_os)

                    return (RealPhase next_phase, outputFilename)

-----------------------------------------------------------------------------
-- Cmm phase

runPhase (RealPhase CmmCpp) input_fn = do
       hsc_env <- getPipeSession
       logger <- getLogger
       output_fn <- phaseOutputFilename Cmm
       liftIO $ doCpp logger
                      (hsc_tmpfs hsc_env)
                      (hsc_dflags hsc_env)
                      (hsc_unit_env hsc_env)
                      False{-not raw-}
                      input_fn output_fn
       return (RealPhase Cmm, output_fn)

runPhase (RealPhase Cmm) input_fn = do
       hsc_env <- getPipeSession
       let dflags = hsc_dflags hsc_env
       let next_phase = hscPostBackendPhase HsSrcFile (backend dflags)
       output_fn <- phaseOutputFilename next_phase
       PipeState{hsc_env} <- getPipeState
       mstub <- liftIO $ hscCompileCmmFile hsc_env input_fn output_fn
       stub_o <- liftIO (mapM (compileStub hsc_env) mstub)
       setForeignOs (maybeToList stub_o)
       return (RealPhase next_phase, output_fn)

-----------------------------------------------------------------------------
-- Cc phase

runPhase (RealPhase cc_phase) input_fn
   | any (cc_phase `eqPhase`) [Cc, Ccxx, HCc, Cobjc, Cobjcxx]
   = do
        hsc_env <- getPipeSession
        let dflags    = hsc_dflags hsc_env
        let unit_env  = hsc_unit_env hsc_env
        let home_unit = hsc_home_unit hsc_env
        let tmpfs     = hsc_tmpfs hsc_env
        let platform  = ue_platform unit_env
        let hcc       = cc_phase `eqPhase` HCc

        let cmdline_include_paths = includePaths dflags

        -- HC files have the dependent packages stamped into them
        pkgs <- if hcc then liftIO $ getHCFilePackages input_fn else return []

        -- add package include paths even if we're just compiling .c
        -- files; this is the Value Add(TM) that using ghc instead of
        -- gcc gives you :)
        ps <- liftIO $ mayThrowUnitErr (preloadUnitsInfo' unit_env pkgs)
        let pkg_include_dirs     = collectIncludeDirs ps
        let include_paths_global = foldr (\ x xs -> ("-I" ++ x) : xs) []
              (includePathsGlobal cmdline_include_paths ++ pkg_include_dirs)
        let include_paths_quote = foldr (\ x xs -> ("-iquote" ++ x) : xs) []
              (includePathsQuote cmdline_include_paths ++
               includePathsQuoteImplicit cmdline_include_paths)
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
        let pkg_extra_cc_opts
                | hcc       = []
                | otherwise = collectExtraCcOpts ps

        let framework_paths
                | platformUsesFrameworks platform
                = let pkgFrameworkPaths     = collectFrameworksDirs ps
                      cmdlineFrameworkPaths = frameworkPaths dflags
                  in map ("-F"++) (cmdlineFrameworkPaths ++ pkgFrameworkPaths)
                | otherwise
                = []

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

        ghcVersionH <- liftIO $ getGhcVersionPathName dflags unit_env

        logger <- getLogger
        liftIO $ GHC.SysTools.runCc (phaseForeignLanguage cc_phase) logger tmpfs dflags (
                        [ GHC.SysTools.FileOption "" input_fn
                        , GHC.SysTools.Option "-o"
                        , GHC.SysTools.FileOption "" output_fn
                        ]
                       ++ map GHC.SysTools.Option (
                          pic_c_flags

                -- Stub files generated for foreign exports references the runIO_closure
                -- and runNonIO_closure symbols, which are defined in the base package.
                -- These symbols are imported into the stub.c file via RtsAPI.h, and the
                -- way we do the import depends on whether we're currently compiling
                -- the base package or not.
                       ++ (if platformOS platform == OSMinGW32 &&
                              isHomeUnitId home_unit baseUnitId
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
runPhase (RealPhase (As with_cpp)) input_fn
  = do
        hsc_env <- getPipeSession
        let dflags     = hsc_dflags   hsc_env
        let logger     = hsc_logger   hsc_env
        let unit_env   = hsc_unit_env hsc_env
        let platform   = ue_platform unit_env

        -- LLVM from version 3.0 onwards doesn't support the OS X system
        -- assembler, so we use clang as the assembler instead. (#5636)
        let as_prog | backend dflags == LLVM
                    , platformOS platform == OSDarwin
                    = GHC.SysTools.runClang
                    | otherwise
                    = GHC.SysTools.runAs

        let cmdline_include_paths = includePaths dflags
        let pic_c_flags = picCCOpts dflags

        next_phase <- maybeMergeForeign
        output_fn <- phaseOutputFilename next_phase

        -- we create directories for the object file, because it
        -- might be a hierarchical module.
        liftIO $ createDirectoryIfMissing True (takeDirectory output_fn)

        ccInfo <- liftIO $ getCompilerInfo logger dflags
        let global_includes = [ GHC.SysTools.Option ("-I" ++ p)
                              | p <- includePathsGlobal cmdline_include_paths ]
        let local_includes = [ GHC.SysTools.Option ("-iquote" ++ p)
                             | p <- includePathsQuote cmdline_include_paths ++
                                includePathsQuoteImplicit cmdline_include_paths]
        let runAssembler inputFilename outputFilename
              = liftIO $
                  withAtomicRename outputFilename $ \temp_outputFilename ->
                    as_prog
                       logger dflags
                       (local_includes ++ global_includes
                       -- See Note [-fPIC for assembler]
                       ++ map GHC.SysTools.Option pic_c_flags
                       -- See Note [Produce big objects on Windows]
                       ++ [ GHC.SysTools.Option "-Wa,-mbig-obj"
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
                           then [GHC.SysTools.Option "-mcpu=v9"]
                           else [])
                       ++ (if any (ccInfo ==) [Clang, AppleClang, AppleClang51]
                            then [GHC.SysTools.Option "-Qunused-arguments"]
                            else [])
                       ++ [ GHC.SysTools.Option "-x"
                          , if with_cpp
                              then GHC.SysTools.Option "assembler-with-cpp"
                              else GHC.SysTools.Option "assembler"
                          , GHC.SysTools.Option "-c"
                          , GHC.SysTools.FileOption "" inputFilename
                          , GHC.SysTools.Option "-o"
                          , GHC.SysTools.FileOption "" temp_outputFilename
                          ])

        liftIO $ debugTraceMsg logger dflags 4 (text "Running the assembler")
        runAssembler input_fn output_fn

        return (RealPhase next_phase, output_fn)


-----------------------------------------------------------------------------
-- LlvmOpt phase
runPhase (RealPhase LlvmOpt) input_fn = do
    dflags <- getDynFlags
    logger <- getLogger
    let -- we always (unless -optlo specified) run Opt since we rely on it to
        -- fix up some pretty big deficiencies in the code we generate
        optIdx = max 0 $ min 2 $ optLevel dflags  -- ensure we're in [0,2]
        llvmOpts = case lookup optIdx $ llvmPasses $ llvmConfig dflags of
                    Just passes -> passes
                    Nothing -> panic ("runPhase LlvmOpt: llvm-passes file "
                                      ++ "is missing passes for level "
                                      ++ show optIdx)
        defaultOptions = map GHC.SysTools.Option . concat . fmap words . fst
                         $ unzip (llvmOptions dflags)

        -- don't specify anything if user has specified commands. We do this
        -- for opt but not llc since opt is very specifically for optimisation
        -- passes only, so if the user is passing us extra options we assume
        -- they know what they are doing and don't get in the way.
        optFlag = if null (getOpts dflags opt_lo)
                  then map GHC.SysTools.Option $ words llvmOpts
                  else []

    output_fn <- phaseOutputFilename LlvmLlc

    liftIO $ GHC.SysTools.runLlvmOpt logger dflags
               (   optFlag
                ++ defaultOptions ++
                [ GHC.SysTools.FileOption "" input_fn
                , GHC.SysTools.Option "-o"
                , GHC.SysTools.FileOption "" output_fn]
                )

    return (RealPhase LlvmLlc, output_fn)


-----------------------------------------------------------------------------
-- LlvmLlc phase

runPhase (RealPhase LlvmLlc) input_fn = do
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
    dflags <- getDynFlags
    logger <- getLogger
    let
        llvmOpts = case optLevel dflags of
          0 -> "-O1" -- required to get the non-naive reg allocator. Passing -regalloc=greedy is not sufficient.
          1 -> "-O1"
          _ -> "-O2"

        defaultOptions = map GHC.SysTools.Option . concatMap words . snd
                         $ unzip (llvmOptions dflags)
        optFlag = if null (getOpts dflags opt_lc)
                  then map GHC.SysTools.Option $ words llvmOpts
                  else []

    next_phase <- if -- hidden debugging flag '-dno-llvm-mangler' to skip mangling
                     | gopt Opt_NoLlvmMangler dflags -> return (As False)
                     | otherwise -> return LlvmMangle

    output_fn <- phaseOutputFilename next_phase

    liftIO $ GHC.SysTools.runLlvmLlc logger dflags
                (  optFlag
                ++ defaultOptions
                ++ [ GHC.SysTools.FileOption "" input_fn
                   , GHC.SysTools.Option "-o"
                   , GHC.SysTools.FileOption "" output_fn
                   ]
                )

    return (RealPhase next_phase, output_fn)



-----------------------------------------------------------------------------
-- LlvmMangle phase

runPhase (RealPhase LlvmMangle) input_fn = do
      let next_phase = As False
      output_fn <- phaseOutputFilename next_phase
      dflags <- getDynFlags
      logger <- getLogger
      liftIO $ llvmFixupAsm logger dflags input_fn output_fn
      return (RealPhase next_phase, output_fn)

-----------------------------------------------------------------------------
-- merge in stub objects

runPhase (RealPhase MergeForeign) input_fn = do
     PipeState{foreign_os,hsc_env} <- getPipeState
     output_fn <- phaseOutputFilename StopLn
     liftIO $ createDirectoryIfMissing True (takeDirectory output_fn)
     if null foreign_os
       then panic "runPhase(MergeForeign): no foreign objects"
       else do
         dflags <- getDynFlags
         logger <- getLogger
         let tmpfs = hsc_tmpfs hsc_env
         liftIO $ joinObjectFiles logger tmpfs dflags (input_fn : foreign_os) output_fn
         return (RealPhase StopLn, output_fn)

-- warning suppression
runPhase (RealPhase other) _input_fn =
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

getHCFilePackages :: FilePath -> IO [UnitId]
getHCFilePackages filename =
  Exception.bracket (openFile filename ReadMode) hClose $ \h -> do
    l <- hGetLine h
    case l of
      '/':'*':' ':'G':'H':'C':'_':'P':'A':'C':'K':'A':'G':'E':'S':rest ->
          return (map stringToUnitId (words rest))
      _other ->
          return []


linkDynLibCheck :: Logger -> TmpFs -> DynFlags -> UnitEnv -> [String] -> [UnitId] -> IO ()
linkDynLibCheck logger tmpfs dflags unit_env o_files dep_units = do
  when (haveRtsOptsFlags dflags) $
    putLogMsg logger dflags NoReason SevInfo noSrcSpan
      $ withPprStyle defaultUserStyle
      (text "Warning: -rtsopts and -with-rtsopts have no effect with -shared." $$
      text "    Call hs_init_ghc() from your main() function to set these options.")
  linkDynLib logger tmpfs dflags unit_env o_files dep_units


-- -----------------------------------------------------------------------------
-- Running CPP

-- | Run CPP
--
-- UnitState is needed to compute MIN_VERSION macros
doCpp :: Logger -> TmpFs -> DynFlags -> UnitEnv -> Bool -> FilePath -> FilePath -> IO ()
doCpp logger tmpfs dflags unit_env raw input_fn output_fn = do
    let hscpp_opts = picPOpts dflags
    let cmdline_include_paths = includePaths dflags
    let unit_state = ue_units unit_env
    pkg_include_dirs <- mayThrowUnitErr
                        (collectIncludeDirs <$> preloadUnitsInfo unit_env)
    let include_paths_global = foldr (\ x xs -> ("-I" ++ x) : xs) []
          (includePathsGlobal cmdline_include_paths ++ pkg_include_dirs)
    let include_paths_quote = foldr (\ x xs -> ("-iquote" ++ x) : xs) []
          (includePathsQuote cmdline_include_paths ++
           includePathsQuoteImplicit cmdline_include_paths)
    let include_paths = include_paths_quote ++ include_paths_global

    let verbFlags = getVerbFlags dflags

    let cpp_prog args | raw       = GHC.SysTools.runCpp logger dflags args
                      | otherwise = GHC.SysTools.runCc Nothing logger tmpfs dflags
                                        (GHC.SysTools.Option "-E" : args)

    let platform   = targetPlatform dflags
        targetArch = stringEncodeArch $ platformArch platform
        targetOS = stringEncodeOS $ platformOS platform
        isWindows = platformOS platform == OSMinGW32
    let target_defs =
          [ "-D" ++ HOST_OS     ++ "_BUILD_OS",
            "-D" ++ HOST_ARCH   ++ "_BUILD_ARCH",
            "-D" ++ targetOS    ++ "_HOST_OS",
            "-D" ++ targetArch  ++ "_HOST_ARCH" ]
        -- remember, in code we *compile*, the HOST is the same our TARGET,
        -- and BUILD is the same as our HOST.

    let io_manager_defs =
          [ "-D__IO_MANAGER_WINIO__=1" | isWindows ] ++
          [ "-D__IO_MANAGER_MIO__=1"               ]

    let sse_defs =
          [ "-D__SSE__"      | isSseEnabled      platform ] ++
          [ "-D__SSE2__"     | isSse2Enabled     platform ] ++
          [ "-D__SSE4_2__"   | isSse4_2Enabled   dflags ]

    let avx_defs =
          [ "-D__AVX__"      | isAvxEnabled      dflags ] ++
          [ "-D__AVX2__"     | isAvx2Enabled     dflags ] ++
          [ "-D__AVX512CD__" | isAvx512cdEnabled dflags ] ++
          [ "-D__AVX512ER__" | isAvx512erEnabled dflags ] ++
          [ "-D__AVX512F__"  | isAvx512fEnabled  dflags ] ++
          [ "-D__AVX512PF__" | isAvx512pfEnabled dflags ]

    backend_defs <- getBackendDefs logger dflags

    let th_defs = [ "-D__GLASGOW_HASKELL_TH__" ]
    -- Default CPP defines in Haskell source
    ghcVersionH <- getGhcVersionPathName dflags unit_env
    let hsSourceCppOpts = [ "-include", ghcVersionH ]

    -- MIN_VERSION macros
    let uids = explicitUnits unit_state
        pkgs = catMaybes (map (lookupUnit unit_state) uids)
    mb_macro_include <-
        if not (null pkgs) && gopt Opt_VersionMacros dflags
            then do macro_stub <- newTempName logger tmpfs dflags TFL_CurrentModule "h"
                    writeFile macro_stub (generatePackageVersionMacros pkgs)
                    -- Include version macros for every *exposed* package.
                    -- Without -hide-all-packages and with a package database
                    -- size of 1000 packages, it takes cpp an estimated 2
                    -- milliseconds to process this file. See #10970
                    -- comment 8.
                    return [GHC.SysTools.FileOption "-include" macro_stub]
            else return []

    cpp_prog       (   map GHC.SysTools.Option verbFlags
                    ++ map GHC.SysTools.Option include_paths
                    ++ map GHC.SysTools.Option hsSourceCppOpts
                    ++ map GHC.SysTools.Option target_defs
                    ++ map GHC.SysTools.Option backend_defs
                    ++ map GHC.SysTools.Option th_defs
                    ++ map GHC.SysTools.Option hscpp_opts
                    ++ map GHC.SysTools.Option sse_defs
                    ++ map GHC.SysTools.Option avx_defs
                    ++ map GHC.SysTools.Option io_manager_defs
                    ++ mb_macro_include
        -- Set the language mode to assembler-with-cpp when preprocessing. This
        -- alleviates some of the C99 macro rules relating to whitespace and the hash
        -- operator, which we tend to abuse. Clang in particular is not very happy
        -- about this.
                    ++ [ GHC.SysTools.Option     "-x"
                       , GHC.SysTools.Option     "assembler-with-cpp"
                       , GHC.SysTools.Option     input_fn
        -- We hackily use Option instead of FileOption here, so that the file
        -- name is not back-slashed on Windows.  cpp is capable of
        -- dealing with / in filenames, so it works fine.  Furthermore
        -- if we put in backslashes, cpp outputs #line directives
        -- with *double* backslashes.   And that in turn means that
        -- our error messages get double backslashes in them.
        -- In due course we should arrange that the lexer deals
        -- with these \\ escapes properly.
                       , GHC.SysTools.Option     "-o"
                       , GHC.SysTools.FileOption "" output_fn
                       ])

getBackendDefs :: Logger -> DynFlags -> IO [String]
getBackendDefs logger dflags | backend dflags == LLVM = do
    llvmVer <- figureLlvmVersion logger dflags
    return $ case fmap llvmVersionList llvmVer of
               Just [m] -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (m,0) ]
               Just (m:n:_) -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (m,n) ]
               _ -> []
  where
    format (major, minor)
      | minor >= 100 = error "getBackendDefs: Unsupported minor version"
      | otherwise = show $ (100 * major + minor :: Int) -- Contract is Int

getBackendDefs _ _ =
    return []

-- ---------------------------------------------------------------------------
-- Macros (cribbed from Cabal)

generatePackageVersionMacros :: [UnitInfo] -> String
generatePackageVersionMacros pkgs = concat
  -- Do not add any C-style comments. See #3389.
  [ generateMacros "" pkgname version
  | pkg <- pkgs
  , let version = unitPackageVersion pkg
        pkgname = map fixchar (unitPackageNameString pkg)
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


Note [Merging object files for GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHCi can usually loads standard linkable object files using GHC's linker
implementation. However, most users build their projects with -split-sections,
meaning that such object files can have an extremely high number of sections.
As the linker must map each of these sections individually, loading such object
files is very inefficient.

To avoid this inefficiency, we use the linker's `-r` flag and a linker script
to produce a merged relocatable object file. This file will contain a singe
text section section and can consequently be mapped far more efficiently. As
gcc tends to do unpredictable things to our linker command line, we opt to
invoke ld directly in this case, in contrast to our usual strategy of linking
via gcc.

-}

joinObjectFiles :: Logger -> TmpFs -> DynFlags -> [FilePath] -> FilePath -> IO ()
joinObjectFiles logger tmpfs dflags o_files output_fn = do
  let toolSettings' = toolSettings dflags
      ldIsGnuLd = toolSettings_ldIsGnuLd toolSettings'
      osInfo = platformOS (targetPlatform dflags)
      ld_r args = GHC.SysTools.runMergeObjects logger tmpfs dflags (
                        -- See Note [Produce big objects on Windows]
                        concat
                          [ [GHC.SysTools.Option "--oformat", GHC.SysTools.Option "pe-bigobj-x86-64"]
                          | OSMinGW32 == osInfo
                          , not $ target32Bit (targetPlatform dflags)
                          ]
                     ++ map GHC.SysTools.Option ld_build_id
                     ++ [ GHC.SysTools.Option "-o",
                          GHC.SysTools.FileOption "" output_fn ]
                     ++ args)

      -- suppress the generation of the .note.gnu.build-id section,
      -- which we don't need and sometimes causes ld to emit a
      -- warning:
      ld_build_id | toolSettings_ldSupportsBuildId toolSettings' = ["--build-id=none"]
                  | otherwise                     = []

  if ldIsGnuLd
     then do
          script <- newTempName logger tmpfs dflags TFL_CurrentModule "ldscript"
          cwd <- getCurrentDirectory
          let o_files_abs = map (\x -> "\"" ++ (cwd </> x) ++ "\"") o_files
          writeFile script $ "INPUT(" ++ unwords o_files_abs ++ ")"
          ld_r [GHC.SysTools.FileOption "" script]
     else if toolSettings_ldSupportsFilelist toolSettings'
     then do
          filelist <- newTempName logger tmpfs dflags TFL_CurrentModule "filelist"
          writeFile filelist $ unlines o_files
          ld_r [GHC.SysTools.Option "-filelist",
                GHC.SysTools.FileOption "" filelist]
     else
          ld_r (map (GHC.SysTools.FileOption "") o_files)

-- -----------------------------------------------------------------------------
-- Misc.

writeInterfaceOnlyMode :: DynFlags -> Bool
writeInterfaceOnlyMode dflags =
 gopt Opt_WriteInterface dflags &&
 NoBackend == backend dflags

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
hscPostBackendPhase :: HscSource -> Backend -> Phase
hscPostBackendPhase HsBootFile _    =  StopLn
hscPostBackendPhase HsigFile _      =  StopLn
hscPostBackendPhase _ bcknd =
  case bcknd of
        ViaC        -> HCc
        NCG         -> As False
        LLVM        -> LlvmOpt
        NoBackend   -> StopLn
        Interpreter -> StopLn

touchObjectFile :: Logger -> DynFlags -> FilePath -> IO ()
touchObjectFile logger dflags path = do
  createDirectoryIfMissing True $ takeDirectory path
  GHC.SysTools.touch logger dflags "Touching object file" path

-- | Find out path to @ghcversion.h@ file
getGhcVersionPathName :: DynFlags -> UnitEnv -> IO FilePath
getGhcVersionPathName dflags unit_env = do
  candidates <- case ghcVersionFile dflags of
    Just path -> return [path]
    Nothing -> do
        ps <- mayThrowUnitErr (preloadUnitsInfo' unit_env [rtsUnitId])
        return ((</> "ghcversion.h") <$> collectIncludeDirs ps)

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
