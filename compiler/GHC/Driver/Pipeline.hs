{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
   TPhase(..), PipeEnv(..),
   phaseOutputFilenameNew,
   hscPostBackendPhase,

   doCpp,
   linkingNeeded, checkLinkInfo, writeInterfaceOnlyMode
  ,P,runPipelineNew') where

#include "ghcplatform.h"
import GHC.Prelude

import GHC.Platform

import GHC.Tc.Types
import GHC.Tc.Utils.Monad hiding ( getImports )

import GHC.Driver.Main
import GHC.Driver.Env hiding ( Hsc )
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Pipeline.Monad
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Phases
import GHC.Driver.Pipeline.Phases
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Ppr
import GHC.Driver.Hooks

import GHC.Platform.Ways
import GHC.Platform.ArchOS

import GHC.Parser.Header

import GHC.SysTools
import GHC.Utils.TmpFs

import GHC.Linker.ExtraObj
import GHC.Linker.Dynamic
import GHC.Linker.Static
import GHC.Linker.Types

import GHC.Utils.Outputable
import GHC.Utils.Error
import GHC.Utils.Fingerprint
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Exception as Exception
import GHC.Utils.Logger

import GHC.CmmToLlvm         ( llvmFixupAsm, llvmVersionList )
import qualified GHC.LanguageExtensions as LangExt
import GHC.Settings

import GHC.Data.FastString     ( mkFastString )
import GHC.Data.StringBuffer   ( hGetStringBuffer, hPutStringBuffer )
import GHC.Data.Maybe          ( expectJust )

import GHC.Iface.Make          ( mkFullIface )
import GHC.Runtime.Loader      ( initializePlugins )


import GHC.Types.Basic       ( SuccessFlag(..) )
import GHC.Types.Error       ( singleMessage, getMessages )
import GHC.Types.Name.Env
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
import Data.IORef
import Data.List        ( isInfixOf, intercalate )
import Data.Maybe
import Data.Version
import Data.Either      ( partitionEithers )

import Data.Time        ( getCurrentTime )
import GHC.Driver.CmdLine
import Control.Monad.Catch
import Control.Monad.Trans.Reader

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
           -> IO (Either DriverMessages (DynFlags, FilePath))
preprocess hsc_env input_fn mb_input_buf mb_phase =
  handleSourceError (\err -> return $ Left $ to_driver_messages $ srcErrorMessages err) $
  MC.handle handler $
  fmap Right $ do
  massertPpr (isJust mb_phase || isHaskellSrcFilename input_fn) (text input_fn)
  input_fn_final <- mkInputFn
  let preprocess_pipeline = preprocessPipeline pipe_env (setDumpPrefix pipe_env hsc_env) input_fn_final
  (dflags, fp) <- runPipelineNew' (hsc_hooks hsc_env) preprocess_pipeline

  -- We stop before Hsc phase so we shouldn't generate an interface
  return (dflags, fp)
  where
    srcspan = srcLocSpan $ mkSrcLoc (mkFastString input_fn) 1 1
    handler (ProgramError msg) =
      return $ Left $ singleMessage $
        mkPlainErrorMsgEnvelope srcspan $
        DriverUnknownMessage $ mkPlainError noHints $ text msg
    handler ex = throwGhcExceptionIO ex

    to_driver_messages :: Messages GhcMessage -> Messages DriverMessage
    to_driver_messages msgs = case traverse to_driver_message msgs of
      Nothing    -> pprPanic "non-driver message in preprocess"
                             (vcat $ pprMsgEnvelopeBagWithLoc (getMessages msgs))
      Just msgs' -> msgs'

    to_driver_message = \case
      GhcDriverMessage msg
        -> Just msg
      GhcPsMessage (PsHeaderMessage msg)
        -> Just (DriverPsHeaderMessage (PsHeaderMessage msg))
      _ -> Nothing

    pipe_env = mkPipeEnv StopPreprocess input_fn (Temporary TFL_GhcSession)
    mkInputFn  =
      case mb_input_buf of
        Just input_buf -> do
          fn <- newTempName (hsc_logger hsc_env)
                            (hsc_tmpfs hsc_env)
                            (hsc_dflags hsc_env)
                            TFL_CurrentModule
                            ("buf_" ++ src_suffix pipe_env)
          hdl <- openBinaryFile fn WriteMode
          -- Add a LINE pragma so reported source locations will
          -- mention the real input file, not this temp file.
          hPutStrLn hdl $ "{-# LINE 1 \""++ input_fn ++ "\"#-}"
          hPutStringBuffer hdl input_buf
          hClose hdl
          return fn
        Nothing -> return input_fn

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

type P m = TPipelineClass TPhase m

compileOne :: HscEnv
           -> ModSummary      -- ^ summary for module being compiled
           -> Int             -- ^ module N ...
           -> Int             -- ^ ... of M
           -> Maybe ModIface  -- ^ old interface, if we have one
           -> Maybe Linkable  -- ^ old linkable, if we have one
           -> IO HomeModInfo   -- ^ the complete HomeModInfo, if successful

compileOne = compileOne' (Just batchMsg)

compileOne' :: Maybe Messager
            -> HscEnv
            -> ModSummary      -- ^ summary for module being compiled
            -> Int             -- ^ module N ...
            -> Int             -- ^ ... of M
            -> Maybe ModIface  -- ^ old interface, if we have one
            -> Maybe Linkable  -- ^ old linkable, if we have one
            -> IO HomeModInfo   -- ^ the complete HomeModInfo, if successful

compileOne' mHscMessage
            hsc_env0 summary mod_index nmods mb_old_iface mb_old_linkable
 = do

   debugTraceMsg logger 2 (text "compile: input file" <+> text input_fnpp)

   let flags = hsc_dflags hsc_env0
     in do unless (gopt Opt_KeepHiFiles flags) $
               addFilesToClean tmpfs TFL_CurrentModule $
                   [ml_hi_file $ ms_location summary]
           unless (gopt Opt_KeepOFiles flags) $
               addFilesToClean tmpfs TFL_GhcSession $
                   [ml_obj_file $ ms_location summary]

   plugin_hsc_env <- initializePlugins hsc_env (Just (ms_mnwib summary))
   let pipe_env = mkPipeEnv NoStop input_fn pipelineOutput
   status <- hscRecompStatus mHscMessage plugin_hsc_env summary
                mb_old_iface mb_old_linkable (mod_index, nmods)
   let pipeline = hscPipeline pipe_env (setDumpPrefix pipe_env plugin_hsc_env, summary, status)
   (iface, old_linkable) <- runPipelineNew' (hsc_hooks hsc_env) pipeline
   -- See Note [ModDetails and --make mode]
   details <- initModDetails plugin_hsc_env summary iface
   return $! HomeModInfo iface details old_linkable

 where lcl_dflags  = ms_hspp_opts summary
       location    = ms_location summary
       input_fn    = expectJust "compile:hs" (ml_hs_file location)
       input_fnpp  = ms_hspp_file summary
       mod_graph   = hsc_mod_graph hsc_env0
       needsLinker = needsTemplateHaskellOrQQ mod_graph
       isDynWay    = any (== WayDyn) (ways lcl_dflags)
       isProfWay   = any (== WayProf) (ways lcl_dflags)
       internalInterpreter = not (gopt Opt_ExternalInterpreter lcl_dflags)

       pipelineOutput = case bcknd of
         Interpreter -> NoOutputFile
         NoBackend -> NoOutputFile
         _ -> Persistent

       logger = hsc_logger hsc_env0
       tmpfs  = hsc_tmpfs hsc_env0

       -- #8180 - when using TemplateHaskell, switch on -dynamic-too so
       -- the linker can correctly load the object files.  This isn't necessary
       -- when using -fexternal-interpreter.
       dflags1 = if hostIsDynamic && internalInterpreter &&
                    not isDynWay && not isProfWay && needsLinker
                  then gopt_set lcl_dflags Opt_BuildDynamicToo
                  else lcl_dflags

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
         | Just Target { targetAllowObjCode = obj } <- findTarget summary (hsc_targets hsc_env0)
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
         = (Interpreter, gopt_set (dflags2 { backend = Interpreter }) Opt_ForceRecomp)
         | otherwise
         = (backend dflags, dflags2)
       dflags  = dflags3 { includePaths = addImplicitQuoteInclude old_paths [current_dir] }
       hsc_env = hscSetFlags dflags hsc_env0

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
        let pipeline = case lang of
              LangC      -> viaCPipeline Cc
              LangCxx    -> viaCPipeline Ccxx
              LangObjc   -> viaCPipeline Cobjc
              LangObjcxx -> viaCPipeline Cobjcxx
              LangAsm    -> \pe hsc_env ml fp -> Just <$> asPipeline True pe hsc_env ml fp
#if __GLASGOW_HASKELL__ < 811
              RawObject  -> panic "compileForeign: should be unreachable"
#endif
            pipe_env = mkPipeEnv NoStop stub_c (Temporary TFL_GhcSession)
        res <- runPipelineNew' (hsc_hooks hsc_env) (pipeline pipe_env hsc_env Nothing stub_c)
        case res of
          -- This should never happen as viaCPipeline should only return `Nothing` when the stop phase is `StopC`.
          -- Future refactoring to not check StopC for this case
          Nothing -> pprPanic "compileForeign" (ppr stub_c)
          Just fp -> return fp

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
  let pipe_env = (mkPipeEnv NoStop empty_stub Persistent) { src_basename = basename}
      pipeline = viaCPipeline HCc pipe_env hsc_env (Just location) empty_stub
  _ <- runPipelineNew' (hsc_hooks hsc_env) pipeline
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
            pkg_deps  = concatMap (dep_direct_pkgs . mi_deps . hm_iface) home_mod_infos

            -- the linkables to link
            linkables = map (expectJust "link".hm_linkable) home_mod_infos

        debugTraceMsg logger 3 (text "link: linkables are ..." $$ vcat (map ppr linkables))

        -- check for the -no-link flag
        if isNoLink (ghcLink dflags)
          then do debugTraceMsg logger 3 (text "link(batch): linking omitted (-c flag given).")
                  return Succeeded
          else do

        let getOfiles LM{ linkableUnlinked } = map nameOfObject (filter isObject linkableUnlinked)
            obj_files = concatMap getOfiles linkables
            platform  = targetPlatform dflags
            exe_file  = exeFileName platform staticLink (outputFile dflags)

        linking_needed <- linkingNeeded logger dflags unit_env staticLink linkables pkg_deps

        if not (gopt Opt_ForceRecomp dflags) && not linking_needed
           then do debugTraceMsg logger 2 (text exe_file <+> text "is up to date, linking not required.")
                   return Succeeded
           else do

        compilationProgressMsg logger (text "Linking " <> text exe_file <> text " ...")

        -- Don't showPass in Batch mode; doLink will do that for us.
        let link = case ghcLink dflags of
                LinkBinary    -> linkBinary logger tmpfs
                LinkStaticLib -> linkStaticLib logger
                LinkDynLib    -> linkDynLibCheck logger tmpfs
                other         -> panicBadLink other
        link dflags unit_env obj_files pkg_deps

        debugTraceMsg logger 3 (text "link: done")

        -- linkBinary only returns if it succeeds
        return Succeeded

   | otherwise
   = do debugTraceMsg logger 3 (text "link(batch): upsweep (partially) failed OR" $$
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

oneShot :: HscEnv -> StopPhase -> [(String, Maybe Phase)] -> IO ()
oneShot hsc_env stop_phase srcs = do
  o_files <- mapMaybeM (compileFile hsc_env stop_phase) srcs
  case stop_phase of
    StopPreprocess -> return ()
    StopC  -> return ()
    StopAs -> return ()
    NoStop -> doLink hsc_env o_files

compileFile :: HscEnv -> StopPhase -> (FilePath, Maybe Phase) -> IO (Maybe FilePath)
compileFile hsc_env stop_phase (src, _mb_phase) = do
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
         | NoBackend <- backend dflags = NoOutputFile
         | NoStop <- stop_phase, not (isNoLink ghc_link) = Persistent
                -- -o foo applies to linker
         | isJust mb_o_file = SpecificFile
                -- -o foo applies to the file we are compiling now
         | otherwise = Persistent
        pipe_env = mkPipeEnv stop_phase src output
        pipeline = pipelineStart pipe_env (setDumpPrefix pipe_env hsc_env) src
   runPipelineNew' (hsc_hooks hsc_env) pipeline


doLink :: HscEnv -> [FilePath] -> IO ()
doLink hsc_env o_files =
    let
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


-- | Wrap up a pipeline with specific module scope
mkPipeEnv :: StopPhase -- End phase
          -> FilePath -- input fn
          -> PipelineOutput -- Output
          -> PipeEnv
mkPipeEnv stop_phase  input_fn output =

  let (basename, suffix) = splitExtension input_fn
      suffix' = drop 1 suffix -- strip off the .
      env = PipeEnv{ stop_phase,
                     src_filename = input_fn,
                     src_basename = basename,
                     src_suffix = suffix',
                     output_spec = output }
  in env

setDumpPrefix :: PipeEnv -> HscEnv -> HscEnv
setDumpPrefix pipe_env hsc_env =
  hscUpdateFlags (\dflags -> dflags { dumpPrefix = Just (src_basename pipe_env ++ ".")}) hsc_env


newtype HookedUse a = HookedUse { runHookedUse :: Hooks -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch) via (ReaderT Hooks IO)

instance MonadUse TPhase HookedUse where
  use_ fa = HookedUse $ \hooks ->
    case runPhaseHook hooks of
      Nothing -> runPhaseNew fa
      Just (PhaseHook h) -> h fa

runPipelineNew' :: Hooks -> HookedUse a -> IO a
runPipelineNew' hooks pipeline = runHookedUse pipeline hooks

phaseOutputFilenameNew :: Phase -> PipeEnv -> HscEnv -> Maybe ModLocation -> IO FilePath
phaseOutputFilenameNew next_phase pipe_env hsc_env maybe_loc = do
  let PipeEnv{stop_phase, src_basename, output_spec} = pipe_env
  --PipeState{maybe_loc,hsc_env} <- getPipeState
  let dflags = hsc_dflags hsc_env
      logger = hsc_logger hsc_env
      tmpfs = hsc_tmpfs hsc_env
      --next_phase = nextPhase (targetPlatform dflags) cur_phase
  getOutputFilename logger tmpfs (stopPhaseToPhase stop_phase) output_spec
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
             | Just d <- odir = (d </> persistent)
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



runPhaseNew :: TPhase out -> IO out
runPhaseNew (T_Unlit pipe_env hsc_env inp_path) = do
  out_path <- phaseOutputFilenameNew (Cpp HsSrcFile) pipe_env hsc_env Nothing
  liftIO $ runUnlitPhase hsc_env inp_path out_path
runPhaseNew (T_FileArgs hsc_env inp_path) =
  liftIO $ getFileArgs hsc_env inp_path
runPhaseNew (T_Cpp pipe_env hsc_env inp_path) = do
  out_path <- phaseOutputFilenameNew (HsPp HsSrcFile) pipe_env hsc_env Nothing
  liftIO $ runCppPhase hsc_env inp_path out_path
runPhaseNew (T_HsPp pipe_env hsc_env origin_path inp_path) = do
  out_path <- phaseOutputFilenameNew (Hsc HsSrcFile) pipe_env hsc_env Nothing
  liftIO $ runHsPpPhase hsc_env origin_path inp_path out_path
runPhaseNew (T_HscRecomp pipe_env hsc_env fp hsc_src) = do
  runHscPhase pipe_env hsc_env fp hsc_src
runPhaseNew (T_Hsc hsc_env mod_sum) = runHscTcPhase hsc_env mod_sum
runPhaseNew (T_HscPostTc hsc_env ms fer m mfi) =
  runHscPostTcPhase hsc_env ms fer m mfi
runPhaseNew (T_HscBackend pipe_env hsc_env mod_name hsc_src location x) = do
  runHscBackendPhase pipe_env hsc_env mod_name hsc_src location x
runPhaseNew (T_CmmCpp pipe_env hsc_env input_fn) = do
  output_fn <- phaseOutputFilenameNew Cmm pipe_env hsc_env Nothing
  doCpp (hsc_logger hsc_env)
        (hsc_tmpfs hsc_env)
        (hsc_dflags hsc_env)
        (hsc_unit_env hsc_env)
        False{-not raw-}
        input_fn output_fn
  return output_fn
runPhaseNew (T_Cmm pipe_env hsc_env input_fn) = do
  let dflags = hsc_dflags hsc_env
  let next_phase = hscPostBackendPhase HsSrcFile (backend dflags)
  output_fn <- phaseOutputFilenameNew next_phase pipe_env hsc_env Nothing
  mstub <- hscCompileCmmFile hsc_env input_fn output_fn
  stub_o <- mapM (compileStub hsc_env) mstub
  let foreign_os = (maybeToList stub_o)
  return (foreign_os, output_fn)

runPhaseNew (T_Cc phase pipe_env hsc_env input_fn) = runCcPhase phase pipe_env hsc_env input_fn
runPhaseNew (T_As cpp pipe_env hsc_env location input_fn) = do
  runAsPhase cpp pipe_env hsc_env location input_fn
runPhaseNew (T_LlvmOpt pipe_env hsc_env input_fn) =
  runLlvmOptPhase pipe_env hsc_env input_fn
runPhaseNew (T_LlvmLlc pipe_env hsc_env input_fn) =
  runLlvmLlcPhase pipe_env hsc_env input_fn
runPhaseNew (T_LlvmMangle pipe_env hsc_env input_fn) =
  runLlvmManglePhase pipe_env hsc_env input_fn
runPhaseNew (T_MergeForeign pipe_env hsc_env location input_fn fos) =
  runMergeForeign pipe_env hsc_env location input_fn fos
runPhaseNew (T_IO io_action) = liftIO io_action

runLlvmManglePhase :: PipeEnv -> HscEnv -> FilePath -> IO [Char]
runLlvmManglePhase pipe_env hsc_env input_fn = do
      let next_phase = As False
      output_fn <- phaseOutputFilenameNew next_phase pipe_env hsc_env Nothing
      let dflags = hsc_dflags hsc_env
      llvmFixupAsm (targetPlatform dflags) input_fn output_fn
      return output_fn

runMergeForeign :: PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> [FilePath] -> IO FilePath
runMergeForeign _pipe_env hsc_env _location input_fn foreign_os = do
     if null foreign_os
       then return input_fn
       else do
         -- Work around a binutil < 2.31 bug where you can't merge objects if the output file
         -- is one of the inputs
         new_o <- newTempName (hsc_logger hsc_env) (hsc_tmpfs hsc_env) (hsc_dflags hsc_env) TFL_CurrentModule "o"
         copyFile input_fn new_o
         let dflags = hsc_dflags hsc_env
             logger = hsc_logger hsc_env
         let tmpfs = hsc_tmpfs hsc_env
         liftIO $ joinObjectFiles logger tmpfs dflags (new_o : foreign_os) input_fn
         return input_fn


runLlvmLlcPhase :: PipeEnv -> HscEnv -> FilePath -> IO FilePath
runLlvmLlcPhase pipe_env hsc_env input_fn = do
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
    let dflags = hsc_dflags hsc_env
        logger = hsc_logger hsc_env
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

    output_fn <- phaseOutputFilenameNew next_phase pipe_env hsc_env Nothing

    liftIO $ GHC.SysTools.runLlvmLlc logger dflags
                (  optFlag
                ++ defaultOptions
                ++ [ GHC.SysTools.FileOption "" input_fn
                   , GHC.SysTools.Option "-o"
                   , GHC.SysTools.FileOption "" output_fn
                   ]
                )

    return output_fn

runLlvmOptPhase :: PipeEnv -> HscEnv -> FilePath -> IO FilePath
runLlvmOptPhase pipe_env hsc_env input_fn = do
    let dflags = hsc_dflags hsc_env
        logger = hsc_logger hsc_env
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

    output_fn <- phaseOutputFilenameNew LlvmLlc pipe_env hsc_env Nothing

    liftIO $ GHC.SysTools.runLlvmOpt logger dflags
               (   optFlag
                ++ defaultOptions ++
                [ GHC.SysTools.FileOption "" input_fn
                , GHC.SysTools.Option "-o"
                , GHC.SysTools.FileOption "" output_fn]
                )

    return output_fn




runAsPhase :: Bool -> PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> IO FilePath
runAsPhase with_cpp pipe_env hsc_env location input_fn = do
        let dflags     = hsc_dflags   hsc_env
        let logger     = hsc_logger   hsc_env
        let unit_env   = hsc_unit_env hsc_env
        let platform   = ue_platform unit_env

        -- LLVM from version 3.0 onwards doesn't support the OS X system
        -- assembler, so we use clang as the assembler instead. (#5636)
        let (as_prog, get_asm_info) | backend dflags == LLVM
                    , platformOS platform == OSDarwin
                    = (GHC.SysTools.runClang, pure Clang)
                    | otherwise
                    = (GHC.SysTools.runAs, liftIO $ getAssemblerInfo logger dflags)

        asmInfo <- get_asm_info

        let cmdline_include_paths = includePaths dflags
        let pic_c_flags = picCCOpts dflags

        output_fn <- phaseOutputFilenameNew StopLn pipe_env hsc_env location

        -- we create directories for the object file, because it
        -- might be a hierarchical module.
        liftIO $ createDirectoryIfMissing True (takeDirectory output_fn)

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
                       ++ (if any (asmInfo ==) [Clang, AppleClang, AppleClang51]
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

        debugTraceMsg logger 4 (text "Running the assembler")
        runAssembler input_fn output_fn

        return output_fn


runCcPhase :: Phase -> PipeEnv -> HscEnv -> FilePath -> IO FilePath
runCcPhase cc_phase pipe_env hsc_env input_fn = do
  let dflags    = hsc_dflags hsc_env
  let logger    = hsc_logger hsc_env
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
  output_fn <- phaseOutputFilenameNew next_phase pipe_env hsc_env Nothing

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

  return output_fn

-- This is where all object files get written from, for hs-boot and hsig files as well.
runHscBackendPhase :: PipeEnv
                   -> HscEnv
                   -> ModuleName
                   -> HscSource
                   -> ModLocation
                   -> HscBackendAction
                   -> IO ([FilePath], ModIface, Maybe Linkable, FilePath)
runHscBackendPhase pipe_env hsc_env mod_name src_flavour location result = runHsc hsc_env $ do
  dflags <- getDynFlags
  logger <- getLogger
  let o_file = ml_obj_file location -- The real object file
      next_phase = hscPostBackendPhase src_flavour (backend dflags)
  case result of
      HscUpdate iface ->
          do
             case src_flavour of
               HsigFile -> do
                 -- We need to create a REAL but empty .o file
                 -- because we are going to attempt to put it in a library
                 let input_fn = expectJust "runPhase" (ml_hs_file location)
                     basename = dropExtension input_fn
                 liftIO $ compileEmptyStub dflags hsc_env basename location mod_name

               -- In the case of hs-boot files, generate a dummy .o-boot
               -- stamp file for the benefit of Make
               HsBootFile -> liftIO $ touchObjectFile logger dflags o_file
               HsSrcFile -> panic "HscUpdate not relevant for HscSrcFile"

             return ([], iface, Nothing, o_file)
      HscRecomp { hscs_guts = cgguts,
                  hscs_mod_location = mod_location,
                  hscs_partial_iface = partial_iface,
                  hscs_old_iface_hash = mb_old_iface_hash
                }
        -> case backend dflags of
          NoBackend -> panic "HscRecomp not relevant for NoBackend"
          Interpreter -> do
              -- In interpreted mode the regular codeGen backend is not run so we
              -- generate a interface without codeGen info.
              final_iface <- liftIO $ mkFullIface hsc_env partial_iface Nothing
              liftIO $ hscMaybeWriteIface logger dflags True final_iface mb_old_iface_hash location

              (hasStub, comp_bc, spt_entries) <- liftIO $ hscInteractive hsc_env cgguts mod_location

              stub_o <- liftIO $ case hasStub of
                        Nothing -> return []
                        Just stub_c -> do
                            stub_o <- compileStub hsc_env stub_c
                            return [DotO stub_o]

              let hs_unlinked = [BCOs comp_bc spt_entries]
              unlinked_time <- liftIO getCurrentTime
              let !linkable = LM unlinked_time (mkHomeModule (hsc_home_unit hsc_env) mod_name)
                             (hs_unlinked ++ stub_o)
--              setIface final_iface
--              setLinkable linkable
              return ([], final_iface, Just linkable, panic "interpreter")
          _ -> do
              output_fn <- liftIO $ phaseOutputFilenameNew next_phase pipe_env hsc_env (Just location)
              (outputFilename, mStub, foreign_files, cg_infos) <- liftIO $
                hscGenHardCode hsc_env cgguts mod_location output_fn
              final_iface <- liftIO (mkFullIface hsc_env partial_iface (Just cg_infos))
             -- setIface final_iface


              -- See Note [Writing interface files]
              liftIO $ hscMaybeWriteIface logger dflags False final_iface mb_old_iface_hash mod_location

              stub_o <- liftIO (mapM (compileStub hsc_env) mStub)
              foreign_os <- liftIO $
                mapM (uncurry (compileForeign hsc_env)) foreign_files
              let fos = (maybe [] return stub_o ++ foreign_os)

              -- This is awkward, no linkable is produced here because we still
              -- have some way to do before the object file is produced
              -- In future we can split up the driver logic more so that this function
              -- is in TPipeline and in this branch we can invoke the rest of the backend phase.
              return (fos, final_iface, Nothing, outputFilename)


runUnlitPhase :: HscEnv -> FilePath -> FilePath -> IO FilePath
runUnlitPhase hsc_env input_fn output_fn = do
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

--    output_fn <- phaseOutputFilename (Cpp sf)

    let flags = [ -- The -h option passes the file name for unlit to
                  -- put in a #line directive
                  GHC.SysTools.Option     "-h"
                  -- See Note [Don't normalise input filenames].
                , GHC.SysTools.Option $ escape input_fn
                , GHC.SysTools.FileOption "" input_fn
                , GHC.SysTools.FileOption "" output_fn
                ]

    let dflags = hsc_dflags hsc_env
        logger = hsc_logger hsc_env
    GHC.SysTools.runUnlit logger dflags flags

    return output_fn

getFileArgs :: HscEnv -> FilePath -> IO ((DynFlags, [Warn]))
getFileArgs hsc_env input_fn = do
  let dflags0 = hsc_dflags hsc_env
      parser_opts = initParserOpts dflags0
  src_opts <- getOptionsFromFile parser_opts input_fn
  (dflags1, unhandled_flags, warns)
    <- parseDynamicFilePragma dflags0 src_opts
  checkProcessArgsResult unhandled_flags
  return (dflags1, warns)

--               liftIO $ handleFlagWarnings logger dflags1 warns
runCppPhase :: HscEnv -> FilePath -> FilePath -> IO FilePath
runCppPhase hsc_env input_fn output_fn = do
  doCpp (hsc_logger hsc_env)
           (hsc_tmpfs hsc_env)
           (hsc_dflags hsc_env)
           (hsc_unit_env hsc_env)
           True{-raw-}
           input_fn output_fn
  return output_fn


runHscPhase :: PipeEnv
  -> HscEnv
  -> FilePath
  -> HscSource
  -> IO (HscEnv, ModSummary, HscRecompStatus)
runHscPhase pipe_env hsc_env0 input_fn src_flavour = do
  let dflags0 = hsc_dflags hsc_env0
      PipeEnv{ src_basename=basename,
               src_suffix=suff } = pipe_env

  -- we add the current directory (i.e. the directory in which
  -- the .hs files resides) to the include path, since this is
  -- what gcc does, and it's probably what you want.
  let current_dir = takeDirectory basename
      new_includes = addImplicitQuoteInclude paths [current_dir]
      paths = includePaths dflags0
      dflags = dflags0 { includePaths = new_includes }
      hsc_env = hscSetFlags dflags hsc_env0



  -- gather the imports and module name
  (hspp_buf,mod_name,imps,src_imps) <- do
              buf <- hGetStringBuffer input_fn
              let imp_prelude = xopt LangExt.ImplicitPrelude dflags
                  popts = initParserOpts dflags
              eimps <- getImports popts imp_prelude buf input_fn (basename <.> suff)
              case eimps of
                  Left errs -> throwErrors (GhcPsMessage <$> errs)
                  Right (src_imps,imps,L _ mod_name) -> return
                        (Just buf, mod_name, imps, src_imps)

  -- Take -o into account if present
  -- Very like -ohi, but we must *only* do this if we aren't linking
  -- (If we're linking then the -o applies to the linked thing, not to
  -- the object file for one module.)
  -- Note the nasty duplication with the same computation in compileFile above
  location <- getLocationNew pipe_env dflags src_flavour mod_name
  let o_file = ml_obj_file location -- The real object file
      hi_file = ml_hi_file location
      hie_file = ml_hie_file location
      dyn_o_file = dynamicOutputFile dflags o_file

  src_hash <- getFileHash (basename <.> suff)
  hi_date <- modificationTimeIfExists hi_file
  hie_date <- modificationTimeIfExists hie_file
  o_mod <- modificationTimeIfExists o_file
  dyn_o_mod <- modificationTimeIfExists dyn_o_file

  -- Tell the finder cache about this module
  mod <- do
    let home_unit = hsc_home_unit hsc_env
    let fc        = hsc_FC hsc_env
    addHomeModuleToFinder fc home_unit mod_name location

  -- Make the ModSummary to hand to hscMain
  let
    mod_summary = ModSummary {  ms_mod       = mod,
                                ms_hsc_src   = src_flavour,
                                ms_hspp_file = input_fn,
                                ms_hspp_opts = dflags,
                                ms_hspp_buf  = hspp_buf,
                                ms_location  = location,
                                ms_hs_hash   = src_hash,
                                ms_obj_date  = o_mod,
                                ms_dyn_obj_date = dyn_o_mod,
                                ms_parsed_mod   = Nothing,
                                ms_iface_date   = hi_date,
                                ms_hie_date     = hie_date,
                                ms_textual_imps = imps,
                                ms_srcimps      = src_imps }


  -- run the compiler!
  let msg :: Messager
      msg hsc_env _ what _ = oneShotMsg (hsc_logger hsc_env) what
  plugin_hsc_env' <- liftIO $ initializePlugins hsc_env (Just $ ms_mnwib mod_summary)

  -- Need to set the knot-tying mutable variable for interface
  -- files. See GHC.Tc.Utils.TcGblEnv.tcg_type_env_var.
  -- See also Note [hsc_type_env_var hack]
  type_env_var <- liftIO $ newIORef emptyNameEnv
  let plugin_hsc_env = plugin_hsc_env' { hsc_type_env_var = Just (mod, type_env_var) }

  status <- liftIO $ hscRecompStatus (Just msg) plugin_hsc_env mod_summary
                        Nothing Nothing (1, 1)

  return (plugin_hsc_env, mod_summary, status)

runHscTcPhase :: HscEnv -> ModSummary -> IO (FrontendResult, Messages GhcMessage)
runHscTcPhase = hscTypecheckAndGetWarnings

runHscPostTcPhase ::
    HscEnv
  -> ModSummary
  -> FrontendResult
  -> Messages GhcMessage
  -> Maybe Fingerprint
  -> IO HscBackendAction
runHscPostTcPhase hsc_env mod_summary tc_result tc_warnings mb_old_hash = do
        runHsc hsc_env $ do
            hscDesugarAndSimplify mod_summary tc_result tc_warnings mb_old_hash

{-
  logger <- getLogger
  case status of
      HscUpToDate iface _ ->
          do liftIO $ touchObjectFile logger dflags o_file
             -- The .o file must have a later modification date
             -- than the source file (else we wouldn't get Nothing)
             -- but we touch it anyway, to keep 'make' happy (we think).
             setIface iface
             return (RealPhase StopLn, o_file)
      HscRecompNeeded mb_old_hash -> do
        (tc_result, warnings) <- liftIO $
           hscTypecheckAndGetWarnings plugin_hsc_env mod_summary

        -- In the rest of the pipeline use the loaded plugins
        setPlugins (hsc_plugins        plugin_hsc_env)
                   (hsc_static_plugins plugin_hsc_env)
        -- "driver" plugins may have modified the DynFlags so we update them
        setDynFlags (hsc_dflags plugin_hsc_env)

        return (HscPostTc mod_summary tc_result warnings mb_old_hash,
                panic "HscPostTc doesn't have an input filename")
-}




runHsPpPhase :: HscEnv -> FilePath -> FilePath -> FilePath -> IO FilePath
runHsPpPhase hsc_env orig_fn input_fn output_fn = do
    let dflags = hsc_dflags hsc_env
    let logger = hsc_logger hsc_env
    GHC.SysTools.runPp logger dflags
      ( [ GHC.SysTools.Option     orig_fn
      , GHC.SysTools.Option     input_fn
      , GHC.SysTools.FileOption "" output_fn
      ] )
    return output_fn


phaseIfFlag :: Monad m
            => HscEnv
            -> (DynFlags -> Bool)
            -> a
            -> m a
            -> m a
phaseIfFlag hsc_env flag def action =
  if flag (hsc_dflags hsc_env)
    then action
    else return def

-- | Check if the start is *before* the current phase, otherwise skip with a default
phaseIfAfter :: P m => Platform -> Phase -> Phase -> a -> m a -> m a
phaseIfAfter platform start_phase cur_phase def action =
  if start_phase `eqPhase` cur_phase
         || happensBefore platform start_phase cur_phase

    then action
    else return def

preprocessPipeline :: P m => PipeEnv -> HscEnv -> FilePath -> m (DynFlags, FilePath)
preprocessPipeline pipe_env hsc_env input_fn = do
  unlit_fn <-
    runAfter (Unlit HsSrcFile) input_fn $ do
      use (T_Unlit pipe_env hsc_env input_fn)


  (dflags1, warns1) <- use (T_FileArgs hsc_env unlit_fn)
  let hsc_env1 = hscSetFlags dflags1 hsc_env

  (cpp_fn, hsc_env2)
    <- runAfterFlag hsc_env1 (Cpp HsSrcFile) (xopt LangExt.Cpp) (unlit_fn, hsc_env1) $ do
          cpp_fn <- use (T_Cpp pipe_env hsc_env1 unlit_fn)
          (dflags2, _) <- use (T_FileArgs hsc_env1 cpp_fn)
          let hsc_env2 = hscSetFlags dflags2 hsc_env1
          return (cpp_fn, hsc_env2)


  pp_fn <- runAfterFlag hsc_env2 (HsPp HsSrcFile) (gopt Opt_Pp) cpp_fn $
            use (T_HsPp pipe_env hsc_env2 input_fn cpp_fn)

  (dflags3, warns3)
    <- if pp_fn == unlit_fn
          -- Didn't run any preprocessors so don't need to reparse, would be nicer
          -- if `T_FileArgs` recognised this.
          then return (dflags1, warns1)
          else do
            -- Reparse with original hsc_env so that we don't get duplicated options
            use (T_FileArgs hsc_env pp_fn)

  use (T_IO (handleFlagWarnings (hsc_logger hsc_env) dflags3 warns3))
  return (dflags3, pp_fn)


  -- This won't change through the compilation pipeline
  where platform = targetPlatform (hsc_dflags hsc_env)
        runAfter :: P p => Phase
                  -> a -> p a -> p a
        runAfter = phaseIfAfter platform start_phase
        start_phase = startPhase (src_suffix pipe_env)
        runAfterFlag :: P p
                  => HscEnv
                  -> Phase
                  -> (DynFlags -> Bool)
                  -> a
                  -> p a
                  -> p a
        runAfterFlag hsc_env phase flag def action =
          runAfter phase def
           $ phaseIfFlag hsc_env flag def action


fullPipeline :: P p => PipeEnv -> HscEnv -> FilePath -> HscSource -> p (ModIface, Maybe Linkable)
fullPipeline pipe_env hsc_env pp_fn src_flavour = do
  (dflags, input_fn) <- preprocessPipeline pipe_env hsc_env pp_fn
  let hsc_env' = hscSetFlags dflags hsc_env
  (hsc_env_with_plugins, mod_sum, hsc_recomp_status)
    <- use (T_HscRecomp pipe_env hsc_env' input_fn src_flavour)
  res <- hscPipeline pipe_env (hsc_env_with_plugins, mod_sum, hsc_recomp_status)
  checkDynamicToo pipe_env hsc_env pp_fn src_flavour res
  -- Once the pipeline has finished, check to see if -dynamic-too failed and
  -- rerun again if it failed but just the `--dynamic` way.

checkDynamicToo :: P m => PipeEnv -> HscEnv -> FilePath -> HscSource -> (ModIface, Maybe Linkable) -> m (ModIface, Maybe Linkable)
checkDynamicToo pipe_env hsc_env pp_fn src_flavour res = do
  use (T_IO (dynamicTooState (hsc_dflags hsc_env))) >>= \case
      DT_Dont   -> return res
      DT_Dyn    -> return res
      DT_OK     -> return res
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
          | OSMinGW32 <- platformOS (targetPlatform dflags) -> return res
          | otherwise -> do
              use (T_IO (debugTraceMsg logger 4
                        (text "Running the full pipeline again for -dynamic-too")))
              hsc_env' <- use (T_IO (resetHscEnv hsc_env))
              fullPipeline pipe_env hsc_env' pp_fn src_flavour
  where
    dflags = hsc_dflags hsc_env
    logger = hsc_logger hsc_env


resetHscEnv :: HscEnv -> IO HscEnv
resetHscEnv hsc_env = do
  let dflags0 = flip gopt_unset Opt_BuildDynamicToo
                     $ setDynamicNow
                     $ (hsc_dflags hsc_env)
  hsc_env' <- newHscEnv dflags0
  (dbs,unit_state,home_unit,mconstants) <- initUnits (hsc_logger hsc_env) dflags0 Nothing
  dflags1 <- updatePlatformConstants dflags0 mconstants
  unit_env0 <- initUnitEnv (ghcNameVersion dflags1) (targetPlatform dflags1)
  let unit_env = unit_env0
        { ue_home_unit = Just home_unit
        , ue_units     = unit_state
        , ue_unit_dbs  = Just dbs
        }
  let hsc_env'' = hscSetFlags dflags1 $ hsc_env'
       { hsc_unit_env = unit_env
       }
  return hsc_env''





-- Everything after preprocess
hscPipeline :: P m => PipeEnv -> ((HscEnv, ModSummary, HscRecompStatus)) -> m (ModIface, Maybe Linkable)
hscPipeline pipe_env (hsc_env_with_plugins, mod_sum, hsc_recomp_status) = do
  case hsc_recomp_status of
    HscUpToDate iface mb_linkable -> return (iface, mb_linkable)
    HscRecompNeeded mb_old_hash -> do
      (tc_result, warnings) <- use (T_Hsc hsc_env_with_plugins mod_sum)
      hscBackendAction <- use (T_HscPostTc hsc_env_with_plugins mod_sum tc_result warnings mb_old_hash )
      hscBackendPipeline pipe_env hsc_env_with_plugins mod_sum hscBackendAction

hscBackendPipeline :: P m => PipeEnv -> HscEnv -> ModSummary -> HscBackendAction -> m (ModIface, Maybe Linkable)
hscBackendPipeline pipe_env hsc_env mod_sum result =
  case backend (hsc_dflags hsc_env) of
    NoBackend ->
      case result of
        HscUpdate iface ->  return (iface, Nothing)
        HscRecomp {} -> (,) <$> use (T_IO (mkFullIface hsc_env (hscs_partial_iface result) Nothing)) <*> pure Nothing
    -- TODO: Why is there not a linkable?
    -- Interpreter -> (,) <$> use (T_IO (mkFullIface hsc_env (hscs_partial_iface result) Nothing)) <*> pure Nothing
    _ -> do
      res <- hscGenBackendPipeline pipe_env hsc_env mod_sum result
      liftIO (dynamicTooState (hsc_dflags hsc_env)) >>= \case
        DT_OK -> do
          let dflags' = setDynamicNow (hsc_dflags hsc_env) -- set "dynamicNow"
          () <$ hscGenBackendPipeline pipe_env (hscSetFlags dflags' hsc_env) mod_sum result
        _ -> return ()
      return res

hscGenBackendPipeline :: P m
  => PipeEnv
  -> HscEnv
  -> ModSummary
  -> HscBackendAction
  -> m (ModIface, Maybe Linkable)
hscGenBackendPipeline pipe_env hsc_env mod_sum result = do
  let mod_name = moduleName (ms_mod mod_sum)
      src_flavour = (ms_hsc_src mod_sum)
      dflags = hsc_dflags hsc_env
  location <- use (T_IO (getLocationNew pipe_env dflags src_flavour mod_name))
  (fos, miface, mlinkable, o_file) <- use (T_HscBackend pipe_env hsc_env mod_name src_flavour location result)
  final_fp <- hscPostBackendPipeline pipe_env hsc_env (ms_hsc_src mod_sum) (backend (hsc_dflags hsc_env)) (Just location) o_file
  final_linkable <-
    case final_fp of
      -- No object file produced, bytecode or NoBackend
      Nothing -> return mlinkable
      Just o_fp -> do
        unlinked_time <- use (T_IO (liftIO getCurrentTime))
        final_o <- use (T_MergeForeign pipe_env hsc_env (Just location) o_fp fos)
        let !linkable = LM unlinked_time
                                    (ms_mod mod_sum)
                                    [DotO final_o]
        return (Just linkable)
  return (miface, final_linkable)

asPipeline :: P m => Bool -> PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m FilePath
asPipeline use_cpp pipe_env hsc_env location input_fn = do
  use (T_As use_cpp pipe_env hsc_env location input_fn)

viaCPipeline :: P m => Phase -> PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m (Maybe FilePath)
viaCPipeline c_phase pipe_env hsc_env location input_fn = do
  out_fn <- use (T_Cc c_phase pipe_env hsc_env input_fn)
  case stop_phase pipe_env of
    StopC -> return Nothing
    _ -> Just <$> asPipeline False pipe_env hsc_env location out_fn

llvmPipeline :: P m => PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m FilePath
llvmPipeline pipe_env hsc_env location fp = do
  opt_fn <- use (T_LlvmOpt pipe_env hsc_env fp)
  llvmLlcPipeline pipe_env hsc_env location opt_fn

llvmLlcPipeline :: P m => PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m FilePath
llvmLlcPipeline pipe_env hsc_env location opt_fn = do
  llc_fn <- use (T_LlvmLlc pipe_env hsc_env opt_fn)
  llvmManglePipeline pipe_env hsc_env location llc_fn

llvmManglePipeline :: P m  => PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m FilePath
llvmManglePipeline pipe_env hsc_env location llc_fn = do
  mangled_fn <-
    if gopt Opt_NoLlvmMangler (hsc_dflags hsc_env)
      then use (T_LlvmMangle pipe_env hsc_env llc_fn)
      else return llc_fn
  asPipeline False pipe_env hsc_env location mangled_fn

cmmCppPipeline :: P m => PipeEnv -> HscEnv -> FilePath -> m FilePath
cmmCppPipeline pipe_env hsc_env input_fn = do
  output_fn <- use (T_CmmCpp pipe_env hsc_env input_fn)
  cmmPipeline pipe_env hsc_env output_fn

cmmPipeline :: P m => PipeEnv -> HscEnv -> FilePath -> m FilePath
cmmPipeline pipe_env hsc_env input_fn = do
  (fos, output_fn) <- use (T_Cmm pipe_env hsc_env input_fn)
  mo_fn <- hscPostBackendPipeline pipe_env hsc_env HsSrcFile (backend (hsc_dflags hsc_env)) Nothing output_fn
  case mo_fn of
    Nothing -> panic "CMM pipeline - produced no .o file"
    Just mo_fn -> use (T_MergeForeign pipe_env hsc_env Nothing mo_fn fos)



hscPostBackendPipeline :: P m => PipeEnv -> HscEnv -> HscSource -> Backend -> Maybe ModLocation -> FilePath -> m (Maybe FilePath)
hscPostBackendPipeline _ _ HsBootFile _ _ _   = return Nothing
hscPostBackendPipeline _ _ HsigFile _ _ _     = return Nothing
hscPostBackendPipeline pipe_env hsc_env _ bcknd ml input_fn =
  case bcknd of
        ViaC        -> viaCPipeline HCc pipe_env hsc_env ml input_fn
        NCG         -> Just <$> asPipeline False pipe_env hsc_env ml input_fn
        LLVM        -> Just <$> llvmPipeline pipe_env hsc_env ml input_fn
        NoBackend   -> return Nothing
        Interpreter -> return Nothing

-- Pipeline from a given suffix
pipelineStart :: P m => PipeEnv -> HscEnv -> FilePath -> m (Maybe FilePath)
pipelineStart pipe_env hsc_env input_fn =
  fromSuffix (src_suffix pipe_env)
  where
   stop_after = stop_phase pipe_env
   frontend :: P m => HscSource -> m (Maybe FilePath)
   frontend sf = case stop_after of
                    StopPreprocess -> do
                      -- The actual output from preprocessing
                      (_, out_fn) <- preprocessPipeline pipe_env hsc_env input_fn
                      let logger = hsc_logger hsc_env
                      -- Sometimes, a compilation phase doesn't actually generate any output
                      -- (eg. the CPP phase when -fcpp is not turned on).  If we end on this
                      -- stage, but we wanted to keep the output, then we have to explicitly
                      -- copy the file, remembering to prepend a {-# LINE #-} pragma so that
                      -- further compilation stages can tell what the original filename was.
                      -- File name we expected the output to have
                      final_fn <- liftIO $ phaseOutputFilenameNew anyHsc pipe_env hsc_env Nothing
                      when (final_fn /= out_fn) $ do
                        let msg = "Copying `" ++ input_fn ++"' to `" ++ final_fn ++ "'"
                            line_prag = "{-# LINE 1 \"" ++ src_filename pipe_env ++ "\" #-}\n"
                        use (T_IO $ showPass logger msg)
                        use (T_IO $ copyWithHeader line_prag input_fn final_fn)
                      return Nothing
                    _ -> objFromLinkable <$> fullPipeline pipe_env hsc_env input_fn sf
   c :: P m => Phase -> m (Maybe FilePath)
   c phase = viaCPipeline phase pipe_env hsc_env Nothing input_fn
   as :: P m => Bool -> m (Maybe FilePath)
   as use_cpp = Just <$> asPipeline use_cpp pipe_env hsc_env Nothing input_fn

   objFromLinkable (_, Just (LM _ _ [DotO lnk])) = Just lnk
   objFromLinkable _ = Nothing





   fromSuffix :: P m => String -> m (Maybe FilePath)
   fromSuffix "lhs"      = frontend HsSrcFile
   fromSuffix "lhs-boot" = frontend HsBootFile
   fromSuffix "lhsig"    = frontend HsigFile
   fromSuffix "hs"       = frontend HsSrcFile
   fromSuffix "hs-boot"  = frontend HsBootFile
   fromSuffix "hsig"     = frontend HsigFile
   fromSuffix "hscpp"    = frontend HsSrcFile
   fromSuffix "hspp"     = frontend HsSrcFile
   fromSuffix "hc"       = c HCc
   fromSuffix "c"        = c Cc
   fromSuffix "cpp"      = c Ccxx
   fromSuffix "C"        = c Cc
   fromSuffix "m"        = c Cobjc
   fromSuffix "M"        = c Cobjcxx
   fromSuffix "mm"       = c Cobjcxx
   fromSuffix "cc"       = c Ccxx
   fromSuffix "cxx"      = c Ccxx
   fromSuffix "s"        = as False
   fromSuffix "S"        = as True
   fromSuffix "ll"       = Just <$> llvmPipeline pipe_env hsc_env Nothing input_fn
   fromSuffix "bc"       = Just <$> llvmLlcPipeline pipe_env hsc_env Nothing input_fn
   fromSuffix "lm_s"     = Just <$> llvmManglePipeline pipe_env hsc_env Nothing input_fn
   fromSuffix "o"        = return (Just input_fn)
   fromSuffix "cmm"      = Just <$> cmmCppPipeline pipe_env hsc_env input_fn
   fromSuffix "cmmcpp"   = Just <$> cmmPipeline pipe_env hsc_env input_fn
   fromSuffix _          = return (Just input_fn)


{-
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
     -}
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


getLocationNew :: PipeEnv -> DynFlags -> HscSource -> ModuleName -> IO ModLocation
getLocationNew pipe_env dflags src_flavour mod_name = do
    let PipeEnv{ src_basename=basename,
             src_suffix=suff } = pipe_env
    location1 <- mkHomeModLocation2 dflags mod_name basename suff

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
    logMsg logger MCInfo noSrcSpan
      $ withPprStyle defaultUserStyle
      (text "Warning: -rtsopts and -with-rtsopts have no effect with -shared." $$
      text "    Call hs_init_ghc() from your main() function to set these options.")
  linkDynLib logger tmpfs dflags unit_env o_files dep_units


-- -----------------------------------------------------------------------------
-- Running CPP

-- | Run CPP
--
-- UnitEnv is needed to compute MIN_VERSION macros
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
