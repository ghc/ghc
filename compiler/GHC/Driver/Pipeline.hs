{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
--
-- GHC Driver
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module GHC.Driver.Pipeline (
   -- * Run a series of compilation steps in a pipeline, for a
   -- collection of source files.
   oneShot, compileFile,

   -- * Interfaces for the compilation manager (interpreted/batch-mode)
   preprocess,
   compileOne, compileOne',
   compileForeign, compileEmptyStub,

   -- * Linking
   link, linkingNeeded, checkLinkInfo,

   -- * PipeEnv
   PipeEnv(..), mkPipeEnv, phaseOutputFilenameNew,

   -- * Running individual phases
   TPhase(..), runPhase,
   hscPostBackendPhase,

   -- * Constructing Pipelines
   TPipelineClass, MonadUse(..),

   preprocessPipeline, fullPipeline, hscPipeline, hscBackendPipeline, hscPostBackendPipeline,
   hscGenBackendPipeline, asPipeline, viaCPipeline, cmmCppPipeline, cmmPipeline,
   llvmPipeline, llvmLlcPipeline, llvmManglePipeline, pipelineStart,

   -- * Default method of running a pipeline
   runPipeline
) where


import GHC.Prelude

import GHC.Platform

import GHC.Utils.Monad ( MonadIO(liftIO), mapMaybeM )

import GHC.Driver.Main
import GHC.Driver.Env hiding ( Hsc )
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Pipeline.Monad
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Phases
import GHC.Driver.Pipeline.Execute
import GHC.Driver.Pipeline.Phases
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Ppr
import GHC.Driver.Hooks

import GHC.Platform.Ways

import GHC.SysTools
import GHC.Utils.TmpFs

import GHC.Linker.ExtraObj
import GHC.Linker.Static
import GHC.Linker.Types

import GHC.Utils.Outputable
import GHC.Utils.Error
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Exception as Exception
import GHC.Utils.Logger

import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.FastString     ( mkFastString )
import GHC.Data.StringBuffer   ( hPutStringBuffer )
import GHC.Data.Maybe          ( expectJust )

import GHC.Iface.Make          ( mkFullIface )
import GHC.Runtime.Loader      ( initializePlugins )


import GHC.Types.Basic       ( SuccessFlag(..), ForeignSrcLang(..) )
import GHC.Types.Error       ( singleMessage, getMessages )
import GHC.Types.Target
import GHC.Types.SrcLoc
import GHC.Types.SourceFile
import GHC.Types.SourceError

import GHC.Unit
import GHC.Unit.Env
--import GHC.Unit.Finder
--import GHC.Unit.State
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
import Data.Maybe
import Data.Either      ( partitionEithers )
import qualified Data.Set as Set

import Data.Time        ( getCurrentTime )

-- Simpler type synonym for actions in the pipeline monad
type P m = TPipelineClass TPhase m

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
  runPipeline (hsc_hooks hsc_env) preprocess_pipeline

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
                            (tmpDir (hsc_dflags hsc_env))
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
   status <- hscRecompStatus mHscMessage plugin_hsc_env upd_summary
                mb_old_iface mb_old_linkable (mod_index, nmods)
   let pipeline = hscPipeline pipe_env (setDumpPrefix pipe_env plugin_hsc_env, upd_summary, status)
   (iface, linkable) <- runPipeline (hsc_hooks hsc_env) pipeline
   -- See Note [ModDetails and --make mode]
   details <- initModDetails plugin_hsc_env upd_summary iface
   return $! HomeModInfo iface details linkable

 where lcl_dflags  = ms_hspp_opts summary
       location    = ms_location summary
       input_fn    = expectJust "compile:hs" (ml_hs_file location)
       input_fnpp  = ms_hspp_file summary
       mod_graph   = hsc_mod_graph hsc_env0
       needsLinker = needsTemplateHaskellOrQQ mod_graph
       isDynWay    = hasWay (ways lcl_dflags) WayDyn
       isProfWay   = hasWay (ways lcl_dflags) WayProf
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
       upd_summary = summary { ms_hspp_opts = dflags }
       hsc_env = hscSetFlags dflags hsc_env0

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
            pkg_deps  = Set.toList
                          $ Set.unions
                          $ fmap (dep_direct_pkgs . mi_deps . hm_iface)
                          $ home_mod_infos

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
            exe_file  = exeFileName platform staticLink (outputFile_ dflags)

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
      exe_file   = exeFileName platform staticLink (outputFile_ dflags)
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
  let batch_lib_file = if ws `hasNotWay` WayDyn
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
        notStopPreprocess | StopPreprocess <- stop_phase = False
                          | _              <- stop_phase = True
        -- When linking, the -o argument refers to the linker's output.
        -- otherwise, we use it as the name for the pipeline's output.
        output
         | NoBackend <- backend dflags, notStopPreprocess = NoOutputFile
                -- avoid -E -fno-code undesirable interactions. see #20439
         | NoStop <- stop_phase, not (isNoLink ghc_link) = Persistent
                -- -o foo applies to linker
         | isJust mb_o_file = SpecificFile
                -- -o foo applies to the file we are compiling now
         | otherwise = Persistent
        pipe_env = mkPipeEnv stop_phase src output
        pipeline = pipelineStart pipe_env (setDumpPrefix pipe_env hsc_env) src
   runPipeline (hsc_hooks hsc_env) pipeline


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

-----------------------------------------------------------------------------
-- stub .h and .c files (for foreign export support), and cc files.

-- The _stub.c file is derived from the haskell source file, possibly taking
-- into account the -stubdir option.
--
-- The object file created by compiling the _stub.c file is put into a
-- temporary file, which will be later combined with the main .o file
-- (see the MergeForeign phase).
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
        res <- runPipeline (hsc_hooks hsc_env) (pipeline pipe_env hsc_env Nothing stub_c)
        case res of
          -- This should never happen as viaCPipeline should only return `Nothing` when the stop phase is `StopC`.
          -- Future refactoring to not check StopC for this case
          Nothing -> pprPanic "compileForeign" (ppr stub_c)
          Just fp -> return fp

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
  empty_stub <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "c"
  let home_unit = hsc_home_unit hsc_env
      src = text "int" <+> ppr (mkHomeModule home_unit mod_name) <+> text "= 0;"
  writeFile empty_stub (showSDoc dflags (pprCode CStyle src))
  let pipe_env = (mkPipeEnv NoStop empty_stub Persistent) { src_basename = basename}
      pipeline = viaCPipeline HCc pipe_env hsc_env (Just location) empty_stub
  _ <- runPipeline (hsc_hooks hsc_env) pipeline
  return ()


{- Environment Initialisation -}

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
  hscUpdateFlags (\dflags -> dflags { dumpPrefix = src_basename pipe_env ++ "."}) hsc_env

{- The Pipelines -}

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

-- | The preprocessor pipeline
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

  liftIO (handleFlagWarnings (hsc_logger hsc_env) (initDiagOpts dflags3) warns3)
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

-- | The complete compilation pipeline, from start to finish
fullPipeline :: P m => PipeEnv -> HscEnv -> FilePath -> HscSource -> m (ModIface, Maybe Linkable)
fullPipeline pipe_env hsc_env pp_fn src_flavour = do
  (dflags, input_fn) <- preprocessPipeline pipe_env hsc_env pp_fn
  let hsc_env' = hscSetFlags dflags hsc_env
  (hsc_env_with_plugins, mod_sum, hsc_recomp_status)
    <- use (T_HscRecomp pipe_env hsc_env' input_fn src_flavour)
  hscPipeline pipe_env (hsc_env_with_plugins, mod_sum, hsc_recomp_status)

-- | Everything after preprocess
hscPipeline :: P m => PipeEnv ->  ((HscEnv, ModSummary, HscRecompStatus)) -> m (ModIface, Maybe Linkable)
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
        HscRecomp {} -> (,) <$> liftIO (mkFullIface hsc_env (hscs_partial_iface result) Nothing) <*> pure Nothing
    -- TODO: Why is there not a linkable?
    -- Interpreter -> (,) <$> use (T_IO (mkFullIface hsc_env (hscs_partial_iface result) Nothing)) <*> pure Nothing
    _ -> do
      res <- hscGenBackendPipeline pipe_env hsc_env mod_sum result
      when (gopt Opt_BuildDynamicToo (hsc_dflags hsc_env)) $ do
          let dflags' = setDynamicNow (hsc_dflags hsc_env) -- set "dynamicNow"
          () <$ hscGenBackendPipeline pipe_env (hscSetFlags dflags' hsc_env) mod_sum result
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
  let location = ms_location mod_sum
  (fos, miface, mlinkable, o_file) <- use (T_HscBackend pipe_env hsc_env mod_name src_flavour location result)
  final_fp <- hscPostBackendPipeline pipe_env hsc_env (ms_hsc_src mod_sum) (backend (hsc_dflags hsc_env)) (Just location) o_file
  final_linkable <-
    case final_fp of
      -- No object file produced, bytecode or NoBackend
      Nothing -> return mlinkable
      Just o_fp -> do
        unlinked_time <- liftIO (liftIO getCurrentTime)
        final_o <- use (T_MergeForeign pipe_env hsc_env o_fp fos)
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
      then return llc_fn
      else use (T_LlvmMangle pipe_env hsc_env llc_fn)
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
    Just mo_fn -> use (T_MergeForeign pipe_env hsc_env mo_fn fos)

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
                      final_fn <- liftIO $ phaseOutputFilenameNew (Hsc HsSrcFile) pipe_env hsc_env Nothing
                      when (final_fn /= out_fn) $ do
                        let msg = "Copying `" ++ out_fn ++"' to `" ++ final_fn ++ "'"
                            line_prag = "{-# LINE 1 \"" ++ src_filename pipe_env ++ "\" #-}\n"
                        liftIO (showPass logger msg)
                        liftIO (copyWithHeader line_prag out_fn final_fn)
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

Note [The Pipeline Monad]
~~~~~~~~~~~~~~~~~~~~~~~~~

The pipeline is represented as a free monad by the `TPipelineClass` type synonym,
which stipulates the general monadic interface for the pipeline and `MonadUse`, instantiated
to `TPhase`, which indicates the actions available in the pipeline.

The `TPhase` actions correspond to different compiled phases, they are executed by
the 'runPhase' function which interprets each action into IO.

The idea in the future is that we can now implement different instiations of
`TPipelineClass` to give different behaviours that the default `HookedPhase` implementation:

* Additional logging of different phases
* Automatic parrelism (in the style of shake)
* Easy consumption by external tools such as ghcide
* Easier to create your own pipeline and extend existing pipelines.

The structure of the code as a free monad also means that the return type of each
phase is a lot more flexible.

-}
