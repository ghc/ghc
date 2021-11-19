{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
#include <ghcplatform.h>

{- Functions for providing the default interpretation of the 'TPhase' actions
-}
module GHC.Driver.Pipeline.Execute where

import GHC.Prelude
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import GHC.Driver.Hooks
import Control.Monad.Trans.Reader
import GHC.Driver.Pipeline.Monad
import GHC.Driver.Pipeline.Phases
import GHC.Driver.Env hiding (Hsc)
import GHC.Unit.Module.Location
import GHC.Driver.Phases
import GHC.Unit.Module.Name ( ModuleName )
import GHC.Unit.Types
import GHC.Types.SourceFile
import GHC.Unit.Module.Status
import GHC.Unit.Module.ModIface
import GHC.Linker.Types
import GHC.Driver.Backend
import GHC.Driver.Session
import GHC.Driver.CmdLine
import GHC.Unit.Module.ModSummary
import qualified GHC.LanguageExtensions as LangExt
import GHC.Types.SrcLoc
import GHC.Driver.Main
import GHC.Tc.Types
import GHC.Types.Error
import GHC.Driver.Errors.Types
import GHC.Fingerprint
import GHC.Utils.Logger
import GHC.Utils.TmpFs
import GHC.Platform
import Data.List (intercalate, isInfixOf)
import GHC.Unit.Env
import GHC.SysTools.Info
import GHC.Utils.Error
import Data.Maybe
import GHC.CmmToLlvm.Mangler
import GHC.SysTools
import GHC.Utils.Panic.Plain
import System.Directory
import System.FilePath
import GHC.Utils.Misc
import GHC.Utils.Outputable
import qualified Control.Exception as Exception
import GHC.Unit.Info
import GHC.Unit.State
import GHC.Unit.Home
import GHC.Data.Maybe
import GHC.Iface.Make
import Data.Time
import GHC.Driver.Config.Parser
import GHC.Parser.Header
import GHC.Data.StringBuffer
import GHC.Types.SourceError
import GHC.Unit.Finder
import GHC.Runtime.Loader
import Data.IORef
import GHC.Types.Name.Env
import GHC.Platform.Ways
import GHC.Platform.ArchOS
import GHC.CmmToLlvm.Base ( llvmVersionList )
import {-# SOURCE #-} GHC.Driver.Pipeline (compileForeign, compileEmptyStub)
import GHC.Settings
import System.IO
import GHC.Linker.ExtraObj
import GHC.Linker.Dynamic
import Data.Version
import GHC.Utils.Panic
import GHC.Unit.Module.Env
import GHC.Driver.Env.KnotVars
import GHC.Driver.Config.Finder
import GHC.Rename.Names

newtype HookedUse a = HookedUse { runHookedUse :: (Hooks, PhaseHook) -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch) via (ReaderT (Hooks, PhaseHook) IO)

instance MonadUse TPhase HookedUse where
  use fa = HookedUse $ \(hooks, (PhaseHook k)) ->
    case runPhaseHook hooks of
      Nothing -> k fa
      Just (PhaseHook h) -> h fa

-- | The default mechanism to run a pipeline, see Note [The Pipeline Monad]
runPipeline :: Hooks -> HookedUse a -> IO a
runPipeline hooks pipeline = runHookedUse pipeline (hooks, PhaseHook runPhase)

-- | Default interpretation of each phase, in terms of IO.
runPhase :: TPhase out -> IO out
runPhase (T_Unlit pipe_env hsc_env inp_path) = do
  out_path <- phaseOutputFilenameNew (Cpp HsSrcFile) pipe_env hsc_env Nothing
  runUnlitPhase hsc_env inp_path out_path
runPhase (T_FileArgs hsc_env inp_path) = getFileArgs hsc_env inp_path
runPhase (T_Cpp pipe_env hsc_env inp_path) = do
  out_path <- phaseOutputFilenameNew (HsPp HsSrcFile) pipe_env hsc_env Nothing
  runCppPhase hsc_env inp_path out_path
runPhase (T_HsPp pipe_env hsc_env origin_path inp_path) = do
  out_path <- phaseOutputFilenameNew (Hsc HsSrcFile) pipe_env hsc_env Nothing
  runHsPpPhase hsc_env origin_path inp_path out_path
runPhase (T_HscRecomp pipe_env hsc_env fp hsc_src) = do
  runHscPhase pipe_env hsc_env fp hsc_src
runPhase (T_Hsc hsc_env mod_sum) = runHscTcPhase hsc_env mod_sum
runPhase (T_HscPostTc hsc_env ms fer m mfi) =
  runHscPostTcPhase hsc_env ms fer m mfi
runPhase (T_HscBackend pipe_env hsc_env mod_name hsc_src location x) = do
  runHscBackendPhase pipe_env hsc_env mod_name hsc_src location x
runPhase (T_CmmCpp pipe_env hsc_env input_fn) = do
  output_fn <- phaseOutputFilenameNew Cmm pipe_env hsc_env Nothing
  doCpp (hsc_logger hsc_env)
        (hsc_tmpfs hsc_env)
        (hsc_dflags hsc_env)
        (hsc_unit_env hsc_env)
        False{-not raw-}
        input_fn output_fn
  return output_fn
runPhase (T_Cmm pipe_env hsc_env input_fn) = do
  let dflags = hsc_dflags hsc_env
  let next_phase = hscPostBackendPhase HsSrcFile (backend dflags)
  output_fn <- phaseOutputFilenameNew next_phase pipe_env hsc_env Nothing
  mstub <- hscCompileCmmFile hsc_env input_fn output_fn
  stub_o <- mapM (compileStub hsc_env) mstub
  let foreign_os = (maybeToList stub_o)
  return (foreign_os, output_fn)

runPhase (T_Cc phase pipe_env hsc_env input_fn) = runCcPhase phase pipe_env hsc_env input_fn
runPhase (T_As cpp pipe_env hsc_env location input_fn) = do
  runAsPhase cpp pipe_env hsc_env location input_fn
runPhase (T_LlvmOpt pipe_env hsc_env input_fn) =
  runLlvmOptPhase pipe_env hsc_env input_fn
runPhase (T_LlvmLlc pipe_env hsc_env input_fn) =
  runLlvmLlcPhase pipe_env hsc_env input_fn
runPhase (T_LlvmMangle pipe_env hsc_env input_fn) =
  runLlvmManglePhase pipe_env hsc_env input_fn
runPhase (T_MergeForeign pipe_env hsc_env input_fn fos) =
  runMergeForeign pipe_env hsc_env input_fn fos

runLlvmManglePhase :: PipeEnv -> HscEnv -> FilePath -> IO [Char]
runLlvmManglePhase pipe_env hsc_env input_fn = do
      let next_phase = As False
      output_fn <- phaseOutputFilenameNew next_phase pipe_env hsc_env Nothing
      let dflags = hsc_dflags hsc_env
      llvmFixupAsm (targetPlatform dflags) input_fn output_fn
      return output_fn

runMergeForeign :: PipeEnv -> HscEnv -> FilePath -> [FilePath] -> IO FilePath
runMergeForeign _pipe_env hsc_env input_fn foreign_os = do
     if null foreign_os
       then return input_fn
       else do
         -- Work around a binutil < 2.31 bug where you can't merge objects if the output file
         -- is one of the inputs
         new_o <- newTempName (hsc_logger hsc_env)
                              (hsc_tmpfs hsc_env)
                              (tmpDir (hsc_dflags hsc_env))
                              TFL_CurrentModule "o"
         copyFile input_fn new_o
         let dflags = hsc_dflags hsc_env
             logger = hsc_logger hsc_env
         let tmpfs = hsc_tmpfs hsc_env
         joinObjectFiles logger tmpfs dflags (new_o : foreign_os) input_fn
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
        llvmOpts = case llvmOptLevel dflags of
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

    GHC.SysTools.runLlvmLlc logger dflags
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
        optIdx = max 0 $ min 2 $ llvmOptLevel dflags  -- ensure we're in [0,2]
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

    GHC.SysTools.runLlvmOpt logger dflags
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
                    = (GHC.SysTools.runAs, getAssemblerInfo logger dflags)

        asmInfo <- get_asm_info

        let cmdline_include_paths = includePaths dflags
        let pic_c_flags = picCCOpts dflags

        output_fn <- phaseOutputFilenameNew StopLn pipe_env hsc_env location

        -- we create directories for the object file, because it
        -- might be a hierarchical module.
        createDirectoryIfMissing True (takeDirectory output_fn)

        let global_includes = [ GHC.SysTools.Option ("-I" ++ p)
                              | p <- includePathsGlobal cmdline_include_paths ]
        let local_includes = [ GHC.SysTools.Option ("-iquote" ++ p)
                             | p <- includePathsQuote cmdline_include_paths ++
                                includePathsQuoteImplicit cmdline_include_paths]
        let runAssembler inputFilename outputFilename
              = withAtomicRename outputFilename $ \temp_outputFilename ->
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
  pkgs <- if hcc then getHCFilePackages input_fn else return []

  -- add package include paths even if we're just compiling .c
  -- files; this is the Value Add(TM) that using ghc instead of
  -- gcc gives you :)
  ps <- mayThrowUnitErr (preloadUnitsInfo' unit_env pkgs)
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

  let cc_opt | llvmOptLevel dflags >= 2 = [ "-O2" ]
             | llvmOptLevel dflags >= 1 = [ "-O" ]
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

  ghcVersionH <- getGhcVersionPathName dflags unit_env

  GHC.SysTools.runCc (phaseForeignLanguage cc_phase) logger tmpfs dflags (
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
runHscBackendPhase pipe_env hsc_env mod_name src_flavour location result = do
  let dflags = hsc_dflags hsc_env
      logger = hsc_logger hsc_env
      o_file = if dynamicNow dflags then ml_dyn_obj_file location else ml_obj_file location -- The real object file
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
                 compileEmptyStub dflags hsc_env basename location mod_name

               -- In the case of hs-boot files, generate a dummy .o-boot
               -- stamp file for the benefit of Make
               HsBootFile -> touchObjectFile logger dflags o_file
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
              final_iface <- mkFullIface hsc_env partial_iface Nothing
              hscMaybeWriteIface logger dflags True final_iface mb_old_iface_hash location

              (hasStub, comp_bc, spt_entries) <- hscInteractive hsc_env cgguts mod_location

              stub_o <- case hasStub of
                        Nothing -> return []
                        Just stub_c -> do
                            stub_o <- compileStub hsc_env stub_c
                            return [DotO stub_o]

              let hs_unlinked = [BCOs comp_bc spt_entries]
              unlinked_time <- getCurrentTime
              let !linkable = LM unlinked_time (mkHomeModule (hsc_home_unit hsc_env) mod_name)
                             (hs_unlinked ++ stub_o)
              return ([], final_iface, Just linkable, panic "interpreter")
          _ -> do
              output_fn <- phaseOutputFilenameNew next_phase pipe_env hsc_env (Just location)
              (outputFilename, mStub, foreign_files, cg_infos) <-
                hscGenHardCode hsc_env cgguts mod_location output_fn
              final_iface <- mkFullIface hsc_env partial_iface (Just cg_infos)

              -- See Note [Writing interface files]
              hscMaybeWriteIface logger dflags False final_iface mb_old_iface_hash mod_location

              stub_o <- mapM (compileStub hsc_env) mStub
              foreign_os <-
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
  (hspp_buf,mod_name,imps,src_imps, ghc_prim_imp) <- do
    buf <- hGetStringBuffer input_fn
    let imp_prelude = xopt LangExt.ImplicitPrelude dflags
        popts = initParserOpts dflags
        rn_pkg_qual = renameRawPkgQual (hsc_unit_env hsc_env)
        rn_imps = fmap (\(rpk, lmn@(L _ mn)) -> (rn_pkg_qual mn rpk, lmn))
    eimps <- getImports popts imp_prelude buf input_fn (basename <.> suff)
    case eimps of
        Left errs -> throwErrors (GhcPsMessage <$> errs)
        Right (src_imps,imps, ghc_prim_imp, L _ mod_name) -> return
              (Just buf, mod_name, rn_imps imps, rn_imps src_imps, ghc_prim_imp)

  -- Take -o into account if present
  -- Very like -ohi, but we must *only* do this if we aren't linking
  -- (If we're linking then the -o applies to the linked thing, not to
  -- the object file for one module.)
  -- Note the nasty duplication with the same computation in compileFile above
  location <- mkOneShotModLocation pipe_env dflags src_flavour mod_name
  let o_file = ml_obj_file location -- The real object file
      hi_file = ml_hi_file location
      hie_file = ml_hie_file location
      dyn_o_file = ml_dyn_obj_file location

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
                                ms_ghc_prim_import = ghc_prim_imp,
                                ms_textual_imps = imps,
                                ms_srcimps      = src_imps }


  -- run the compiler!
  let msg :: Messager
      msg hsc_env _ what _ = oneShotMsg (hsc_logger hsc_env) what
  plugin_hsc_env' <- initializePlugins hsc_env (Just $ ms_mnwib mod_summary)

  -- Need to set the knot-tying mutable variable for interface
  -- files. See GHC.Tc.Utils.TcGblEnv.tcg_type_env_var.
  -- See also Note [hsc_type_env_var hack]
  type_env_var <- newIORef emptyNameEnv
  let plugin_hsc_env = plugin_hsc_env' { hsc_type_env_vars = knotVarsFromModuleEnv (mkModuleEnv [(mod, type_env_var)]) }

  status <- hscRecompStatus (Just msg) plugin_hsc_env mod_summary
                        Nothing Nothing (1, 1)

  return (plugin_hsc_env, mod_summary, status)

-- | Calculate the ModLocation from the provided DynFlags. This function is only used
-- in one-shot mode and therefore takes into account the effect of -o/-ohi flags
-- (which do nothing in --make mode)
mkOneShotModLocation :: PipeEnv -> DynFlags -> HscSource -> ModuleName -> IO ModLocation
mkOneShotModLocation pipe_env dflags src_flavour mod_name = do
    let PipeEnv{ src_basename=basename,
             src_suffix=suff } = pipe_env
    let location1 = mkHomeModLocation2 fopts mod_name basename suff

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

    let dynohi = dynOutputHi dflags
        location4 | Just fn <- dynohi = location3{ ml_dyn_hi_file = fn }
                  | otherwise         = location3

    -- Take -o into account if present
    -- Very like -ohi, but we must *only* do this if we aren't linking
    -- (If we're linking then the -o applies to the linked thing, not to
    -- the object file for one module.)
    -- Note the nasty duplication with the same computation in compileFile
    -- above
    let expl_o_file = outputFile_ dflags
        expl_dyn_o_file  = dynOutputFile_ dflags
        location5 | Just ofile <- expl_o_file
                  , let dyn_ofile = fromMaybe (ofile -<.> dynObjectSuf_ dflags) expl_dyn_o_file
                  , isNoLink (ghcLink dflags)
                  = location4 { ml_obj_file = ofile
                              , ml_dyn_obj_file = dyn_ofile }
                  | Just dyn_ofile <- expl_dyn_o_file
                  = location4 { ml_dyn_obj_file = dyn_ofile }
                  | otherwise = location4
    return location5
    where
      fopts = initFinderOpts dflags

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

phaseOutputFilenameNew :: Phase -- ^ The next phase
                       -> PipeEnv
                       -> HscEnv
                       -> Maybe ModLocation -- ^ A ModLocation, if we are compiling a Haskell source file
                       -> IO FilePath
phaseOutputFilenameNew next_phase pipe_env hsc_env maybe_loc = do
  let PipeEnv{stop_phase, src_basename, output_spec} = pipe_env
  let dflags = hsc_dflags hsc_env
      logger = hsc_logger hsc_env
      tmpfs = hsc_tmpfs hsc_env
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
  -- 1. If we are generating object files for a .hs file, then return the odir as the ModLocation
  -- will have been modified to point to the accurate locations
 | StopLn <- next_phase, Just loc <- maybe_location  =
      return $ if dynamicNow dflags then ml_dyn_obj_file loc
                                    else ml_obj_file loc
 -- 2. If output style is persistant then
 | is_last_phase, Persistent   <- output = persistent_fn
 -- 3. Specific file is only set when outputFile is set by -o
 -- If we are in dynamic mode but -dyno is not set then write to the same path as
 -- -o with a .dyn_* extension. This case is not triggered for object files which
 -- are always handled by the ModLocation.
 | is_last_phase, SpecificFile <- output =
    return $
      if dynamicNow dflags
        then case dynOutputFile_ dflags of
                Nothing -> let ofile = getOutputFile_ dflags
                               new_ext = case takeExtension ofile of
                                            "" -> "dyn"
                                            ext -> "dyn_" ++ tail ext
                           in replaceExtension ofile new_ext
                Just fn -> fn
        else getOutputFile_ dflags
 | keep_this_output                      = persistent_fn
 | Temporary lifetime <- output          = newTempName logger tmpfs (tmpDir dflags) lifetime suffix
 | otherwise                             = newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule
   suffix
    where
          getOutputFile_ dflags = case outputFile_ dflags of
                                    Nothing -> pprPanic "SpecificFile: No filename" (ppr $ (dynamicNow dflags, outputFile_ dflags, dynOutputFile_ dflags))
                                    Just fn -> fn

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
        rmodel | gopt Opt_PIC dflags         = "pic"
               | positionIndependent dflags  = "pic"
               | ways dflags `hasWay` WayDyn = "dynamic-no-pic"
               | otherwise                   = "static"

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
            then do macro_stub <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "h"
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


compileStub :: HscEnv -> FilePath -> IO FilePath
compileStub hsc_env stub_c = compileForeign hsc_env LangC stub_c


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
          script <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "ldscript"
          cwd <- getCurrentDirectory
          let o_files_abs = map (\x -> "\"" ++ (cwd </> x) ++ "\"") o_files
          writeFile script $ "INPUT(" ++ unwords o_files_abs ++ ")"
          ld_r [GHC.SysTools.FileOption "" script]
     else if toolSettings_ldSupportsFilelist toolSettings'
     then do
          filelist <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "filelist"
          writeFile filelist $ unlines o_files
          ld_r [GHC.SysTools.Option "-filelist",
                GHC.SysTools.FileOption "" filelist]
     else
          ld_r (map (GHC.SysTools.FileOption "") o_files)

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


-- -----------------------------------------------------------------------------
-- Misc.



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
