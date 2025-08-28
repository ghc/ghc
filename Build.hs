#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | GHC builder
--
-- Importantly, it doesn't link with the cabal library but use cabal-install
-- program instead (compared to e.g. Hadrian).
module Main where

import Data.Maybe
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Control.Monad
import Control.Exception (bracket)
import System.Environment
import System.Directory
import System.Process
import System.FilePath
import System.Exit
import System.IO.Unsafe
import Data.Time.Clock
import Data.IORef
import Data.Fixed

main :: IO ()
main = do
  -- detect GHC and cabal-install to use for bootstrapping
  ghc0 <- do
    ghc_path <- fromMaybe "ghc" <$> lookupEnv "GHC"
    findExecutable ghc_path >>= \case
      Nothing -> error ("Couldn't find GHC: " ++ show ghc_path)
      Just x  -> pure (Ghc x)

  cabal <- do
    cabal_path <- fromMaybe "cabal" <$> lookupEnv "CABAL"
    findExecutable cabal_path >>= \case
      Nothing -> error ("Couldn't find cabal-install: " ++ show cabal_path)
      Just x  -> pure (Cabal x)

  ghc0_version <- readCreateProcess (runGhc ghc0 ["--version"]) ""
  msg $ "Bootstrapping GHC version: " ++ init ghc0_version

  msg "Building stage1 GHC program and utility programs"
  buildGhcStage1 defaultGhcBuildOptions cabal ghc0 "_build/stage0/"

  -- now we copy the stage1 compiler and other tools into _build/stage1 and we
  -- generate settings to use the newly installed packages. That's not what
  -- Hadrian does but it's easier for us to nuke the stage1 directory to remove
  -- only stage1's built libs without nuking the stage1 compiler which is slow
  -- to build.
  createDirectoryIfMissing True "_build/stage1/bin"
  createDirectoryIfMissing True "_build/stage1/lib"
  -- createDirectoryIfMissing True "_build/stage1/pkgs"
  cp "_build/stage0/bin/*" "_build/stage1/bin/"
  cp "_build/stage0/lib/template-hsc.h" "_build/stage1/lib/template-hsc.h"
  -- cp "_build/stage0/pkgs/*" "_build/stage1/pkgs/"

  ghc1    <- Ghc    <$> makeAbsolute "_build/stage1/bin/ghc"
  ghcPkg1 <- GhcPkg <$> makeAbsolute "_build/stage1/bin/ghc-pkg"
  deriveConstants <- DeriveConstants <$> makeAbsolute "_build/stage1/bin/deriveConstants"
  genapply <- GenApply <$> makeAbsolute "_build/stage1/bin/genapply"
  genprimop <- GenPrimop <$> makeAbsolute "_build/stage1/bin/genprimopcode"
  ghcToolchain <- GhcToolchain <$> makeAbsolute "_build/stage1/bin/ghc-toolchain"

  -- generate settings based on stage1 compiler settings: stage1 should never be
  -- a cross-compiler! Hence we reuse the same target platform as the bootstrap
  -- compiler.
  stage0_target_triple <- ghcTargetTriple ghc0
  let stage1_settings = emptySettings
                          { settingsTriple = Just stage0_target_triple
                          }

  void $ readCreateProcess (shell $ "rm -fR _build/stage1/pkgs; ln -s $(pwd)/_build/stage0/cabal/packagedb/ghc-* _build/stage1/pkgs") ""
  generateSettings ghcToolchain stage1_settings "_build/stage1/"

  msg "Building boot libraries with stage1 compiler..."
  buildBootLibraries cabal ghc1 ghcPkg1 deriveConstants genapply genprimop defaultGhcBuildOptions "_build/stage1/"

  msg "Building stage2 GHC program"
  createDirectoryIfMissing True "_build/stage2"
  buildGhcStage2 defaultGhcBuildOptions cabal ghc1 "_build/stage2/"

  -- Reuse stage1 settings for stage2 and copy stage1's built boot package for
  -- stage2 to use.
  createDirectoryIfMissing True "_build/stage2/lib/"
  void $ readCreateProcess (shell $ "rm -fR _build/stage2/pkgs; ln -s $(pwd)/_build/stage2/cabal/packagedb/ghc-* _build/stage2/pkgs") ""
  cp "_build/stage1/lib/settings" "_build/stage2/lib/settings"

  -- TODO: in the future we want to generate different settings for cross
  -- targets and build boot libraries with stage2 using these settings. In any
  -- case, we need non-cross boot packages to build plugins for use with
  -- -fplugin-library.


  -- Finally create bindist directory
  msg "Creating bindist"
  createDirectoryIfMissing True "_build/bindist/lib/"
  createDirectoryIfMissing True "_build/bindist/bin/"
  createDirectoryIfMissing True "_build/bindist/pkgs/"
  cp "_build/stage2/bin/*" "_build/bindist/bin/"
  cp "_build/stage2/lib/*" "_build/bindist/lib/"
  cp "_build/stage2/pkgs/*" "_build/bindist/pkgs/"
  cp "driver/ghc-usage.txt" "_build/bindist/lib/"
  cp "driver/ghci-usage.txt" "_build/bindist/lib/"

  msg "Done"


-- | Build stage1 GHC program
buildGhcStage1 :: GhcBuildOptions -> Cabal -> Ghc -> FilePath -> IO ()
buildGhcStage1 = buildGhcStage True

-- | Build stage2 GHC program
buildGhcStage2 :: GhcBuildOptions -> Cabal -> Ghc -> FilePath -> IO ()
buildGhcStage2 = buildGhcStage False

-- | Build GHC program
buildGhcStage :: Bool -> GhcBuildOptions -> Cabal -> Ghc -> FilePath -> IO ()
buildGhcStage booting opts cabal ghc0 dst = do
  let src = dst </> "src"
  prepareGhcSources opts src

  msg "  - Building GHC and utility programs..."

  let builddir = dst </> "cabal"
  createDirectoryIfMissing True builddir

  -- we need to augment the current environment to pass HADRIAN_SETTINGS
  -- environment variable to ghc-boot's Setup.hs script.
  (arch,os) <- ghcTargetArchOS ghc0
  stage1_ghc_boot_settings <- do
    commit_id <- readCreateProcess (proc "git" ["rev-parse", "HEAD"]) ""
    -- we infer stage1's host platform from stage0's settings
    let settings =
          [ ("hostPlatformArch",    arch)
          , ("hostPlatformOS",      os)
          , ("cProjectGitCommitId", commit_id)
          , ("cProjectVersion",     Text.unpack $ gboVersion opts)
          , ("cProjectVersionInt",  Text.unpack $ gboVersionInt opts)
          , ("cProjectPatchLevel",  Text.unpack $ gboVersionPatchLevel opts)
          , ("cProjectPatchLevel1", Text.unpack $ gboVersionPatchLevel1 opts)
          , ("cProjectPatchLevel2", Text.unpack $ gboVersionPatchLevel2 opts)
          ] :: [(String,String)]
    pure (show settings)

  current_env <- getEnvironment
  let stage1_env = ("HADRIAN_SETTINGS", stage1_ghc_boot_settings) : current_env

  let cabal_project_path = "cabal.project.stage" ++ (if booting then "1" else "2")

  -- makeCabalProject cabal_project_path (if booting then stage1_project else stage2_project)

  -- the targets
  let targets
        | booting =
           [ "ghc-bin:ghc"
           , "ghc-pkg:ghc-pkg"
           , "genprimopcode:genprimopcode"
           , "deriveConstants:deriveConstants"
           , "genapply:genapply"
           , "ghc-toolchain-bin:ghc-toolchain-bin"
           , "unlit:unlit"
           , "hsc2hs:hsc2hs"
           ]
        | otherwise =
           [ "ghc-bin:ghc"
           , "ghc-pkg:ghc-pkg"
           , "genprimopcode:genprimopcode"
           , "deriveConstants:deriveConstants"
           , "genapply:genapply"
           , "unlit:unlit"
           , "hsc2hs:hsc2hs"
           , "hp2ps:hp2ps"
           , "hpc-bin:hpc"
           , "iserv:iserv"
           , "runghc:runghc"
           , "ghc-bignum:ghc-bignum"
           ]

  let build_cmd = (runCabal cabal $
              [ "build"
              , "--project-file=" ++ cabal_project_path
              , "--builddir=" ++ builddir
              , "-j"
              , "--with-compiler=" ++ ghcPath ghc0
              ] ++ targets)
              { env = Just stage1_env
              }

  (exit_code, cabal_stdout, cabal_stderr) <- readCreateProcessWithExitCode build_cmd ""
  writeFile (dst </> "cabal.stdout") cabal_stdout
  writeFile (dst </> "cabal.stderr") cabal_stderr
  case exit_code of
    ExitSuccess -> pure ()
    ExitFailure n -> do
      putStrLn $ "cabal-install failed with error code: " ++ show n
      putStrLn cabal_stdout
      putStrLn cabal_stderr
      putStrLn $ "Logs can be found in \"" ++ (dst </> "cabal.{stdout,stderr}\"")
      exitFailure

  msg "  - Copying programs and generating GHC settings..."
  let listbin_cmd p = runCabal cabal
        [ "list-bin"
        , "--project-file=" ++ cabal_project_path
        , "--with-compiler=" ++ ghcPath ghc0
        , "--builddir=" ++ builddir
        , p
        , "-v0"
        ]
  let copy_bin target bin = do
        (list_bin_exit_code, list_bin_stdout, list_bin_stderr) <- readCreateProcessWithExitCode (listbin_cmd target) ""
        case list_bin_exit_code of
          ExitSuccess
            | (bin_src:_) <- lines list_bin_stdout
            -> cp bin_src (dst </> "bin" </> bin)
          _ -> do
            putStrLn $ "Failed to run cabal list-bin for the target: " ++ show target
            putStrLn list_bin_stderr
            exitFailure
  createDirectoryIfMissing True (dst </> "bin")

  copy_bin "ghc-bin:ghc"     "ghc"
  copy_bin "ghc-pkg:ghc-pkg" "ghc-pkg"
  copy_bin "unlit:unlit"     "unlit"
  copy_bin "hsc2hs:hsc2hs"   "hsc2hs"
  -- always install these tools: they are needed to build the ghc library (e.g.
  -- for a different target)
  copy_bin "deriveConstants:deriveConstants"     "deriveConstants"
  copy_bin "genprimopcode:genprimopcode"         "genprimopcode"
  copy_bin "genapply:genapply"                   "genapply"

  createDirectoryIfMissing True (dst </> "lib")
  cp "utils/hsc2hs/data/template-hsc.h" (dst </> "lib/template-hsc.h")

  unless booting $ do
    copy_bin "hp2ps:hp2ps"   "hp2ps"
    copy_bin "hpc-bin:hpc"   "hpc"
    copy_bin "runghc:runghc" "runghc"
    copy_bin "iserv:iserv"   "ghc-iserv" -- vanilla iserv

  when booting $ do
    copy_bin "ghc-toolchain-bin:ghc-toolchain-bin" "ghc-toolchain"

  -- initialize empty global package database
  -- pkgdb <- makeAbsolute (dst </> "pkgs")
  -- ghcpkg <- GhcPkg <$> makeAbsolute (dst </> "bin/ghc-pkg")
  -- initEmptyDB ghcpkg pkgdb


-- | Prepare GHC sources in the given directory
prepareGhcSources :: GhcBuildOptions -> FilePath -> IO ()
prepareGhcSources opts dst = do
  msg $ "  - Preparing sources in " ++ dst ++ "..."
  createDirectoryIfMissing True dst
  createDirectoryIfMissing True ("compiler/MachRegs")

  -- cp "./libraries"    dst
  -- cp "./compiler/*"   (dst </> "libraries/ghc/")
  -- cp "./rts"          (dst </> "libraries/")
  -- cp "./ghc"          (dst </> "ghc-bin")
  -- cp "./utils"        dst

  cp "./config.sub"   ("rts/")
  cp "./config.guess" ("rts/")

  -- These needs to shared
  cp "rts/include/rts/Bytecodes.h"            "compiler/"
  cp "rts/include/rts/storage/ClosureTypes.h" "compiler/"
  cp "rts/include/rts/storage/FunTypes.h"     "compiler/"
  cp "rts/include/stg/MachRegs.h"             "compiler/"
  cp "rts/include/stg/MachRegs/*.h"           "compiler/MachRegs/"

  -- shared among ghc-internal rts and unlit
  cp "utils/fs/fs.h" "libraries/ghc-internal/include"
  cp "utils/fs/fs.c" "libraries/ghc-internal/cbits"
  cp "utils/fs/fs.*" "rts/"
  cp "utils/fs/fs.*" "utils/unlit/"

  python <- findExecutable "python" >>= \case
    Nothing -> error "Couldn't find 'python'"
    Just r -> pure r

  void $ readCreateProcess (proc python
    [ "rts/gen_event_types.py"
    , "--event-types-defines"
    , "rts/include/rts/EventLogConstants.h"
    ]) ""

  void $ readCreateProcess (proc python
    [ "rts/gen_event_types.py"
    , "--event-types-array"
    , "rts/include/rts/EventTypes.h"
    ]) ""

  -- substitute variables in files
  let subst fin fout rs = do
        t <- Text.readFile fin
        Text.writeFile fout (List.foldl' (\v (needle,rep) -> Text.replace needle rep v) t rs)
  let subst_in f = subst (f <.> "in") f
  let common_substs =
        [ (,) "@ProjectVersion@"       (gboVersion opts)
        , (,) "@ProjectVersionMunged@" (gboVersionMunged opts)
        , (,) "@ProjectVersionForLib@" (gboVersionForLib opts)
        , (,) "@ProjectPatchLevel1@"   (gboVersionPatchLevel1 opts)
        , (,) "@ProjectPatchLevel2@"   (gboVersionPatchLevel2 opts)
        , (,) "@ProjectVersionInt@"    (gboVersionInt opts)
        ]
      llvm_substs =
        [ (,) "@LlvmMinVersion@" (gboLlvmMinVersion opts)
        , (,) "@LlvmMaxVersion@" (gboLlvmMaxVersion opts)
        ]
      boot_th_substs =
        [ (,) "@Suffix@"     ""
        , (,) "@SourceRoot@" "."
        ]

  subst_in ("ghc/ghc-bin.cabal") common_substs
  subst_in ("compiler/ghc.cabal") common_substs
  subst_in ("libraries/ghc-boot/ghc-boot.cabal") common_substs
  subst_in ("libraries/ghc-boot-th/ghc-boot-th.cabal") (common_substs ++ boot_th_substs)
  subst_in ("libraries/ghc-heap/ghc-heap.cabal") common_substs
  subst_in ("libraries/template-haskell/template-haskell.cabal") common_substs
  subst_in ("libraries/ghci/ghci.cabal") common_substs

  -- This is only used for a warning message. Nuke the check!
  subst_in ("compiler/GHC/CmmToLlvm/Version/Bounds.hs") llvm_substs

  subst_in ("utils/ghc-pkg/ghc-pkg.cabal") common_substs
  subst_in ("utils/iserv/iserv.cabal") common_substs
  subst_in ("utils/runghc/runghc.cabal") common_substs

  subst_in ("libraries/ghc-internal/ghc-internal.cabal") common_substs
  subst_in ("libraries/ghc-experimental/ghc-experimental.cabal") common_substs
  subst_in ("libraries/base/base.cabal") common_substs
  subst_in ("rts/include/ghcversion.h") common_substs


buildBootLibraries :: Cabal -> Ghc -> GhcPkg -> DeriveConstants -> GenApply -> GenPrimop -> GhcBuildOptions -> FilePath -> IO ()
buildBootLibraries cabal ghc ghcpkg derive_constants genapply genprimop opts dst = do
  src <- makeAbsolute (dst </> "src")
  prepareGhcSources opts src

  -- Build the RTS
  src_rts <- makeAbsolute "rts"
  build_dir <- makeAbsolute (dst </> "cabal")
  ghcversionh <- makeAbsolute (src_rts </> "include/ghcversion.h")

  -- FIXME: could we build a cross compiler, simply by not reading this from the boot compiler, but passing it in?
  -- target_triple <- ghcTargetTriple ghc
  -- let to_triple = \case
  --       [arch,vendor,os] -> (arch,vendor,os)
  --       t -> error $ "Triple expected but got: " ++ show t
  -- let (arch,vendor,os) = to_triple $ words $ map (\c -> if c == '-' then ' ' else c) target_triple

  -- let cabal_project_rts_path = dst </> "cabal.project-rts"
  --     -- cabal's code handling escaping is bonkers. We need to wrap the whole
  --     -- option into \" otherwise it does weird things (like keeping only the
  --     -- last double-quote).
  -- let def_string k v = "  ghc-options: \"-optc-D" ++ k ++ "=\\\"" ++ v ++ "\\\"\""
  -- let def        k v = "  ghc-options: \"-optc-D" ++ k ++ "=" ++ v ++ "\""
  -- let rts_options =
  --       [ "package rts"
  --       , def_string "ProjectVersion" (Text.unpack (gboVersionInt opts))
  --       , def_string "RtsWay"            "v"
  --       , def_string "HostPlatform"      target_triple
  --       , def_string "HostArch"          arch
  --       , def_string "HostOS"            os
  --       , def_string "HostVendor"        vendor
  --       , def_string "BuildPlatform"     "FIXME"
  --       , def_string "BuildArch"         "FIXME"
  --       , def_string "BuildOS"           "FIXME"
  --       , def_string "BuildVendor"       "FIXME"
  --       , def_string "GhcUnregisterised" "FIXME"
  --       , def_string "TablesNextToCode"  "FIXME"
  --         -- Set the namespace for the rts fs functions
  --       , def "FS_NAMESPACE" "rts"
  --         -- This is stupid, I can't seem to figure out how to set this in cabal
  --         -- this needs to be fixed in cabal.
  --       , if os == "darwin"
  --         then "  flags: +tables-next-to-code +leading-underscore"
  --         else "  flags: +tables-next-to-code"
  --         -- FIXME: we should
  --         -- FIXME: deal with libffi (add package?)
  --         --
  --         -- FIXME: we should make tables-next-to-code  optional here and in the
  --         -- compiler settings. Ideally, GHC should even look into the rts's
  --         -- ghcautoconf.h to check whether TABLES_NEXT_TO_CODE is defined or
  --         -- not. It would be cleaner than duplicating this information into the
  --         -- settings (similar to what we do with platform constants).

  --         -- FIXME: Cabal doesn't like when flags are on separate lines like
  --         -- this:
  --         --  flags: +use-system-libffi
  --         --  flags: +tables-next-to-code
  --         -- Apparently it makes it ignore the first set of flags...
  --         -- See https://github.com/haskell/cabal/issues/10767
  --       ]

  -- makeCabalProject cabal_project_rts_path $
  --       [ "package-dbs: clear, global"
  --       , ""
  --       , "packages:"
  --       , "  " ++ src </> "libraries/rts"
  --       , ""
  --       , "benchmarks: False"
  --       , "tests: False"
  --       , "allow-boot-library-installs: True"
  --       , "active-repositories: :none"
  --       , ""
  --       , "package *"
  --       , "  library-vanilla: True"
  --       , "  shared: False"
  --       , "  executable-profiling: False"
  --       , "  executable-dynamic: False"
  --       , "  executable-static: False"
  --       , ""
  --       ] ++ rts_options

  let build_rts_cmd = runCabal cabal
        [ "build"
        , "--project-file=cabal.project.stage2"
        , "rts:rts"
        , "--with-compiler=" ++ ghcPath ghc
        , "--with-hc-pkg=" ++ ghcPkgPath ghcpkg
        , "--ghc-options=\"-ghcversion-file=" ++ ghcversionh ++ "\""
        , "--builddir=" ++ build_dir
        ]

  -- FIXME: deriveConstants requires ghcautoconf.h and ghcplatform.h but these
  -- files are generated by the configure script of the RTS...
  -- We use the following hack:
  --  1. run cabal until it fails. This should generate the headers we need before failing.
  --  2. use deriveConstants to generate the other files
  --  3. rerun cabal to build the rts

  msg "  - Generating headers and sources..."

  -- first run is expected to fail because of misssing headers
  (_exit_code, rts_conf_stdout, rts_conf_stderr) <- readCreateProcessWithExitCode build_rts_cmd ""
  writeFile (dst </> "rts-conf.stdout") rts_conf_stdout
  writeFile (dst </> "rts-conf.stderr") rts_conf_stderr
  ghcplatform_dir <- do
    ghcplatform_h <- readCreateProcess (shell ("find " ++ build_dir ++ " -name ghcplatform.h")) ""
    case ghcplatform_h of
      "" -> do
        putStrLn "Couldn't find ghcplatform.h"
        exitFailure
      d  -> pure (takeDirectory d)

  -- deriving constants
  let derived_constants = src_rts </> "include/DerivedConstants.h"
  withSystemTempDirectory "derive-constants" $ \tmp_dir -> do
    target <- getTarget ghc
    void $ readCreateProcess (runDeriveConstants derive_constants
      [ "--gen-header"
      , "-o",  derived_constants
      , "--target-os", target
      , "--tmpdir", tmp_dir
      , "--gcc-program", "cc" -- FIXME
      , "--nm-program", "nm"   -- FIXME
      , "--objdump-program", "objdump" -- FIXME
      -- pass `-fcommon` to force symbols into the common section. If they
      -- end up in the ro data section `nm` won't list their size, and thus
      -- derivedConstants will fail. Recent clang (e.g. 16) will by default
      -- use `-fno-common`.
      , "--gcc-flag", "-fcommon"
      , "--gcc-flag", "-I" ++ src_rts </> "include"
      , "--gcc-flag", "-I" ++ src_rts
      , "--gcc-flag", "-I" ++ ghcplatform_dir
      ]) ""

  -- Generate autoapply
  let run_genapply args out = writeFile out =<< readCreateProcess (runGenApply genapply args) ""
  run_genapply [derived_constants]         (src_rts </> "AutoApply.cmm")
  run_genapply [derived_constants, "-V16"] (src_rts </> "AutoApply_V16.cmm")
  run_genapply [derived_constants, "-V32"] (src_rts </> "AutoApply_V32.cmm")
  run_genapply [derived_constants, "-V64"] (src_rts </> "AutoApply_V64.cmm")

  -- Generate primop code for ghc-internal
  --
  -- Note that this can't be done in a Setup.hs for ghc-internal because
  -- cabal-install can't build Setup.hs because it depends on base, Cabal, etc.
  -- libraries that aren't built yet.
  let primops_txt    = "compiler/GHC/Builtin/primops.txt"
  let primops_txt_pp = primops_txt <.> ".pp"
  primops <- readCreateProcess (shell $ "cc -E -undef -traditional -P -x c " ++ primops_txt_pp) ""
  writeFile primops_txt primops
  writeFile ("libraries/ghc-internal/src/GHC/Internal/Prim.hs") =<< readCreateProcess (runGenPrimop genprimop ["--make-haskell-source"]) primops
  writeFile ("libraries/ghc-internal/src/GHC/Internal/PrimopWrappers.hs") =<< readCreateProcess (runGenPrimop genprimop ["--make-haskell-wrappers"]) primops

  -- build boot libraries: ghc-internal, base...

  -- let boot_libs_env = dst </> "boot-libs.env"
  -- let build_boot_cmd = runCabal cabal
  --       [ "install"
  --       , "--lib"
  --       , "--package-env=" ++ boot_libs_env
  --       , "--force-reinstalls"
  --       , "--project-file=" ++ cabal_project_bootlibs_path
  --       , "--with-compiler=" ++ ghcPath ghc
  --       , "--with-hc-pkg=" ++ ghcPkgPath ghcpkg
  --       , "--ghc-options=\"-ghcversion-file=" ++ ghcversionh ++ "\""
  --       , "--builddir=" ++ build_dir
  --       , "-j"

  --         -- targets
  --       -- , "rts"
  --       -- , "ghc-internal"
  --       -- , "ghc-experimental"
  --       -- , "ghc-compact"
  --       -- , "base"
  --       -- , "stm"
  --       -- , "system-cxx-std-lib"
  --       --   -- shallow compat packages over ghc-internal
  --       -- , "ghc-prim"
  --       -- , "ghc-bignum"
  --       -- , "integer-gmp"
  --       -- , "template-haskell"
  --       --   -- target dependencies
  --       -- , "ghc-boot-th" -- dependency of template-haskell
  --       -- , "pretty"      -- dependency of ghc-boot-th
  --       --   -- other boot libraries used by tests
  --       -- , "array"
  --       -- , "binary"
  --       -- , "bytestring"
  --       -- , "Cabal"
  --       -- , "Cabal-syntax"
  --       -- , "containers"
  --       -- , "deepseq"
  --       -- , "directory"
  --       -- , "exceptions"
  --       -- , "file-io"
  --       -- , "filepath"
  --       -- , "hpc"
  --       -- , "mtl"
  --       -- , "os-string"
  --       -- , "parsec"
  --       -- , "process"
  --       -- , "semaphore-compat"
  --       -- , "text"
  --       -- , "time"
  --       -- , "transformers"
  --       -- , "unix" -- FIXME: we'd have to install Win32 for Windows target. Maybe --libs could install dependencies too..
  --       --   -- ghc related
  --       -- , "ghc-boot"
  --       -- , "ghc-heap"
  --       -- , "ghc-platform"
  --       -- , "ghc-toolchain" -- some test requires this
  --       -- , "ghci"
  --       , "ghc:ghc"
  --       ]

  -- msg "  - Building boot libraries..."
  -- (boot_exit_code, boot_stdout, boot_stderr) <- readCreateProcessWithExitCode build_boot_cmd ""
  -- writeFile (dst </> "boot-libs.stdout") boot_stdout
  -- writeFile (dst </> "boot-libs.stderr") boot_stderr
  -- case boot_exit_code of
  --   ExitSuccess -> pure ()
  --   ExitFailure r -> do
  --     putStrLn $ "Failed to build boot libraries with error code " ++ show r
  --     putStrLn boot_stdout
  --     putStrLn boot_stderr
  --     putStrLn $ "Logs can be found in " ++ dst ++ "boot-libs.{stdout,stderr}"
  --     exitFailure

  -- -- The libraries have been installed globally.
  -- boot_libs_env_lines <- lines <$> readFile boot_libs_env
  -- -- FIXME: Sometimes the package environment contains the path to the global db,
  -- -- sometimes not... I don't know why yet.
  -- (global_db,pkg_ids) <- case drop 2 boot_libs_env_lines of
  --   [] -> error "Unexpected empty package environment"
  --   (x:xs)
  --     | not ("package-db" `List.isPrefixOf` x)
  --     -> do
  --       putStrLn "For some reason cabal-install didn't generate a valid package environment (package-db is missing)."
  --       putStrLn "It happens sometimes for unknown reasons... Rerun 'make' to workaround this..."
  --       exitFailure
  --     | otherwise -> pure (drop 11 x, map (drop 11) xs)
  -- putStrLn $ "We've built boot libraries in " ++ global_db ++ ":"
  -- mapM_ (putStrLn . ("  - " ++)) pkg_ids

  -- copy the libs in another db
  -- createDirectoryIfMissing True (dst </> "pkgs")
  -- void $ readCreateProcess (shell $ "ln -s _build/stage2/cabal/packagedb/ghc-9.13 _build/stage2/pkgs") ""

  -- initEmptyDB ghcpkg (dst </> "pkgs")
  -- let pkg_root = takeDirectory global_db
  -- forM_ pkg_ids $ \pid -> do
  --   conf <- Text.readFile (global_db </> pid <.> "conf")
  --   -- replace full path with ${pkgroot}
  --   -- NOTE: GHC assumes that pkgroot is just one directory above the directory
  --   -- containing the package db. In our case where everything is at the same
  --   -- level in "pkgs" we need to re-add "/pkgs"
  --   Text.writeFile (dst </> "pkgs" </> pid <.> "conf")
  --                  (Text.replace (Text.pack pkg_root) "${pkgroot}/pkgs" conf)
  --   cp (pkg_root </> pid) (dst </> "pkgs")

  -- void $ readCreateProcess (runGhcPkg ghcpkg ["recache", "--package-db=" ++ (dst </> "pkgs")]) ""



---------------------------
-- Options
---------------------------

data GhcBuildOptions = GhcBuildOptions
  { gboVersion            :: !Text -- ^ GHC version
  , gboVersionInt         :: !Text -- ^ GHC version as an Int
  , gboVersionMunged      :: !Text -- ^ GHC version "munged"
  , gboVersionForLib      :: !Text -- ^ GHC version for libraries?
  , gboVersionPatchLevel  :: !Text -- ^ GHC patchlevel version
  , gboVersionPatchLevel1 :: !Text -- ^ GHC patchlevel1 version
  , gboVersionPatchLevel2 :: !Text -- ^ GHC patchlevel2 version
  , gboLlvmMinVersion     :: !Text -- ^ Min LLVM version supported
  , gboLlvmMaxVersion     :: !Text -- ^ Max LLVM version supported
  }

defaultGhcBuildOptions :: GhcBuildOptions
defaultGhcBuildOptions = GhcBuildOptions
  { gboVersion            = "9.13"
  , gboVersionInt         = "913"
  , gboVersionMunged      = "9.13"
  , gboVersionForLib      = "9.1300"
  , gboVersionPatchLevel  = "0"
  , gboVersionPatchLevel1 = "0"
  , gboVersionPatchLevel2 = "0"
  , gboLlvmMinVersion     = "13"
  , gboLlvmMaxVersion     = "20"
  }



---------------------------
-- Utilities
---------------------------

{-# NOINLINE init_time #-}
init_time :: IORef UTCTime
init_time = unsafePerformIO (newIORef =<< getCurrentTime)

-- | Display a message to the user with some timestamp
msg :: String -> IO ()
msg x = do
  it <- readIORef init_time
  t <- getCurrentTime
  let d = realToFrac (nominalDiffTimeToSeconds (diffUTCTime t it)) :: Centi
  let stp = "[" ++ show d ++ "s] "
  putStrLn (stp ++ replicate (6 - length stp) ' ' ++ x)

-- Avoid FilePath blindness by using type aliases for programs.
newtype Ghc = Ghc FilePath
newtype GhcPkg = GhcPkg FilePath
newtype GhcToolchain = GhcToolchain FilePath
newtype Cabal = Cabal FilePath
newtype DeriveConstants = DeriveConstants FilePath
newtype GenApply = GenApply FilePath
newtype GenPrimop = GenPrimop FilePath

runGhc :: Ghc -> [String] -> CreateProcess
runGhc (Ghc f) = proc f

ghcPath :: Ghc -> FilePath
ghcPath (Ghc x) = x

runGhcPkg :: GhcPkg -> [String] -> CreateProcess
runGhcPkg (GhcPkg f) = proc f

runGhcToolchain :: GhcToolchain -> [String] -> CreateProcess
runGhcToolchain (GhcToolchain f) = proc f

ghcPkgPath :: GhcPkg -> FilePath
ghcPkgPath (GhcPkg x) = x

runCabal :: Cabal -> [String] -> CreateProcess
runCabal (Cabal f) = proc f

runDeriveConstants :: DeriveConstants -> [String] -> CreateProcess
runDeriveConstants (DeriveConstants f) = proc f

runGenApply :: GenApply -> [String] -> CreateProcess
runGenApply (GenApply f) = proc f

runGenPrimop :: GenPrimop -> [String] -> CreateProcess
runGenPrimop (GenPrimop f) = proc f

-- | Copy
--
-- Recursively, force overwrite, and preserve timestamps (important for package
-- dbs)
cp :: String -> String -> IO ()
cp src dst = void (readCreateProcess (shell $ "cp -rfp " ++ src ++ " " ++ dst) "")


makeCabalProject :: FilePath -> [String] -> IO ()
makeCabalProject path xs = writeFile path $ unlines (xs ++ common)
  where
    common =
        [ ""
        , "program-options"
        , "  ghc-options: -fhide-source-paths -j"
        ]


withSystemTempDirectory :: String -> (String -> IO a) -> IO a
withSystemTempDirectory prefix = do
    bracket
        (do
            tmpdir <- getTemporaryDirectory
            let dir = tmpdir </> prefix
            createDirectory dir
            return dir
        )
        removeDirectoryRecursive

initEmptyDB :: GhcPkg -> FilePath -> IO ()
initEmptyDB ghcpkg pkgdb = do
  -- don't try to recreate the DB if it already exist as it would fail
  exists <- doesDirectoryExist pkgdb
  unless exists $ void $ readCreateProcess (runGhcPkg ghcpkg ["init", pkgdb]) ""

-- | Retrieve GHC's target arch/os from ghc --info
ghcTargetArchOS :: Ghc -> IO (String,String)
ghcTargetArchOS ghc = do
  is <- read <$> readCreateProcess (runGhc ghc ["--info"]) "" :: IO [(String,String)]
  let arch = fromMaybe (error "Couldn't read 'target arch' setting") (lookup "target arch" is)
  let os   = fromMaybe (error "Couldn't read 'target os' setting") (lookup "target os" is)
  pure (arch,os)

-- | Retrieve GHC's target as linux, or darwin
getTarget :: Ghc -> IO String
getTarget ghc = ghcTargetArchOS ghc >>= \case
  (_,"OSDarwin") -> pure "darwin"
  (_,"OSLinux") -> pure "linux"
  _ -> error "Unsupported target"

-- | Retrieve GHC's target triple
ghcTargetTriple :: Ghc -> IO String
ghcTargetTriple ghc = do
  is <- read <$> readCreateProcess (runGhc ghc ["--info"]) "" :: IO [(String,String)]
  pure $ fromMaybe (error "Couldn't read 'Target platform setting") (lookup "Target platform" is)


data Settings = Settings
  { settingsTriple            :: Maybe String
  , settingsTargetPrefix      :: Maybe String
  , settingsLocallyExecutable :: Maybe Bool
  , settingsLlvmTriple        :: Maybe String
  , settingsCc                :: ProgOpt
  , settingsCxx               :: ProgOpt
  , settingsCpp               :: ProgOpt
  , settingsHsCpp             :: ProgOpt
  , settingsJsCpp             :: ProgOpt
  , settingsCmmCpp            :: ProgOpt
  , settingsCcLink            :: ProgOpt
  , settingsAr                :: ProgOpt
  , settingsRanlib            :: ProgOpt
  , settingsNm                :: ProgOpt
  , settingsReadelf           :: ProgOpt
  , settingsMergeObjs         :: ProgOpt
  , settingsWindres           :: ProgOpt
  -- Note we don't actually configure LD into anything but
  -- see #23857 and #22550 for the very unfortunate story.
  , settingsLd                :: ProgOpt
  , settingsUnregisterised    :: Maybe Bool
  , settingsTablesNextToCode  :: Maybe Bool
  , settingsUseLibFFIForAdjustors :: Maybe Bool
  , settingsLdOverride        :: Maybe Bool
  }

-- | Program specifier from the command-line.
data ProgOpt = ProgOpt
  { poPath :: Maybe String
  -- ^ Refers to the path to an executable, or simply the
  -- executable name.
  , poFlags :: Maybe [String]
  }

emptyProgOpt :: ProgOpt
emptyProgOpt = ProgOpt Nothing Nothing

emptySettings :: Settings
emptySettings = Settings
    { settingsTriple    = Nothing
    , settingsTargetPrefix = Nothing
    , settingsLocallyExecutable = Nothing
    , settingsLlvmTriple = Nothing
    , settingsCc        = po0
    , settingsCxx       = po0
    , settingsCpp       = po0
    , settingsHsCpp     = po0
    , settingsJsCpp     = po0
    , settingsCmmCpp    = po0
    , settingsCcLink    = po0
    , settingsAr        = po0
    , settingsRanlib    = po0
    , settingsNm        = po0
    , settingsReadelf   = po0
    , settingsMergeObjs = po0
    , settingsWindres   = po0
    , settingsLd        = po0
    , settingsUnregisterised = Nothing
    , settingsTablesNextToCode = Nothing
    , settingsUseLibFFIForAdjustors = Nothing
    , settingsLdOverride = Nothing
    }
  where
    po0 = emptyProgOpt

generateSettings :: GhcToolchain -> Settings -> FilePath -> IO ()
generateSettings ghc_toolchain Settings{..} dst = do
  createDirectoryIfMissing True (dst </> "lib")

  let gen_settings_path = dst </> "lib/settings.generated"

  mbCC <- lookupEnv "CC" >>= \case
    Just cc -> pure ["--cc", cc]
    Nothing -> pure []
  mbCXX <- lookupEnv "CXX" >>= \case
    Just cxx -> pure ["--cxx", cxx]
    Nothing -> pure []
  let common_args =
       [ "--output-settings"
       , "-o", gen_settings_path
       ] ++ mbCC ++ mbCXX

  let opt m f = fmap f m
  let args = mconcat (catMaybes
       [ opt settingsTriple $ \x -> ["--triple", x]
       -- FIXME: add other options for ghc-toolchain from Settings
       ]) ++ common_args

  (exit_code, toolchain_stdout, toolchain_stderr) <- readCreateProcessWithExitCode (runGhcToolchain ghc_toolchain args) ""
  writeFile (dst </> "ghc-toolchain.stdout") toolchain_stdout
  writeFile (dst </> "ghc-toolchain.stderr") toolchain_stderr
  case exit_code of
    ExitSuccess -> pure ()
    ExitFailure n -> do
      putStrLn $ "ghc-toolchain failed with error code: " ++ show n
      putStrLn toolchain_stdout
      putStrLn toolchain_stderr
      putStrLn $ "Logs can be found in \"" ++ (dst </> "ghc-toolchain.{stdout,stderr}\"")
      exitFailure

  -- fixup settings generated by ghc-toolchain
  kvs <- (Map.fromList . read) <$> readFile gen_settings_path :: IO (Map String String)
  let kvs' = Map.insert "Relative Global Package DB" "../pkgs"
             $ Map.insert "Support SMP" "NO" -- FIXME: this depends on the different ways used to build the RTS!
             $ Map.insert "RTS ways" "v"     -- FIXME: this depends on the different ways used to build the RTS!
             $ Map.insert "otool command" "otool" -- FIXME: this should just arguably be a default in the settings in GHC, and not require the settings file?
             $ Map.insert "install_name_tool command" "install_name_tool"
             $ kvs
  writeFile (dst </> "lib/settings") (show $ Map.toList kvs')
