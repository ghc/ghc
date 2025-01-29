#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wall #-}

-- | GHC builder
--
-- Importantly, it doesn't link with the cabal library but use cabal-install
-- program instead (compared to e.g. Hadrian).
module Main where

import Data.Maybe
import Data.List qualified as List
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

  ghc1    <- Ghc    <$> makeAbsolute "_build/stage0/bin/ghc"
  ghcPkg1 <- GhcPkg <$> makeAbsolute "_build/stage0/bin/ghc-pkg"
  deriveConstants <- DeriveConstants <$> makeAbsolute "_build/stage0/bin/deriveConstants"
  genapply <- GenApply <$> makeAbsolute "_build/stage0/bin/genapply"
  genprimop <- GenPrimop <$> makeAbsolute "_build/stage0/bin/genprimopcode"

  msg "Building boot libraries with stage1 compiler..."
  buildBootLibraries cabal ghc1 ghcPkg1 deriveConstants genapply genprimop defaultGhcBuildOptions "_build/stage1/"

  -- now we copy the stage1 compiler into _build/stage1 and we generate settings
  -- to use the newly installed packages. That's not what Hadrian does but it's
  -- easier for us to nuke the stage1 directory to remove only stage1's built
  -- libs without nuking the stage1 compiler which is slow to build.
  createDirectoryIfMissing True "_build/stage1/bin"
  cp "_build/stage0/bin/ghc" "_build/stage1/bin/ghc"
  cp "_build/stage0/bin/ghc-pkg" "_build/stage1/bin/ghc-pkg"
  createDirectoryIfMissing True "_build/stage1/lib"
  cp "_build/stage0/lib/settings" "_build/stage1/lib/settings"

  msg "Building stage2 GHC program"
  createDirectoryIfMissing True "_build/stage2"
  ghc1' <- Ghc <$> makeAbsolute "_build/stage1/bin/ghc"
  buildGhcStage2 defaultGhcBuildOptions cabal ghc1' "_build/stage2/"

  -- copy stage1's boot packages for stage2 to use.
  cp "_build/stage1/pkgs/*" "_build/stage2/pkgs"

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

  let builddir = dst </> "cabal"
  createDirectoryIfMissing True builddir

  -- we need to augment the current environment to pass HADRIAN_SETTINGS
  -- environment variable to ghc-boot's Setup.hs script.
  stage0_settings <- read <$> readCreateProcess (runGhc ghc0 ["--info"]) ""
  stage1_ghc_boot_settings <- do
    commit_id <- readCreateProcess (proc "git" ["rev-parse", "HEAD"]) ""
    -- we infer stage1's host platform from stage0's settings
    let settings =
          [ ("hostPlatformArch",    fromMaybe (error "Couldn't read 'target arch' setting") (lookup "target arch" stage0_settings))
          , ("hostPlatformOS",      fromMaybe (error "Couldn't read 'target os' setting") (lookup "target os" stage0_settings))
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

  let cabal_project_path = dst </> "cabal.project-ghc"

  let stage1_project =
        [ "packages:"
        , "  " ++ src </> "ghc-bin/"
        , "  " ++ src </> "libraries/ghc/"
        , "  " ++ src </> "libraries/directory/"
        , "  " ++ src </> "libraries/file-io/"
        , "  " ++ src </> "libraries/filepath/"
        , "  " ++ src </> "libraries/ghc-platform/"
        , "  " ++ src </> "libraries/ghc-boot/"
        , "  " ++ src </> "libraries/ghc-boot-th/"
        , "  " ++ src </> "libraries/ghc-heap"
        , "  " ++ src </> "libraries/ghci"
        , "  " ++ src </> "libraries/os-string/"
        , "  " ++ src </> "libraries/process/"
        , "  " ++ src </> "libraries/semaphore-compat"
        , "  " ++ src </> "libraries/time"
        , "  " ++ src </> "libraries/unix/"
        , "  " ++ src </> "libraries/Win32/"
        , "  " ++ src </> "utils/ghc-pkg"
        , "  " ++ src </> "utils/hsc2hs"
        , "  " ++ src </> "utils/unlit"
        , "  " ++ src </> "utils/genprimopcode/"
        , "  " ++ src </> "utils/genapply/"
        , "  " ++ src </> "utils/deriveConstants/"
        , ""
        , "benchmarks: False"
        , "tests: False"
        , "allow-boot-library-installs: True"
        , ""
        , "package *"
        , "  library-vanilla: True"
        , "  shared: False"
        , "  executable-profiling: False"
        , "  executable-dynamic: False"
        , "  executable-static: True"
        , ""
        , "package ghc-boot-th"
        , "  flags: +bootstrap"
        , ""
          -- allow template-haskell with newer ghc-boot-th
        , "allow-newer: ghc-boot-th"
        , ""
        , "constraints:"
          -- FIXME: template-haskell 2.23 is too recent when booting with 9.8.4
        , "  template-haskell <= 2.22"
        ]

  let stage2_project =
        [ "packages:"
        , "  " ++ src </> "ghc-bin/"
        , "  " ++ src </> "libraries/deepseq/"
        , "  " ++ src </> "libraries/hpc/"
        , "  " ++ src </> "libraries/stm/"
        , "  " ++ src </> "libraries/text/"
        , "  " ++ src </> "libraries/ghc/"
        , "  " ++ src </> "libraries/directory/"
        , "  " ++ src </> "libraries/file-io/"
        , "  " ++ src </> "libraries/filepath/"
        , "  " ++ src </> "libraries/haskeline/"
        , "  " ++ src </> "libraries/terminfo/"
        , "  " ++ src </> "libraries/ghc-platform/"
        , "  " ++ src </> "libraries/ghc-boot/"
        , "  " ++ src </> "libraries/ghc-boot-th/"
        , "  " ++ src </> "libraries/ghc-heap"
        , "  " ++ src </> "libraries/ghci"
        , "  " ++ src </> "libraries/os-string/"
        , "  " ++ src </> "libraries/process/"
        , "  " ++ src </> "libraries/semaphore-compat"
        , "  " ++ src </> "libraries/time"
        , "  " ++ src </> "libraries/unix/"
        , "  " ++ src </> "libraries/Win32/"
        , "  " ++ src </> "utils/ghc-pkg"
        , "  " ++ src </> "utils/hsc2hs"
        , "  " ++ src </> "utils/unlit"
        , "  " ++ src </> "utils/genprimopcode/"
        , "  " ++ src </> "utils/genapply/"
        , "  " ++ src </> "utils/deriveConstants/"
        , ""
        , "benchmarks: False"
        , "tests: False"
        , "allow-boot-library-installs: True"
          -- we need even after booting because cabal thinks `template-haskell` isn't reinstallable otherwise
        , ""
        , "package *"
        , "  library-vanilla: True"
        , "  shared: False"
        , "  executable-profiling: False"
        , "  executable-dynamic: False"
        , "  executable-static: True"
        , ""
          -- allow template-haskell with newer ghc-boot-th
        , "allow-newer: ghc-boot-th"
        , ""
        , "package ghc"
        , "  flags: +internal-interpreter"
        , ""
        , "package ghci"
        , "  flags: +internal-interpreter"
        , ""
        , "package ghc-bin"
        , "  flags: +internal-interpreter"
        , ""
        , "package haskeline"
        , "  flags: -terminfo" -- FIXME: should be enabled but I don't have the static libs for terminfo on ArchLinux...
        , ""
        , "package text"
             -- FIXME: avoid having to deal with system-cxx-std-lib fake package for now
        , "  flags: -simdutf"
        , ""
        ]

  makeCabalProject cabal_project_path (if booting then stage1_project else stage2_project)

  -- the targets
  let targets
        | booting =
           [ "ghc-bin:ghc"
           , "ghc-pkg:ghc-pkg"
           , "genprimopcode:genprimopcode"
           , "deriveConstants:deriveConstants"
           , "genapply:genapply"
           ]
        | otherwise = 
           [ "ghc-bin:ghc"
           , "ghc-pkg:ghc-pkg"
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

  when booting $ do
    copy_bin "deriveConstants:deriveConstants" "deriveConstants"
    copy_bin "genprimopcode:genprimopcode"     "genprimopcode"
    copy_bin "genapply:genapply"               "genapply"

  -- initialize empty global package database
  pkgdb <- makeAbsolute (dst </> "pkgs")
  ghcpkg <- GhcPkg <$> makeAbsolute (dst </> "bin/ghc-pkg")
  initEmptyDB ghcpkg pkgdb


  -- generate settings based on stage1 compiler settings
  createDirectoryIfMissing True (dst </> "lib")
  let stage1_settings = makeStage1Settings stage0_settings
  writeFile (dst </> "lib/settings") (show stage1_settings)

  -- try to run the stage1 compiler (no package db yet, so just display the
  -- version)
  (test_exit_code, _test_stdout, _test_stderr) <- readCreateProcessWithExitCode (proc (dst </> "bin/ghc") ["--version"]) ""
  case test_exit_code of
    ExitSuccess -> pure ()
    ExitFailure n -> do
      putStrLn $ "Failed to run stage1 compiler with error code " ++ show n
      exitFailure


-- TODO:
-- - headers shared between different packages should be a common dependency
-- - event types should be generated by Setup.hs
-- package versions: do they need to be all the same?

-- | Prepare GHC sources in the given directory
prepareGhcSources :: GhcBuildOptions -> FilePath -> IO ()
prepareGhcSources opts dst = do
  msg $ "  - Preparing sources in " ++ dst ++ "..."
  createDirectoryIfMissing True dst 
  createDirectoryIfMissing True (dst </> "libraries/ghc/MachRegs")

  cp "./libraries"    dst
  cp "./compiler/*"   (dst </> "libraries/ghc/")
  cp "./rts"          (dst </> "libraries/")
  cp "./ghc"          (dst </> "ghc-bin")
  cp "./utils"        dst

  cp "./config.sub"   (dst </> "libraries/rts/")
  cp "./config.guess" (dst </> "libraries/rts/")

  -- These needs to shared
  cp "rts/include/rts/Bytecodes.h"            (dst </> "libraries/ghc/")
  cp "rts/include/rts/storage/ClosureTypes.h" (dst </> "libraries/ghc/")
  cp "rts/include/rts/storage/FunTypes.h"     (dst </> "libraries/ghc/")
  cp "rts/include/stg/MachRegs.h"             (dst </> "libraries/ghc/")
  cp "rts/include/stg/MachRegs/*.h"           (dst </> "libraries/ghc/MachRegs/")

  -- shared among ghc-internal rts and unlit
  cp "utils/fs/fs.h" (dst </> "libraries/ghc-internal/include")
  cp "utils/fs/fs.c" (dst </> "libraries/ghc-internal/cbits")
  cp "utils/fs/fs.*" (dst </> "libraries/rts/")
  cp "utils/fs/fs.*" (dst </> "utils/unlit/")

  python <- findExecutable "python" >>= \case
    Nothing -> error "Couldn't find 'python'"
    Just r -> pure r

  void $ readCreateProcess (proc python
    [ "rts/gen_event_types.py"
    , "--event-types-defines"
    , dst </> "libraries/rts/include/rts/EventLogConstants.h"
    ]) ""

  void $ readCreateProcess (proc python
    [ "rts/gen_event_types.py"
    , "--event-types-array"
    , dst </> "libraries/rts/include/rts/EventTypes.h"
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

  subst_in (dst </> "ghc-bin/ghc-bin.cabal") common_substs
  subst_in (dst </> "libraries/ghc/ghc.cabal") common_substs
  subst_in (dst </> "libraries/ghc-boot/ghc-boot.cabal") common_substs
  subst_in (dst </> "libraries/ghc-boot-th/ghc-boot-th.cabal") (common_substs ++ boot_th_substs)
  subst_in (dst </> "libraries/ghc-heap/ghc-heap.cabal") common_substs
  subst_in (dst </> "libraries/template-haskell/template-haskell.cabal") common_substs
  subst_in (dst </> "libraries/ghci/ghci.cabal") common_substs

  -- This is only used for a warning message. Nuke the check!
  subst_in (dst </> "libraries/ghc/GHC/CmmToLlvm/Version/Bounds.hs") llvm_substs

  subst_in (dst </> "utils/ghc-pkg/ghc-pkg.cabal") common_substs

  subst_in (dst </> "libraries/ghc-internal/ghc-internal.cabal") common_substs
  subst_in (dst </> "libraries/base/base.cabal") common_substs
  subst_in (dst </> "libraries/rts/include/ghcversion.h") common_substs


-- | Generate settings for stage1 compiler, based on given settings (stage0's
-- compiler settings)
makeStage1Settings :: [(String,String)] -> [(String,String)]
makeStage1Settings in_settings = out_settings
  where
    -- keep the previous setting, fail if it doesn't exist
    keep_fail s = keep_def s (error ("Couldn't find setting "<> show s))

    -- keep the previous setting, default to the given value if it doesn't exist
    keep_def s d = case lookup s in_settings of
      Nothing -> (s,d)
      Just v  -> (s,v)

    -- use the previous setting, or if it doesn't exist use the setting for the
    -- second key. Fail if both don't exist. This is useful to support
    -- bootstrapping with old compilers that mingled some settings.
    keep_or_fail s s2 = case lookup s in_settings of
      Nothing -> case lookup s2 in_settings of
        Nothing -> error ("Couldn't find any of " <> show s <> " and " <> show s2)
        Just v  -> (s,v)
      Just v  -> (s,v)

    --FIXME: we default to these flags for Cmm CPP, otherwise CPP fails
    -- with error: missing '(' after "__has_feature"
    -- because we pass `-traditional` while compiling Apply.cmm (in TSANUtils.h)
    default_cpp_flags = "-E"

    out_settings =
        [ keep_fail "C compiler command"
        , keep_fail "C compiler flags"
        , keep_fail "C++ compiler command"
        , keep_fail "C++ compiler flags"
        , keep_fail "C compiler link flags"
        , keep_fail "C compiler supports -no-pie"
        , keep_or_fail "CPP command" "Haskell CPP command"
        , keep_def "CPP flags" default_cpp_flags
        , keep_fail "Haskell CPP command"
        , keep_fail "Haskell CPP flags"
        , keep_or_fail "JavaScript CPP command" "Haskell CPP command"
        , keep_or_fail "JavaScript CPP flags" "Haskell CPP flags"
        , keep_or_fail "C-- CPP command" "Haskell CPP command"
        , keep_def "C-- CPP flags"   default_cpp_flags
        , keep_def "C-- CPP supports -g0" "NO"
        , keep_fail "ld supports compact unwind"
        , keep_fail "ld supports filelist"
        , keep_fail "ld supports single module"
        , keep_fail "ld is GNU ld"
        , keep_fail "Merge objects command"
        , keep_fail "Merge objects flags"
        , keep_def "Merge objects supports response files" "NO"
        , keep_fail "ar command"
        , keep_fail "ar flags"
        , keep_fail "ar supports at file"
        , keep_fail "ar supports -L"
        , keep_fail "ranlib command"
        , keep_fail "otool command"
        , keep_fail "install_name_tool command"
        , keep_fail "windres command"
        , keep_fail "unlit command"
        , keep_fail "cross compiling"
        , keep_fail "target platform string"
        , keep_fail "target os"
        , keep_fail "target arch"
        , keep_fail "target word size"
        , keep_fail "target word big endian"
        , keep_fail "target has GNU nonexec stack"
        , keep_fail "target has .ident directive"
        , keep_fail "target has subsections via symbols"
        , keep_fail "target has libm"
        , keep_fail "Unregisterised"
        , keep_fail "LLVM target"
        , keep_fail "LLVM llc command"
        , keep_fail "LLVM opt command"
        , keep_def "LLVM llvm-as command" "llvm-as"
        , keep_fail "Use inplace MinGW toolchain"

        , keep_def "target RTS linker only supports shared libraries" "NO"
        , ("Use interpreter", "NO")
        , ("base unit-id", "base") -- there is no base yet... Anyway this isn't really useful to set
        , keep_fail "Support SMP"
        , keep_fail "RTS ways"
        , keep_fail "Tables next to code"
        , keep_fail "Leading underscore"
        , keep_fail "Use LibFFI"
        , keep_fail "RTS expects libdw"
        , ("Relative Global Package DB", "../pkgs")
            -- relative to $topdir (i.e. /lib)
        ]

buildBootLibraries :: Cabal -> Ghc -> GhcPkg -> DeriveConstants -> GenApply -> GenPrimop -> GhcBuildOptions -> FilePath -> IO ()
buildBootLibraries cabal ghc ghcpkg derive_constants genapply genprimop opts dst = do
  src <- makeAbsolute (dst </> "src")
  prepareGhcSources opts src

  -- Build the RTS
  src_rts <- makeAbsolute (src </> "libraries/rts")
  build_dir <- makeAbsolute (dst </> "cabal")
  ghcversionh <- makeAbsolute (src_rts </> "include/ghcversion.h")

  let cabal_project_rts_path = dst </> "cabal.project-rts"
      -- cabal's code handling escaping is bonkers. We need to wrap the whole
      -- option into \" otherwise it does weird things (like keeping only the
      -- last double-quote).
  let def_string k v = "  ghc-options: \"-optc-D" ++ k ++ "=\\\"" ++ v ++ "\\\"\""
  let def        k v = "  ghc-options: \"-optc-D" ++ k ++ "=" ++ v ++ "\""
  let rts_options =
        [ "package rts"
        , def_string "ProjectVersion" (Text.unpack (gboVersionInt opts))
        , def_string "RtsWay"            "FIXME"
        , def_string "HostPlatform"      "FIXME"
        , def_string "HostArch"          "FIXME"
        , def_string "HostOS"            "FIXME"
        , def_string "HostVendor"        "FIXME"
        , def_string "BuildPlatform"     "FIXME"
        , def_string "BuildArch"         "FIXME"
        , def_string "BuildOS"           "FIXME"
        , def_string "BuildVendor"       "FIXME"
        , def_string "TargetPlatform"    "FIXME"
        , def_string "TargetArch"        "FIXME"
        , def_string "TargetOS"          "FIXME"
        , def_string "TargetVendor"      "FIXME"
        , def_string "GhcUnregisterised" "FIXME"
        , def_string "TablesNextToCode"  "FIXME"
          -- Set the namespace for the rts fs functions
        , def "FS_NAMESPACE" "rts"
        , "  flags: +use-system-libffi +tables-next-to-code"
          -- FIXME: deal with libffi (add package?)
          --
          -- FIXME: we should make tables-next-to-code  optional here and in the
          -- compiler settings. Ideally, GHC should even look into the rts's
          -- ghcautoconf.h to check whether TABLES_NEXT_TO_CODE is defined or
          -- not. It would be cleaner than duplicating this information into the
          -- settings (similar to what we do with platform constants).

          -- FIXME: Cabal doesn't like when flags are on separate lines like
          -- this:
          --  flags: +use-system-libffi
          --  flags: +tables-next-to-code
          -- Apparently it makes it ignore the first set of flags...
        ]

  makeCabalProject cabal_project_rts_path $
        [ "package-dbs: clear, global"
        , ""
        , "packages:"
        , "  " ++ src </> "libraries/rts"
        , ""
        , "benchmarks: False"
        , "tests: False"
        , "allow-boot-library-installs: True"
        , "active-repositories: :none"
        , ""
        , "package *"
        , "  library-vanilla: True"
        , "  shared: False"
        , "  executable-profiling: False"
        , "  executable-dynamic: False"
        , "  executable-static: False"
        , ""
        ] ++ rts_options

  let build_rts_cmd = runCabal cabal
        [ "build"
        , "--project-file=" ++ cabal_project_rts_path
        , "rts"
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
  void $ readCreateProcessWithExitCode build_rts_cmd ""
  ghcplatform_dir <- takeDirectory <$> readCreateProcess (shell ("find " ++ build_dir ++ " -name ghcplatform.h")) ""

  -- deriving constants
  let derived_constants = src_rts </> "include/DerivedConstants.h"
  withSystemTempDirectory "derive-constants" $ \tmp_dir -> do
    void $ readCreateProcess (runDeriveConstants derive_constants
      [ "--gen-header"
      , "-o",  derived_constants
      , "--target-os", "linux" -- FIXME
      , "--tmpdir", tmp_dir
      , "--gcc-program", "gcc" -- FIXME
      , "--nm-program", "nm"   -- FIXME
      , "--objdump-program", "objdump" -- FIXME
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
  let primops_txt    = src </> "libraries/ghc/GHC/Builtin/primops.txt"
  let primops_txt_pp = primops_txt <.> ".pp"
  primops <- readCreateProcess (shell $ "gcc -E -undef -traditional -P -x c " ++ primops_txt_pp) ""
  writeFile primops_txt primops
  writeFile (src </> "libraries/ghc-internal/src/GHC/Internal/Prim.hs") =<< readCreateProcess (runGenPrimop genprimop ["--make-haskell-source"]) primops
  writeFile (src </> "libraries/ghc-internal/src/GHC/Internal/PrimopWrappers.hs") =<< readCreateProcess (runGenPrimop genprimop ["--make-haskell-wrappers"]) primops

  -- build libffi
  msg "  - Building libffi..."
  src_libffi <- makeAbsolute (src </> "libffi")
  dst_libffi <- makeAbsolute (dst </> "libffi")
  let libffi_version = "3.4.6"
  createDirectoryIfMissing True src_libffi
  createDirectoryIfMissing True dst_libffi
  void $ readCreateProcess (shell ("tar -xvf libffi-tarballs/libffi-" ++ libffi_version ++ ".tar.gz -C " ++ src_libffi)) ""
  let build_libffi = mconcat
        [ "cd " ++ src_libffi </> "libffi-" ++ libffi_version ++ "; "
        -- FIXME: pass the appropriate toolchain (CC, LD...)
        , "./configure --disable-docs --with-pic=yes --disable-multi-os-directory --prefix=" ++ dst_libffi
        , " && make install -j"
        ]
  (libffi_exit_code, libffi_stdout, libffi_stderr) <- readCreateProcessWithExitCode (shell build_libffi) ""
  case libffi_exit_code of
    ExitSuccess -> pure ()
    ExitFailure r -> do
      putStrLn $ "Failed to build libffi with error code " ++ show r
      putStrLn libffi_stdout
      putStrLn libffi_stderr
      exitFailure
  cp (dst_libffi </> "include" </> "*") (src_rts </> "include")
  cp (dst_libffi </> "lib" </> "libffi.a") (takeDirectory ghcplatform_dir </> "libCffi.a")

  -- build boot libraries: ghc-internal, base... but not GHC itself
  let cabal_project_bootlibs_path = dst </> "cabal-project-boot-libs"
  makeCabalProject cabal_project_bootlibs_path $
        [ "package-dbs: clear, global"
        , ""
        , "packages:"
        , "  " ++ src </> "libraries/rts"
        , "  " ++ src </> "libraries/ghc-prim"
        , "  " ++ src </> "libraries/ghc-internal"
        , "  " ++ src </> "libraries/base"
        , "  " ++ src </> "libraries/ghc"
        , "  " ++ src </> "libraries/ghc-platform/"
        , "  " ++ src </> "libraries/ghc-boot/"
        , "  " ++ src </> "libraries/ghc-boot-th/"
        , "  " ++ src </> "libraries/ghc-heap"
        , "  " ++ src </> "libraries/ghci"
        , "  " ++ src </> "libraries/stm"
        , "  " ++ src </> "libraries/template-haskell"
        , "  " ++ src </> "libraries/hpc"
        , "  " ++ src </> "ghc-bin/"
        , "  " ++ src </> "utils/ghc-pkg"
        , "  " ++ src </> "utils/hsc2hs"
        , "  " ++ src </> "utils/unlit"
        , "  " ++ src </> "libraries/array"
        , "  " ++ src </> "libraries/binary"
        , "  " ++ src </> "libraries/bytestring"
        , "  " ++ src </> "libraries/containers/containers"
        , "  " ++ src </> "libraries/deepseq"
        , "  " ++ src </> "libraries/directory/"
        , "  " ++ src </> "libraries/exceptions"
        , "  " ++ src </> "libraries/file-io/"
        , "  " ++ src </> "libraries/filepath/"
        , "  " ++ src </> "libraries/mtl"
        , "  " ++ src </> "libraries/os-string/"
        , "  " ++ src </> "libraries/parsec"
        , "  " ++ src </> "libraries/pretty/"
        , "  " ++ src </> "libraries/process/"
        , "  " ++ src </> "libraries/semaphore-compat"
        , "  " ++ src </> "libraries/text"
        , "  " ++ src </> "libraries/time"
        , "  " ++ src </> "libraries/transformers"
        , "  " ++ src </> "libraries/unix/"
        , "  " ++ src </> "libraries/Win32/"
        , "  " ++ src </> "libraries/Cabal/Cabal-syntax"
        , "  " ++ src </> "libraries/Cabal/Cabal"
        , "  https://github.com/haskell/alex/archive/refs/tags/v3.5.2.0.tar.gz"
        , ""
        , "benchmarks: False"
        , "tests: False"
        , "allow-boot-library-installs: True"
        , "active-repositories: :none"
        , ""
        , "package *"
        , "  library-vanilla: True"
        , "  shared: False"
        , "  executable-profiling: False"
        , "  executable-dynamic: False"
        , "  executable-static: False"
        , ""
        , "package ghc-internal"
             -- FIXME: make our life easier for now by using the native bignum backend
        , "  flags: +bignum-native"
        , ""
        , "package text"
             -- FIXME: avoid having to deal with system-cxx-std-lib fake package for now
        , "  flags: -simdutf"
        , ""
        ] ++ rts_options


  let boot_libs_env = dst </> "boot-libs.env"
  let build_boot_cmd = runCabal cabal
        [ "install"
        , "--lib"
        , "--package-env=" ++ boot_libs_env
        , "--force-reinstalls"
        , "-v3"
        , "--project-file=" ++ cabal_project_bootlibs_path
        , "--with-compiler=" ++ ghcPath ghc
        , "--with-hc-pkg=" ++ ghcPkgPath ghcpkg
        , "--ghc-options=\"-ghcversion-file=" ++ ghcversionh ++ "\""
        , "--builddir=" ++ build_dir
        , "-j"

          -- targets
        , "rts"
        , "ghc-prim"
        , "ghc-internal"
        , "base"
        ]

  msg "  - Building boot libraries..."
  (boot_exit_code, boot_stdout, boot_stderr) <- readCreateProcessWithExitCode build_boot_cmd ""
  writeFile (dst </> "boot-libs.stdout") boot_stdout
  writeFile (dst </> "boot-libs.stderr") boot_stderr
  case boot_exit_code of
    ExitSuccess -> pure ()
    ExitFailure r -> do
      putStrLn $ "Failed to build boot libraries with error code " ++ show r
      putStrLn boot_stdout
      putStrLn boot_stderr
      putStrLn $ "Logs can be found in " ++ dst ++ "boot-libs.{stdout,stderr}"
      exitFailure

  -- The libraries have been installed globally.
  boot_libs_env_lines <- lines <$> readFile boot_libs_env
  -- FIXME: Sometimes the package environment contains the path to the global db,
  -- sometimes not... I don't know why yet.
  (global_db,pkg_ids) <- case drop 2 boot_libs_env_lines of
    [] -> error "Unexpected empty package environment"
    (x:xs)
      | not ("package-db" `List.isPrefixOf` x)
      -> do
        putStrLn "For some reason cabal-install didn't generate a valid package environment (package-db is missing)."
        putStrLn "It happens sometimes for unknown reasons... Rerun 'make' to workaround this..."
        exitFailure
      | otherwise -> pure (drop 11 x, map (drop 11) xs)
  putStrLn $ "We've built boot libraries in " ++ global_db ++ ":"
  mapM_ (putStrLn . ("  - " ++)) pkg_ids

  -- copy the libs in another db
  createDirectoryIfMissing True (dst </> "pkgs")
  initEmptyDB ghcpkg (dst </> "pkgs")
  let pkg_root = takeDirectory global_db
  forM_ pkg_ids $ \pid -> do
    conf <- Text.readFile (global_db </> pid <.> "conf")
    -- replace full path with ${pkgroot}
    -- NOTE: GHC assumes that pkgroot is just one directory above the directory
    -- containing the package db. In our case where everything is at the same
    -- level in "pkgs" we need to re-add "/pkgs"
    Text.writeFile (dst </> "pkgs" </> pid <.> "conf")
                   (Text.replace (Text.pack pkg_root) "${pkgroot}/pkgs" conf)
    cp (pkg_root </> pid) (dst </> "pkgs")

  void $ readCreateProcess (runGhcPkg ghcpkg ["recache", "--package-db=" ++ (dst </> "pkgs")]) ""



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

cp :: String -> String -> IO ()
cp src dst = void (readCreateProcess (shell $ "cp -rf " ++ src ++ " " ++ dst) "")

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
