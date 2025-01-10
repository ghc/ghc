#!/usr/bin/env cabal
{- cabal:
build-depends:
  base,
  directory,
  filepath,
  process,
  text
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

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
import System.Environment
import System.Directory
import System.Process
import System.FilePath
import System.Exit

main :: IO ()
main = do
  -- FIXME: specific patched cabal-install for now and GHC that is known to
  -- work...
  setEnv "CABAL" "/home/hsyl20/projects/cabal/dist-newstyle/build/x86_64-linux/ghc-9.10.1/cabal-install-3.15.0.0/x/cabal/build/cabal/cabal"
  setEnv "GHC" "ghc-9.8.4"

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

  void $ readCreateProcessWithExitCode (runGhc ghc0 ["--version"]) ""

  -- build GHC stage1
  buildGhcStage1 defaultGhcBuildOptions cabal ghc0


-- | Build stage1 GHC program
buildGhcStage1 :: GhcBuildOptions -> Cabal -> Ghc -> IO ()
buildGhcStage1 opts cabal ghc0 = do
  putStrLn "Preparing GHC sources to build GHC stage1..."
  prepareGhcSources opts "_build/stage0/src/"

  let builddir = "_build/stage0/cabal/"
  createDirectoryIfMissing True builddir

  putStrLn "Prepare GHC stage1 configuration..."
  -- we need to augment the current environment to pass HADRIAN_SETTINGS
  -- environment variable to ghc-boot's Setup.hs script.
  stage0_settings <- read <$> readCreateProcess (runGhc ghc0 ["--info"]) ""
  stage1_ghc_boot_settings <- do
    commit_id <- readCreateProcess (proc "git" ["rev-parse", "HEAD"]) ""
    -- we infer stage1's host platform from stage0's settings
    let settings =
          [ ("hostPlatformArch",    fromMaybe (error "Couldn't read 'target arch' setting") (lookup "target arch" stage0_settings))
          , ("hostPlatformOS",	    fromMaybe (error "Couldn't read 'target os' setting") (lookup "target os" stage0_settings))
          , ("cProjectGitCommitId", commit_id)
          , ("cProjectVersion",	    Text.unpack $ gboVersion opts)
          , ("cProjectVersionInt",  Text.unpack $ gboVersionInt opts)
          , ("cProjectPatchLevel",  Text.unpack $ gboVersionPatchLevel opts)
          , ("cProjectPatchLevel1", Text.unpack $ gboVersionPatchLevel1 opts)
          , ("cProjectPatchLevel2", Text.unpack $ gboVersionPatchLevel2 opts)
          ]
    pure (show settings)

  current_env <- getEnvironment
  let stage1_env = ("HADRIAN_SETTINGS", stage1_ghc_boot_settings) : current_env

  putStrLn "Building GHC stage1 and bootstrapping utility programs..."
  let build_cmd = (runCabal cabal
              [ "build"
              , "--project-file=cabal.project-stage0"
              , "--builddir=" ++ builddir
              , "-j"
              -- the targets
              , "ghc-bin:ghc"
              , "ghc-pkg:ghc-pkg"
              , "genprimopcode:genprimopcode"
              , "deriveConstants:deriveConstants"
              , "genapply:genapply"
              ])
              { env = Just stage1_env
              }
  (exit_code, cabal_stdout, cabal_stderr) <- readCreateProcessWithExitCode build_cmd ""
  writeFile "_build/stage0/cabal.stdout" cabal_stdout
  writeFile "_build/stage0/cabal.stderr" cabal_stderr
  case exit_code of
    ExitSuccess -> pure ()
    ExitFailure n -> do
      putStrLn $ "cabal-install failed with error code: " ++ show n
      putStrLn "Logs can be found in \"_build/stage0/cabal.{stdout,stderr}\""
      exitFailure

  putStrLn "Copying stage0 programs and generating settings to use them..."
  let listbin_cmd p = runCabal cabal
	[ "list-bin"
	, "--project-file=cabal.project-stage0"
	, "--builddir=" ++ builddir
	, p
	]
  let copy_bin target bin = do
	(list_bin_exit_code, list_bin_stdout, list_bin_stderr) <- readCreateProcessWithExitCode (listbin_cmd target) ""
	case list_bin_exit_code of
	  ExitSuccess
	    | (bin_src:_) <- lines list_bin_stdout
	    -> cp bin_src ("_build/stage0/bin" </> bin)
	  _ -> do
	    putStrLn $ "Failed to run cabal list-bin for the target: " ++ show target
	    putStrLn list_bin_stderr
	    exitFailure
  createDirectoryIfMissing True "_build/stage0/bin"
  copy_bin "ghc-bin:ghc"		     "ghc"
  copy_bin "ghc-pkg:ghc-pkg"  		     "ghc-pkg"
  copy_bin "deriveConstants:deriveConstants" "deriveConstants"
  copy_bin "genprimopcode:genprimopcode"     "genprimopcode"
  copy_bin "genapply:genapply"		     "genapply"

  -- generate settings based on stage1 compiler settings
  createDirectoryIfMissing True "_build/stage0/lib"
  let stage1_settings = makeStage1Settings stage0_settings
  writeFile "_build/stage0/lib/settings" (show stage1_settings)

  -- try to run the stage1 compiler (no package db yet, so just display the
  -- version)
  (test_exit_code, test_stdout, _test_stderr) <- readCreateProcessWithExitCode (proc "_build/stage0/bin/ghc" ["--version"]) ""
  case test_exit_code of
    ExitSuccess -> pure ()
    ExitFailure n -> do
      putStrLn $ "Failed to run stage1 compiler with error code " ++ show n
      exitFailure




data GhcBuildOptions = GhcBuildOptions
  { gboVersion	          :: !Text -- ^ GHC version
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
  , gboVersionForLib      = "9.13"
  , gboVersionPatchLevel  = "0"
  , gboVersionPatchLevel1 = "0"
  , gboVersionPatchLevel2 = "0"
  , gboLlvmMinVersion     = "13"
  , gboLlvmMaxVersion     = "20"
  }


-- | Prepare GHC sources in the given directory
prepareGhcSources :: GhcBuildOptions -> FilePath -> IO ()
prepareGhcSources opts dst = do
  createDirectoryIfMissing True dst 

  cp "./libraries" dst
  cp "./compiler"  (dst </> "libraries/ghc")
  cp "./ghc"	   (dst </> "ghc-bin")
  cp "./utils"     dst

  cp "rts/include/rts/Bytecodes.h"            (dst </> "libraries/ghc/")
  cp "rts/include/rts/storage/ClosureTypes.h" (dst </> "libraries/ghc/")
  cp "rts/include/rts/storage/FunTypes.h"     (dst </> "libraries/ghc/")
  cp "rts/include/stg/MachRegs.h"             (dst </> "libraries/ghc/")
  createDirectoryIfMissing True (dst </> "libraries/ghc/MachRegs")
  cp "rts/include/stg/MachRegs/*.h"           (dst </> "libraries/ghc/MachRegs/")

  -- substitute variables in files
  let subst fin fout rs = do
	t <- Text.readFile fin
	Text.writeFile fout (List.foldl' (\v (needle,rep) -> Text.replace needle rep v) t rs)
  let subst_in f = subst (f <.> "in") f
  let common_substs =
	[ (,) "@ProjectVersion@"       (gboVersion opts)
	, (,) "@ProjectVersionMunged@" (gboVersionMunged opts)
	, (,) "@ProjectVersionForLib@" (gboVersionForLib opts)
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
  subst_in (dst </> "libraries/ghci/ghci.cabal") common_substs
  subst_in (dst </> "libraries/ghc/GHC/CmmToLlvm/Version/Bounds.hs") llvm_substs
  subst_in (dst </> "utils/ghc-pkg/ghc-pkg.cabal") common_substs

-- Avoid FilePath blindness by using type aliases for programs.
newtype Ghc = Ghc FilePath
newtype Cabal = Cabal FilePath

runGhc :: Ghc -> [String] -> CreateProcess
runGhc (Ghc f) = proc f

runCabal :: Cabal -> [String] -> CreateProcess
runCabal (Cabal f) = proc f

cp :: String -> String -> IO ()
cp src dst = void (readCreateProcessWithExitCode (shell $ "cp -rf " ++ src ++ " " ++ dst) "")

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
        , ("base unit-id", "base")
        , keep_fail "Support SMP"
        , keep_fail "RTS ways"
        , keep_fail "Tables next to code"
        , keep_fail "Leading underscore"
        , keep_fail "Use LibFFI"
        , keep_fail "RTS expects libdw"
        , ("Relative Global Package DB", "../../stage1/pkgs")
        ]
