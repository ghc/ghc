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
import Data.Text (Text)
import Data.Text qualified as Text
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

  ghc0_version <- readCreateProcess' (runGhc ghc0 ["--version"]) ""
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
  cp "_build/stage0/bin/*" "_build/stage1/bin/"
  cp "_build/stage0/lib/template-hsc.h" "_build/stage1/lib/template-hsc.h"

  ghc1    <- Ghc    <$> makeAbsolute "_build/stage1/bin/ghc"
  genprimop <- GenPrimop <$> makeAbsolute "_build/stage1/bin/genprimopcode"
  ghcToolchain <- GhcToolchain <$> makeAbsolute "_build/stage1/bin/ghc-toolchain"

  -- generate settings based on stage1 compiler settings: stage1 should never be
  -- a cross-compiler! Hence we reuse the same target platform as the bootstrap
  -- compiler.
  stage0_target_triple <- ghcTargetTriple ghc0
  let stage1_settings = emptySettings
                          { settingsTriple = Just stage0_target_triple
                          }

  void $ readCreateProcess' (shell $ "rm -fR _build/stage1/lib/package.conf.d; ln -s $(pwd)/_build/stage0/cabal/packagedb/ghc-* _build/stage1/lib/package.conf.d") ""
  -- ensure we re-cache... with the newly built ghc-pkg.
  void $ readCreateProcess' (shell $ "_build/stage1/bin/ghc-pkg recache") ""
  generateSettings ghcToolchain stage1_settings "_build/stage1/"

  msg "Building boot libraries with stage1 compiler..."
  buildBootLibraries genprimop

  msg "Building stage2 GHC program"
  createDirectoryIfMissing True "_build/stage2"
  buildGhcStage2 defaultGhcBuildOptions cabal ghc1 "_build/stage2/"

  -- Reuse stage1 settings for stage2 and copy stage1's built boot package for
  -- stage2 to use.
  createDirectoryIfMissing True "_build/stage2/lib/"
  void $ readCreateProcess' (shell $ "rm -fR _build/stage2/lib/package.conf.d; ln -s $(pwd)/_build/stage2/cabal/packagedb/ghc-* _build/stage2/lib/package.conf.d") ""
  cp "_build/stage1/lib/settings" "_build/stage2/lib/settings"

  -- TODO: in the future we want to generate different settings for cross
  -- targets and build boot libraries with stage2 using these settings. In any
  -- case, we need non-cross boot packages to build plugins for use with
  -- -fplugin-library.


  -- Finally create bindist directory
  msg "Creating bindist"
  createDirectoryIfMissing True "_build/bindist/lib/"
  createDirectoryIfMissing True "_build/bindist/bin/"
  cp "_build/stage2/bin/*" "_build/bindist/bin/"
  cp "_build/stage2/lib/*" "_build/bindist/lib/"
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
  msg "  - Building GHC and utility programs..."

  let builddir = dst </> "cabal"
  createDirectoryIfMissing True builddir

  -- we need to augment the current environment to pass HADRIAN_SETTINGS
  -- environment variable to ghc-boot's Setup.hs script.
  (arch,os) <- ghcTargetArchOS ghc0
  stage1_ghc_boot_settings <- do
    commit_id <- readCreateProcess' (proc "git" ["rev-parse", "HEAD"]) ""
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
  genapply_path <- makeAbsolute "_build/stage1/bin/genapply"
  deriveConstants_path <- makeAbsolute "_build/stage1/bin/deriveConstants"
  ghcPkg <- makeAbsolute "_build/stage1/bin/ghc-pkg"
  let stage1_env = ("HADRIAN_SETTINGS", stage1_ghc_boot_settings)
                 -- This is a very stupid hack :-/
                 -- We can't really have `rts` depend on genapply and deriveConstants
                 -- although it technically does. The reason is that build-tool-depends
                 -- doesn't run prior to build-type Configure.
                 : ("GENAPPLY", genapply_path)
                 : ("DERIVE_CONSTANTS", deriveConstants_path)
                 : current_env

  let cabal_project_path = "cabal.project.stage" ++ (if booting then "1" else "2")

  -- the targets
  let targets
        | booting =
           [ "rts-headers:rts-headers"
           , "ghc-bin:ghc"
           , "ghc-pkg:ghc-pkg"
           , "genprimopcode:genprimopcode"
           , "deriveConstants:deriveConstants"
           , "genapply:genapply"
           , "ghc-toolchain-bin:ghc-toolchain-bin"
           , "unlit:unlit"
           , "hsc2hs:hsc2hs"
           ]
        | otherwise =
           [ "--with-ghc-pkg=" ++ ghcPkg
           , "rts-headers:rts-headers"
           , "ghc-bin:ghc"
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
           -- All these libraries are somehow needed by some tests :rolleyes:
           -- this seems to be needed occationally.
           , "ghc-bignum:ghc-bignum"
           -- this package is utterly retarded.
           , "system-cxx-std-lib:system-cxx-std-lib"
           , "ghc-compact:ghc-compact"
           , "ghc-experimental:ghc-experimental"
           , "ghc-toolchain:ghc-toolchain"
           , "integer-gmp:integer-gmp"
           , "xhtml:xhtml"
           , "terminfo:terminfo"
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

  (exit_code, cabal_stdout, cabal_stderr) <- readCreateProcessWithExitCode' build_cmd ""
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
        (list_bin_exit_code, list_bin_stdout, list_bin_stderr) <- readCreateProcessWithExitCode' (listbin_cmd target) ""
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

buildBootLibraries :: GenPrimop -> IO ()
buildBootLibraries genprimop = do

  msg "  - Generating headers and sources..."
  -- Generate primop code for ghc-internal
  --
  -- Note that this can't be done in a Setup.hs for ghc-internal because
  -- cabal-install can't build Setup.hs because it depends on base, Cabal, etc.
  -- libraries that aren't built yet.
  let primops_txt    = "compiler/GHC/Builtin/primops.txt"
  let primops_txt_pp = primops_txt <.> ".pp"
  void $ readCreateProcessInto' primops_txt (shell $ "cc -E -undef -traditional -P -x c " ++ primops_txt_pp) ""
  void $ readCreateProcessIntoWithFile' "libraries/ghc-internal/src/GHC/Internal/Prim.hs" (runGenPrimop genprimop ["--make-haskell-source"]) primops_txt
  void $ readCreateProcessIntoWithFile' "libraries/ghc-internal/src/GHC/Internal/PrimopWrappers.hs" (runGenPrimop genprimop ["--make-haskell-wrappers"]) primops_txt


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
newtype GhcToolchain = GhcToolchain FilePath
newtype Cabal = Cabal FilePath
newtype GenPrimop = GenPrimop FilePath

runGhc :: Ghc -> [String] -> CreateProcess
runGhc (Ghc f) = proc f

ghcPath :: Ghc -> FilePath
ghcPath (Ghc x) = x

runGhcToolchain :: GhcToolchain -> [String] -> CreateProcess
runGhcToolchain (GhcToolchain f) = proc f

runCabal :: Cabal -> [String] -> CreateProcess
runCabal (Cabal f) = proc f

runGenPrimop :: GenPrimop -> [String] -> CreateProcess
runGenPrimop (GenPrimop f) = proc f

-- | Copy
--
-- Recursively, force overwrite, and preserve timestamps (important for package
-- dbs)
cp :: String -> String -> IO ()
cp src dst = void (readCreateProcess' (shell $ "cp -rfp " ++ src ++ " " ++ dst) "")

readCreateProcessWithExitCode' :: CreateProcess -> String -> IO (ExitCode, String, String)
readCreateProcessWithExitCode' p input = do
  case p of
    (CreateProcess { cmdspec = RawCommand path args }) -> do
      msg $ " $ " ++ path ++ " " ++ unwords args
    (CreateProcess { cmdspec = ShellCommand cmd }) -> do
      msg $ " $ " ++ cmd
  unless (null input) $ do
    msg $ "Input: " ++ input
  readCreateProcessWithExitCode p input

readCreateProcess' :: CreateProcess -> String -> IO String
readCreateProcess' p input = do
  case p of
    (CreateProcess { cmdspec = RawCommand path args }) -> do
      msg $ " $ " ++ path ++ " " ++ unwords args
    (CreateProcess { cmdspec = ShellCommand cmd }) -> do
      msg $ " $ " ++ cmd
  unless (null input) $ do
    msg $ "Input: " ++ input
  readCreateProcess p input

readCreateProcessInto' :: FilePath -> CreateProcess -> String -> IO String
readCreateProcessInto' dst p input = do
  case p of
    (CreateProcess { cmdspec = RawCommand path args }) -> do
      msg $ " $ " ++ path ++ " " ++ unwords args ++ " > " ++ dst
    (CreateProcess { cmdspec = ShellCommand cmd }) -> do
      msg $ " $ " ++ cmd ++ " > " ++ dst
  unless (null input) $ do
    msg $ "Input: " ++ input
  out <- readCreateProcess p input
  writeFile dst out
  pure out

readCreateProcessIntoWithFile' :: FilePath -> CreateProcess -> FilePath -> IO String
readCreateProcessIntoWithFile' dst p input = do
  case p of
    (CreateProcess { cmdspec = RawCommand path args }) -> do
      msg $ " $ " ++ path ++ " " ++ unwords args ++ " < " ++ input ++ " > " ++ dst
    (CreateProcess { cmdspec = ShellCommand cmd }) -> do
      msg $ " $ " ++ cmd ++ " < " ++ input ++ " > " ++ dst
  out <- readCreateProcess p =<< readFile input
  writeFile dst out
  pure out

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

-- | Retrieve GHC's target arch/os from ghc --info
ghcTargetArchOS :: Ghc -> IO (String,String)
ghcTargetArchOS ghc = do
  is <- read <$> readCreateProcess' (runGhc ghc ["--info"]) "" :: IO [(String,String)]
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
  is <- read <$> readCreateProcess' (runGhc ghc ["--info"]) "" :: IO [(String,String)]
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

  let gen_settings_path = dst </> "lib/settings"

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

  (exit_code, toolchain_stdout, toolchain_stderr) <- readCreateProcessWithExitCode' (runGhcToolchain ghc_toolchain args) ""
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

