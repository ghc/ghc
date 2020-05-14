{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Settings.IO
 ( SettingsError (..)
 , initSettings
 ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Settings.Platform
import GHC.Settings.Utils

import GHC.Settings.Config
import GHC.Utils.CliOption
import GHC.Utils.Fingerprint
import GHC.Platform
import GHC.Utils.Outputable
import GHC.Settings
import GHC.SysTools.BaseDir

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified Data.Map as Map
import System.FilePath
import System.Directory

data SettingsError
  = SettingsError_MissingData String
  | SettingsError_BadData String

initSettings
  :: forall m
  .  MonadIO m
  => String -- ^ TopDir path
  -> ExceptT SettingsError m Settings
initSettings top_dir = do
  -- see Note [topdir: How GHC finds its files]
  -- NB: top_dir is assumed to be in standard Unix
  -- format, '/' separated
  mtool_dir <- liftIO $ findToolDir top_dir
        -- see Note [tooldir: How GHC finds mingw on Windows]

  let installed :: FilePath -> FilePath
      installed file = top_dir </> file
      libexec :: FilePath -> FilePath
      libexec file = top_dir </> "bin" </> file
      settingsFile = installed "settings"
      platformConstantsFile = installed "platformConstants"

      readFileSafe :: FilePath -> ExceptT SettingsError m String
      readFileSafe path = liftIO (doesFileExist path) >>= \case
        True -> liftIO $ readFile path
        False -> throwE $ SettingsError_MissingData $ "Missing file: " ++ path

  settingsStr <- readFileSafe settingsFile
  platformConstantsStr <- readFileSafe platformConstantsFile
  settingsList <- case maybeReadFuzzy settingsStr of
    Just s -> pure s
    Nothing -> throwE $ SettingsError_BadData $
      "Can't parse " ++ show settingsFile
  let mySettings = Map.fromList settingsList
  platformConstants <- case maybeReadFuzzy platformConstantsStr of
    Just s -> pure s
    Nothing -> throwE $ SettingsError_BadData $
      "Can't parse " ++ show platformConstantsFile
  -- See Note [Settings file] for a little more about this file. We're
  -- just partially applying those functions and throwing 'Left's; they're
  -- written in a very portable style to keep ghc-boot light.
  let getSetting key = either pgmError pure $
        getFilePathSetting0 top_dir settingsFile mySettings key
      getToolSetting :: String -> ExceptT SettingsError m String
      getToolSetting key = expandToolDir mtool_dir <$> getSetting key
      getBooleanSetting :: String -> ExceptT SettingsError m Bool
      getBooleanSetting key = either pgmError pure $
        getBooleanSetting0 settingsFile mySettings key
  targetPlatformString <- getSetting "target platform string"
  tablesNextToCode <- getBooleanSetting "Tables next to code"
  myExtraGccViaCFlags <- getSetting "GCC extra via C opts"
  -- On Windows, mingw is distributed with GHC,
  -- so we look in TopDir/../mingw/bin,
  -- as well as TopDir/../../mingw/bin for hadrian.
  -- It would perhaps be nice to be able to override this
  -- with the settings file, but it would be a little fiddly
  -- to make that possible, so for now you can't.
  cc_prog <- getToolSetting "C compiler command"
  cc_args_str <- getSetting "C compiler flags"
  cxx_args_str <- getSetting "C++ compiler flags"
  gccSupportsNoPie <- getBooleanSetting "C compiler supports -no-pie"
  cpp_prog <- getToolSetting "Haskell CPP command"
  cpp_args_str <- getSetting "Haskell CPP flags"

  platform <- either pgmError pure $ getTargetPlatform settingsFile mySettings

  let unreg_cc_args = if platformUnregisterised platform
                      then ["-DNO_REGS", "-DUSE_MINIINTERPRETER"]
                      else []
      cpp_args = map Option (words cpp_args_str)
      cc_args  = words cc_args_str ++ unreg_cc_args
      cxx_args = words cxx_args_str
  ldSupportsCompactUnwind <- getBooleanSetting "ld supports compact unwind"
  ldSupportsBuildId       <- getBooleanSetting "ld supports build-id"
  ldSupportsFilelist      <- getBooleanSetting "ld supports filelist"
  ldIsGnuLd               <- getBooleanSetting "ld is GNU ld"

  let globalpkgdb_path = installed "package.conf.d"
      ghc_usage_msg_path  = installed "ghc-usage.txt"
      ghci_usage_msg_path = installed "ghci-usage.txt"

  -- For all systems, unlit, split, mangle are GHC utilities
  -- architecture-specific stuff is done when building Config.hs
  unlit_path <- getToolSetting "unlit command"

  windres_path <- getToolSetting "windres command"
  libtool_path <- getToolSetting "libtool command"
  ar_path <- getToolSetting "ar command"
  ranlib_path <- getToolSetting "ranlib command"

  -- TODO this side-effect doesn't belong here. Reading and parsing the settings
  -- should be idempotent and accumulate no resources.
  tmpdir <- liftIO $ getTemporaryDirectory

  touch_path <- getToolSetting "touch command"

  mkdll_prog <- getToolSetting "dllwrap command"
  let mkdll_args = []

  -- cpp is derived from gcc on all platforms
  -- HACK, see setPgmP below. We keep 'words' here to remember to fix
  -- Config.hs one day.


  -- Other things being equal, as and ld are simply gcc
  cc_link_args_str <- getSetting "C compiler link flags"
  let   as_prog  = cc_prog
        as_args  = map Option cc_args
        ld_prog  = cc_prog
        ld_args  = map Option (cc_args ++ words cc_link_args_str)

  llvmTarget <- getSetting "LLVM target"

  -- We just assume on command line
  lc_prog <- getSetting "LLVM llc command"
  lo_prog <- getSetting "LLVM opt command"
  lcc_prog <- getSetting "LLVM clang command"

  let iserv_prog = libexec "ghc-iserv"

  integerLibrary <- getSetting "integer library"
  integerLibraryType <- case integerLibrary of
    "integer-gmp" -> pure IntegerGMP
    "integer-simple" -> pure IntegerSimple
    _ -> pgmError $ unwords
      [ "Entry for"
      , show "integer library"
      , "must be one of"
      , show "integer-gmp"
      , "or"
      , show "integer-simple"
      ]

  ghcWithInterpreter <- getBooleanSetting "Use interpreter"
  ghcWithNativeCodeGen <- getBooleanSetting "Use native code generator"
  ghcWithSMP <- getBooleanSetting "Support SMP"
  ghcRTSWays <- getSetting "RTS ways"
  useLibFFI <- getBooleanSetting "Use LibFFI"
  ghcThreaded <- getBooleanSetting "Use Threads"
  ghcDebugged <- getBooleanSetting "Use Debugging"
  ghcRtsWithLibdw <- getBooleanSetting "RTS expects libdw"

  return $ Settings
    { sGhcNameVersion = GhcNameVersion
      { ghcNameVersion_programName = "ghc"
      , ghcNameVersion_projectVersion = cProjectVersion
      }

    , sFileSettings = FileSettings
      { fileSettings_tmpDir         = normalise tmpdir
      , fileSettings_ghcUsagePath   = ghc_usage_msg_path
      , fileSettings_ghciUsagePath  = ghci_usage_msg_path
      , fileSettings_toolDir        = mtool_dir
      , fileSettings_topDir         = top_dir
      , fileSettings_globalPackageDatabase = globalpkgdb_path
      }

    , sToolSettings = ToolSettings
      { toolSettings_ldSupportsCompactUnwind = ldSupportsCompactUnwind
      , toolSettings_ldSupportsBuildId       = ldSupportsBuildId
      , toolSettings_ldSupportsFilelist      = ldSupportsFilelist
      , toolSettings_ldIsGnuLd               = ldIsGnuLd
      , toolSettings_ccSupportsNoPie         = gccSupportsNoPie

      , toolSettings_pgm_L   = unlit_path
      , toolSettings_pgm_P   = (cpp_prog, cpp_args)
      , toolSettings_pgm_F   = ""
      , toolSettings_pgm_c   = cc_prog
      , toolSettings_pgm_a   = (as_prog, as_args)
      , toolSettings_pgm_l   = (ld_prog, ld_args)
      , toolSettings_pgm_dll = (mkdll_prog,mkdll_args)
      , toolSettings_pgm_T   = touch_path
      , toolSettings_pgm_windres = windres_path
      , toolSettings_pgm_libtool = libtool_path
      , toolSettings_pgm_ar = ar_path
      , toolSettings_pgm_ranlib = ranlib_path
      , toolSettings_pgm_lo  = (lo_prog,[])
      , toolSettings_pgm_lc  = (lc_prog,[])
      , toolSettings_pgm_lcc = (lcc_prog,[])
      , toolSettings_pgm_i   = iserv_prog
      , toolSettings_opt_L       = []
      , toolSettings_opt_P       = []
      , toolSettings_opt_P_fingerprint = fingerprint0
      , toolSettings_opt_F       = []
      , toolSettings_opt_c       = cc_args
      , toolSettings_opt_cxx     = cxx_args
      , toolSettings_opt_a       = []
      , toolSettings_opt_l       = []
      , toolSettings_opt_windres = []
      , toolSettings_opt_lcc     = []
      , toolSettings_opt_lo      = []
      , toolSettings_opt_lc      = []
      , toolSettings_opt_i       = []

      , toolSettings_extraGccViaCFlags = words myExtraGccViaCFlags
      }

    , sTargetPlatform = platform
    , sPlatformMisc = PlatformMisc
      { platformMisc_targetPlatformString = targetPlatformString
      , platformMisc_integerLibrary = integerLibrary
      , platformMisc_integerLibraryType = integerLibraryType
      , platformMisc_ghcWithInterpreter = ghcWithInterpreter
      , platformMisc_ghcWithNativeCodeGen = ghcWithNativeCodeGen
      , platformMisc_ghcWithSMP = ghcWithSMP
      , platformMisc_ghcRTSWays = ghcRTSWays
      , platformMisc_tablesNextToCode = tablesNextToCode
      , platformMisc_libFFI = useLibFFI
      , platformMisc_ghcThreaded = ghcThreaded
      , platformMisc_ghcDebugged = ghcDebugged
      , platformMisc_ghcRtsWithLibdw = ghcRtsWithLibdw
      , platformMisc_llvmTarget = llvmTarget
      }

    , sPlatformConstants = platformConstants

    , sRawSettings    = settingsList
    }
