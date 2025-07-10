{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Settings.IO
 ( SettingsError (..)
 , initSettings
 ) where

import GHC.Prelude

import GHC.Settings.Utils

import GHC.Settings.Config
import GHC.Utils.CliOption
import GHC.Utils.Fingerprint
import GHC.Platform
import GHC.Utils.Panic
import GHC.Settings
import GHC.SysTools.BaseDir
import GHC.Unit.Types

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified Data.Map as Map
import System.FilePath
import System.Directory

import GHC.Toolchain.Program
import GHC.Toolchain
import GHC.Data.Maybe
import Data.Bifunctor (Bifunctor(second))

data SettingsError
  = SettingsError_MissingData String
  | SettingsError_BadData String

initSettings
  :: forall m
  .  MonadIO m
  => String -- ^ TopDir path
  -> ExceptT SettingsError m Settings
initSettings top_dir = do
  let installed :: FilePath -> FilePath
      installed file = top_dir </> file
      libexec :: FilePath -> FilePath
      libexec file = top_dir </> ".." </> "bin" </> file
      settingsFile = installed "settings"
      targetFile   = installed $ "targets" </> "default.target"

      readFileSafe :: FilePath -> ExceptT SettingsError m String
      readFileSafe path = liftIO (doesFileExist path) >>= \case
        True -> liftIO $ readFile path
        False -> throwE $ SettingsError_MissingData $ "Missing file: " ++ path

  settingsStr <- readFileSafe settingsFile
  settingsList <- case maybeReadFuzzy settingsStr of
    Just s -> pure s
    Nothing -> throwE $ SettingsError_BadData $
      "Can't parse " ++ show settingsFile
  targetStr <- readFileSafe targetFile
  target <- case maybeReadFuzzy @Target targetStr of
    Just s -> pure s
    Nothing -> throwE $ SettingsError_BadData $
      "Can't parse as Target " ++ show targetFile
  let mySettings = Map.fromList settingsList
      getBooleanSetting :: String -> ExceptT SettingsError m Bool
      getBooleanSetting key = either pgmError pure $
        getRawBooleanSetting settingsFile mySettings key

  -- see Note [topdir: How GHC finds its files]
  -- NB: top_dir is assumed to be in standard Unix
  -- format, '/' separated
  mtool_dir <- liftIO $ findToolDir top_dir
        -- see Note [tooldir: How GHC finds mingw on Windows]

  let getSetting_raw key = either pgmError pure $
        getRawSetting settingsFile mySettings key
      getSetting_topDir top key = either pgmError pure $
        getRawFilePathSetting top settingsFile mySettings key
      getSetting_toolDir top tool key =
        expandToolDir tool <$> getSetting_topDir top key
      getSetting key = getSetting_topDir top_dir key
      getToolSetting key = getSetting_toolDir top_dir mtool_dir key

      expandDirVars top tool = expandToolDir tool . expandTopDir top

      getToolPath :: (Target -> Program) -> String
      getToolPath key = expandDirVars top_dir mtool_dir (prgPath . key $ target)

      getMaybeToolPath :: (Target -> Maybe Program) -> String
      getMaybeToolPath key = getToolPath (fromMaybe (Program "" []) . key)

      getToolFlags :: (Target -> Program) -> [String]
      getToolFlags key = expandDirVars top_dir mtool_dir <$> (prgFlags . key $ target)

      getTool :: (Target -> Program) -> (String, [String])
      getTool key = (getToolPath key, getToolFlags key)

  -- See Note [Settings file] for a little more about this file. We're
  -- just partially applying those functions and throwing 'Left's; they're
  -- written in a very portable style to keep ghc-boot light.
  targetHasLibm <- getBooleanSetting "target has libm"
  let
    (cc_prog, cc_args0)  = getTool (ccProgram . tgtCCompiler)
    (cxx_prog, cxx_args) = getTool (cxxProgram . tgtCxxCompiler)
    (cpp_prog, cpp_args) = getTool (cppProgram . tgtCPreprocessor)
    (hs_cpp_prog, hs_cpp_args) = getTool (hsCppProgram . tgtHsCPreprocessor)
    (js_cpp_prog, js_cpp_args) = getTool (maybe (Program "" []) jsCppProgram . tgtJsCPreprocessor)
    (cmmCpp_prog, cmmCpp_args) = getTool (cmmCppProgram . tgtCmmCPreprocessor)

    platform = getTargetPlatform targetHasLibm target

    unreg_cc_args = if platformUnregisterised platform
                    then ["-DNO_REGS", "-DUSE_MINIINTERPRETER"]
                    else []
    cc_args = cc_args0 ++ unreg_cc_args

    -- The extra flags we need to pass gcc when we invoke it to compile .hc code.
    --
    -- -fwrapv is needed for gcc to emit well-behaved code in the presence of
    -- integer wrap around (#952).
    extraGccViaCFlags = if platformUnregisterised platform
                          -- configure guarantees cc support these flags
                          then ["-fwrapv", "-fno-builtin"]
                          else []

  -- The package database is either a relative path to the location of the settings file
  -- OR an absolute path.
  -- In case the path is absolute then top_dir </> abs_path == abs_path
  --         the path is relative then top_dir </> rel_path == top_dir </> rel_path
  globalpkgdb_path <- installed <$> getSetting "Relative Global Package DB"

  let ghc_usage_msg_path  = installed "ghc-usage.txt"
      ghci_usage_msg_path = installed "ghci-usage.txt"

  -- For all systems, unlit, split, mangle are GHC utilities
  -- architecture-specific stuff is done when building Config.hs
  unlit_path <- getToolSetting "unlit command"

  -- Other things being equal, 'as' is simply 'gcc'
  let (cc_link, cc_link_args) = getTool (ccLinkProgram . tgtCCompilerLink)
      as_prog      = cc_prog
      as_args      = map Option cc_args
      ld_prog      = cc_link
      ld_args      = map Option (cc_args ++ cc_link_args)
      ld_r         = do
        ld_r_prog <- tgtMergeObjs target
        let (ld_r_path, ld_r_args) = getTool (mergeObjsProgram . const ld_r_prog)
        pure (ld_r_path, map Option ld_r_args)
      iserv_prog   = libexec "ghc-iserv"

  targetRTSLinkerOnlySupportsSharedLibs <- getBooleanSetting "target RTS linker only supports shared libraries"
  ghcWithInterpreter <- getBooleanSetting "Use interpreter"

  baseUnitId <- getSetting_raw "base unit-id"

  return $ Settings
    { sGhcNameVersion = GhcNameVersion
      { ghcNameVersion_programName = "ghc"
      , ghcNameVersion_projectVersion = cProjectVersion
      }

    , sFileSettings = FileSettings
      { fileSettings_ghcUsagePath   = ghc_usage_msg_path
      , fileSettings_ghciUsagePath  = ghci_usage_msg_path
      , fileSettings_toolDir        = mtool_dir
      , fileSettings_topDir         = top_dir
      , fileSettings_globalPackageDatabase = globalpkgdb_path
      }

    , sUnitSettings = UnitSettings
      {
        unitSettings_baseUnitId = stringToUnitId baseUnitId
      }

    , sToolSettings = ToolSettings
      { toolSettings_ldSupportsCompactUnwind = ccLinkSupportsCompactUnwind $ tgtCCompilerLink target
      , toolSettings_ldSupportsFilelist      = ccLinkSupportsFilelist      $ tgtCCompilerLink target
      , toolSettings_ldSupportsSingleModule  = ccLinkSupportsSingleModule  $ tgtCCompilerLink target
      , toolSettings_ldIsGnuLd               = ccLinkIsGnu                 $ tgtCCompilerLink target
      , toolSettings_ccSupportsNoPie         = ccLinkSupportsNoPie         $ tgtCCompilerLink target
      , toolSettings_mergeObjsSupportsResponseFiles
                                      = maybe False mergeObjsSupportsResponseFiles
                                                         $ tgtMergeObjs target
      , toolSettings_arSupportsDashL  = arSupportsDashL  $ tgtAr target
      , toolSettings_cmmCppSupportsG0 = cmmCppSupportsG0 $ tgtCmmCPreprocessor target

      , toolSettings_pgm_L       = unlit_path
      , toolSettings_pgm_P       = (hs_cpp_prog, map Option hs_cpp_args)
      , toolSettings_pgm_JSP     = (js_cpp_prog, map Option js_cpp_args)
      , toolSettings_pgm_CmmP    = (cmmCpp_prog, map Option cmmCpp_args)
      , toolSettings_pgm_F       = ""
      , toolSettings_pgm_c       = cc_prog
      , toolSettings_pgm_cxx     = cxx_prog
      , toolSettings_pgm_cpp     = (cpp_prog, map Option cpp_args)
      , toolSettings_pgm_a       = (as_prog, as_args)
      , toolSettings_pgm_l       = (ld_prog, ld_args)
      , toolSettings_pgm_lm      = ld_r
      , toolSettings_pgm_windres = getMaybeToolPath tgtWindres
      , toolSettings_pgm_ar      = getToolPath (arMkArchive . tgtAr)
      , toolSettings_pgm_otool   = getMaybeToolPath tgtOtool
      , toolSettings_pgm_install_name_tool = getMaybeToolPath tgtInstallNameTool
      , toolSettings_pgm_ranlib  = getMaybeToolPath (fmap ranlibProgram . tgtRanlib)
      , toolSettings_pgm_lo      = (getMaybeToolPath tgtOpt,[])
      , toolSettings_pgm_lc      = (getMaybeToolPath tgtLlc,[])
      , toolSettings_pgm_las     = second (map Option) $
                                   getTool (fromMaybe (Program "" []) . tgtLlvmAs)
      , toolSettings_pgm_i       = iserv_prog
      , toolSettings_opt_L       = []
      , toolSettings_opt_P       = []
      , toolSettings_opt_JSP     = []
      , toolSettings_opt_CmmP    = []
      , toolSettings_opt_P_fingerprint   = fingerprint0
      , toolSettings_opt_JSP_fingerprint = fingerprint0
      , toolSettings_opt_CmmP_fingerprint = fingerprint0
      , toolSettings_opt_F       = []
      , toolSettings_opt_c       = cc_args
      , toolSettings_opt_cxx     = cxx_args
      , toolSettings_opt_a       = []
      , toolSettings_opt_l       = []
      , toolSettings_opt_lm      = []
      , toolSettings_opt_windres = []
      , toolSettings_opt_lo      = []
      , toolSettings_opt_lc      = []
      , toolSettings_opt_las     = []
      , toolSettings_opt_i       = []

      , toolSettings_extraGccViaCFlags = extraGccViaCFlags
      }

    , sTargetPlatform = platform
    , sPlatformMisc = PlatformMisc
      { platformMisc_targetPlatformString = targetPlatformTriple target
      , platformMisc_ghcWithInterpreter = ghcWithInterpreter
      , platformMisc_libFFI = tgtUseLibffiForAdjustors target
      , platformMisc_llvmTarget = tgtLlvmTarget target
      , platformMisc_targetRTSLinkerOnlySupportsSharedLibs = targetRTSLinkerOnlySupportsSharedLibs
      }

    , sRawSettings    = settingsList
    , sRawTarget      = target
    }

getTargetPlatform :: Bool {-^ Does target have libm -} -> Target -> Platform
getTargetPlatform targetHasLibm Target{..} = Platform
    { platformArchOS    = tgtArchOs
    , platformWordSize  = case tgtWordSize of WS4 -> PW4
                                              WS8 -> PW8
    , platformByteOrder = tgtEndianness
    , platformUnregisterised = tgtUnregisterised
    , platformHasGnuNonexecStack = tgtSupportsGnuNonexecStack
    , platformHasIdentDirective = tgtSupportsIdentDirective
    , platformHasSubsectionsViaSymbols = tgtSupportsSubsectionsViaSymbols
    , platformIsCrossCompiling = not tgtLocallyExecutable
    , platformLeadingUnderscore = tgtSymbolsHaveLeadingUnderscore
    , platformTablesNextToCode  = tgtTablesNextToCode
    , platformHasLibm = targetHasLibm
    , platform_constants = Nothing -- will be filled later when loading (or building) the RTS unit
    }
