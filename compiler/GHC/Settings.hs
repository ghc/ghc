

-- | Run-time settings
module GHC.Settings
  ( Settings (..)
  , ToolSettings (..)
  , FileSettings (..)
  , UnitSettings(..)
  , GhcNameVersion (..)
  , Platform (..)
  , PlatformMisc (..)
  -- * Accessors
  , dynLibSuffix
  , sProgramName
  , sProjectVersion
  , sGhcUsagePath
  , sGhciUsagePath
  , sToolDir
  , sTopDir
  , sGlobalPackageDatabasePath
  , sLdSupportsCompactUnwind
  , sLdSupportsFilelist
  , sMergeObjsSupportsResponseFiles
  , sLdIsGnuLd
  , sGccSupportsNoPie
  , sArSupportsDashL
  , sPgm_L
  , sPgm_P
  , sPgm_JSP
  , sPgm_CmmP
  , sPgm_F
  , sPgm_c
  , sPgm_cxx
  , sPgm_cpp
  , sPgm_a
  , sPgm_l
  , sPgm_lm
  , sPgm_windres
  , sPgm_ar
  , sPgm_otool
  , sPgm_install_name_tool
  , sPgm_ranlib
  , sPgm_lo
  , sPgm_lc
  , sPgm_las
  , sPgm_i
  , sOpt_L
  , sOpt_P
  , sOpt_P_fingerprint
  , sOpt_JSP
  , sOpt_JSP_fingerprint
  , sOpt_CmmP
  , sOpt_CmmP_fingerprint
  , sOpt_F
  , sOpt_c
  , sOpt_cxx
  , sOpt_a
  , sOpt_l
  , sOpt_lm
  , sOpt_windres
  , sOpt_lo
  , sOpt_lc
  , sOpt_i
  , sExtraGccViaCFlags
  , sTargetPlatformString
  , sGhcWithInterpreter
  , sLibFFI
  , sTargetRTSLinkerOnlySupportsSharedLibs
  ) where

import GHC.Prelude

import GHC.Utils.CliOption
import GHC.Utils.Fingerprint
import GHC.Platform
import GHC.Unit.Types
import GHC.Toolchain.Target

data Settings = Settings
  { sGhcNameVersion    :: {-# UNPACk #-} !GhcNameVersion
  , sFileSettings      :: {-# UNPACK #-} !FileSettings
  , sTargetPlatform    :: Platform       -- Filled in by SysTools
  , sToolSettings      :: {-# UNPACK #-} !ToolSettings
  , sPlatformMisc      :: {-# UNPACK #-} !PlatformMisc
  , sUnitSettings      :: !UnitSettings

  -- You shouldn't need to look things up in rawSettings directly.
  -- They should have their own fields instead.
  , sRawSettings       :: [(String, String)]

  -- Store the target to print out information about the raw target description
  -- (e.g. in --info)
  , sRawTarget         :: Target
  }

data UnitSettings = UnitSettings { unitSettings_baseUnitId :: !UnitId }

-- | Settings for other executables GHC calls.
--
-- Probably should further split down by phase, or split between
-- platform-specific and platform-agnostic.
data ToolSettings = ToolSettings
  { toolSettings_ldSupportsCompactUnwind :: Bool
  , toolSettings_ldSupportsFilelist      :: Bool
  , toolSettings_ldSupportsSingleModule  :: Bool
  , toolSettings_mergeObjsSupportsResponseFiles :: Bool
  , toolSettings_ldIsGnuLd               :: Bool
  , toolSettings_ccSupportsNoPie         :: Bool
  , toolSettings_arSupportsDashL         :: Bool
  , toolSettings_cmmCppSupportsG0        :: Bool

  -- commands for particular phases
  , toolSettings_pgm_L       :: String
  , -- | The Haskell C preprocessor and default options (not added by -optP)
    toolSettings_pgm_P       :: (String, [Option])
  , -- | The JavaScript C preprocessor and default options (not added by -optP)
    toolSettings_pgm_JSP       :: (String, [Option])
  , -- | The C-- C Preprocessor and default options (not added by -optP)
    toolSettings_pgm_CmmP    :: (String, [Option])
  , toolSettings_pgm_F       :: String
  , toolSettings_pgm_c       :: String
  , toolSettings_pgm_cxx     :: String
  , -- | The C preprocessor (distinct from the Haskell C preprocessor!)
    toolSettings_pgm_cpp     :: (String, [Option])
  , toolSettings_pgm_a       :: (String, [Option])
  , toolSettings_pgm_l       :: (String, [Option])
  , toolSettings_pgm_lm      :: Maybe (String, [Option])
    -- ^ N.B. On Windows we don't have a linker which supports object
    -- merging, hence the 'Maybe'. See Note [Object merging] in
    -- "GHC.Driver.Pipeline.Execute" for details.
  , toolSettings_pgm_windres :: String
  , toolSettings_pgm_ar      :: String
  , toolSettings_pgm_otool   :: String
  , toolSettings_pgm_install_name_tool :: String
  , toolSettings_pgm_ranlib  :: String
  , -- | LLVM: opt llvm optimiser
    toolSettings_pgm_lo      :: (String, [Option])
  , -- | LLVM: llc static compiler
    toolSettings_pgm_lc      :: (String, [Option])
    -- | LLVM: assembler
  , toolSettings_pgm_las     :: (String, [Option])
  , toolSettings_pgm_i       :: String

  -- options for particular phases
  , toolSettings_opt_L             :: [String]
  , toolSettings_opt_P             :: [String]
  , toolSettings_opt_JSP           :: [String]
  , toolSettings_opt_CmmP          :: [String]
  , -- | cached Fingerprint of sOpt_P
    -- See Note [Repeated -optP hashing]
    toolSettings_opt_P_fingerprint   :: Fingerprint
  , -- | cached Fingerprint of sOpt_JSP
    -- See Note [Repeated -optP hashing]
    toolSettings_opt_JSP_fingerprint :: Fingerprint
  , -- | cached Fingerprint of sOpt_CmmP
    -- See Note [Repeated -optP hashing]
    toolSettings_opt_CmmP_fingerprint :: Fingerprint
  , toolSettings_opt_F             :: [String]
  , toolSettings_opt_c             :: [String]
  , toolSettings_opt_cxx           :: [String]
  , toolSettings_opt_a             :: [String]
  , toolSettings_opt_l             :: [String]
  , toolSettings_opt_lm            :: [String]
  , toolSettings_opt_windres       :: [String]
  , -- | LLVM: llvm optimiser
    toolSettings_opt_lo            :: [String]
  , -- | LLVM: llc static compiler
    toolSettings_opt_lc            :: [String]
  , toolSettings_opt_las           :: [String]
  , -- | iserv options
    toolSettings_opt_i             :: [String]

  , toolSettings_extraGccViaCFlags :: [String]
  }


-- | Paths to various files and directories used by GHC, including those that
-- provide more settings.
data FileSettings = FileSettings
  { fileSettings_ghcUsagePath          :: FilePath       -- ditto
  , fileSettings_ghciUsagePath         :: FilePath       -- ditto
  , fileSettings_toolDir               :: Maybe FilePath -- ditto
  , fileSettings_topDir                :: FilePath       -- ditto
  , fileSettings_globalPackageDatabase :: FilePath
  }


-- | Settings for what GHC this is.
data GhcNameVersion = GhcNameVersion
  { ghcNameVersion_programName    :: String
  , ghcNameVersion_projectVersion :: String
  }

-- | Dynamic library suffix
dynLibSuffix :: GhcNameVersion -> String
dynLibSuffix (GhcNameVersion name ver) = '-':name ++ ver

-----------------------------------------------------------------------------
-- Accessors from 'Settings'

sProgramName         :: Settings -> String
sProgramName = ghcNameVersion_programName . sGhcNameVersion
sProjectVersion      :: Settings -> String
sProjectVersion = ghcNameVersion_projectVersion . sGhcNameVersion

sGhcUsagePath        :: Settings -> FilePath
sGhcUsagePath = fileSettings_ghcUsagePath . sFileSettings
sGhciUsagePath       :: Settings -> FilePath
sGhciUsagePath = fileSettings_ghciUsagePath . sFileSettings
sToolDir             :: Settings -> Maybe FilePath
sToolDir = fileSettings_toolDir . sFileSettings
sTopDir              :: Settings -> FilePath
sTopDir = fileSettings_topDir . sFileSettings
sGlobalPackageDatabasePath :: Settings -> FilePath
sGlobalPackageDatabasePath = fileSettings_globalPackageDatabase . sFileSettings

sLdSupportsCompactUnwind :: Settings -> Bool
sLdSupportsCompactUnwind = toolSettings_ldSupportsCompactUnwind . sToolSettings
sLdSupportsFilelist :: Settings -> Bool
sLdSupportsFilelist = toolSettings_ldSupportsFilelist . sToolSettings
sMergeObjsSupportsResponseFiles :: Settings -> Bool
sMergeObjsSupportsResponseFiles = toolSettings_mergeObjsSupportsResponseFiles . sToolSettings
sLdIsGnuLd :: Settings -> Bool
sLdIsGnuLd = toolSettings_ldIsGnuLd . sToolSettings
sGccSupportsNoPie :: Settings -> Bool
sGccSupportsNoPie = toolSettings_ccSupportsNoPie . sToolSettings
sArSupportsDashL :: Settings -> Bool
sArSupportsDashL = toolSettings_arSupportsDashL . sToolSettings

sPgm_L :: Settings -> String
sPgm_L = toolSettings_pgm_L . sToolSettings
sPgm_P :: Settings -> (String, [Option])
sPgm_P = toolSettings_pgm_P . sToolSettings
sPgm_JSP :: Settings -> (String, [Option])
sPgm_JSP = toolSettings_pgm_JSP . sToolSettings
sPgm_CmmP :: Settings -> (String, [Option])
sPgm_CmmP = toolSettings_pgm_CmmP . sToolSettings
sPgm_F :: Settings -> String
sPgm_F = toolSettings_pgm_F . sToolSettings
sPgm_c :: Settings -> String
sPgm_c = toolSettings_pgm_c . sToolSettings
sPgm_cxx :: Settings -> String
sPgm_cxx = toolSettings_pgm_cxx . sToolSettings
sPgm_cpp :: Settings -> (String, [Option])
sPgm_cpp = toolSettings_pgm_cpp . sToolSettings
sPgm_a :: Settings -> (String, [Option])
sPgm_a = toolSettings_pgm_a . sToolSettings
sPgm_l :: Settings -> (String, [Option])
sPgm_l = toolSettings_pgm_l . sToolSettings
sPgm_lm :: Settings -> Maybe (String, [Option])
sPgm_lm = toolSettings_pgm_lm . sToolSettings
sPgm_windres :: Settings -> String
sPgm_windres = toolSettings_pgm_windres . sToolSettings
sPgm_ar :: Settings -> String
sPgm_ar = toolSettings_pgm_ar . sToolSettings
sPgm_otool :: Settings -> String
sPgm_otool = toolSettings_pgm_otool . sToolSettings
sPgm_install_name_tool :: Settings -> String
sPgm_install_name_tool = toolSettings_pgm_install_name_tool . sToolSettings
sPgm_ranlib :: Settings -> String
sPgm_ranlib = toolSettings_pgm_ranlib . sToolSettings
sPgm_lo :: Settings -> (String, [Option])
sPgm_lo = toolSettings_pgm_lo . sToolSettings
sPgm_lc :: Settings -> (String, [Option])
sPgm_lc = toolSettings_pgm_lc . sToolSettings
sPgm_las :: Settings -> (String, [Option])
sPgm_las = toolSettings_pgm_las . sToolSettings
sPgm_i :: Settings -> String
sPgm_i = toolSettings_pgm_i . sToolSettings
sOpt_L :: Settings -> [String]
sOpt_L = toolSettings_opt_L . sToolSettings
sOpt_P :: Settings -> [String]
sOpt_P = toolSettings_opt_P . sToolSettings
sOpt_P_fingerprint :: Settings -> Fingerprint
sOpt_P_fingerprint = toolSettings_opt_P_fingerprint . sToolSettings
sOpt_JSP :: Settings -> [String]
sOpt_JSP = toolSettings_opt_JSP . sToolSettings
sOpt_JSP_fingerprint :: Settings -> Fingerprint
sOpt_JSP_fingerprint = toolSettings_opt_JSP_fingerprint . sToolSettings
sOpt_CmmP :: Settings -> [String]
sOpt_CmmP = toolSettings_opt_CmmP . sToolSettings
sOpt_CmmP_fingerprint :: Settings -> Fingerprint
sOpt_CmmP_fingerprint = toolSettings_opt_CmmP_fingerprint . sToolSettings
sOpt_F :: Settings -> [String]
sOpt_F = toolSettings_opt_F . sToolSettings
sOpt_c :: Settings -> [String]
sOpt_c = toolSettings_opt_c . sToolSettings
sOpt_cxx :: Settings -> [String]
sOpt_cxx = toolSettings_opt_cxx . sToolSettings
sOpt_a :: Settings -> [String]
sOpt_a = toolSettings_opt_a . sToolSettings
sOpt_l :: Settings -> [String]
sOpt_l = toolSettings_opt_l . sToolSettings
sOpt_lm :: Settings -> [String]
sOpt_lm = toolSettings_opt_lm . sToolSettings
sOpt_windres :: Settings -> [String]
sOpt_windres = toolSettings_opt_windres . sToolSettings
sOpt_lo :: Settings -> [String]
sOpt_lo = toolSettings_opt_lo . sToolSettings
sOpt_lc :: Settings -> [String]
sOpt_lc = toolSettings_opt_lc . sToolSettings
sOpt_i :: Settings -> [String]
sOpt_i = toolSettings_opt_i . sToolSettings

sExtraGccViaCFlags :: Settings -> [String]
sExtraGccViaCFlags = toolSettings_extraGccViaCFlags . sToolSettings

sTargetPlatformString :: Settings -> String
sTargetPlatformString = platformMisc_targetPlatformString . sPlatformMisc
sGhcWithInterpreter :: Settings -> Bool
sGhcWithInterpreter = platformMisc_ghcWithInterpreter . sPlatformMisc
sLibFFI :: Settings -> Bool
sLibFFI = platformMisc_libFFI . sPlatformMisc

sTargetRTSLinkerOnlySupportsSharedLibs :: Settings -> Bool
sTargetRTSLinkerOnlySupportsSharedLibs = platformMisc_targetRTSLinkerOnlySupportsSharedLibs . sPlatformMisc
