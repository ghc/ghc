module Rules.Generators.ConfigHs (generateConfigHs) where

import Base
import Expression
import Flavour
import GHC
import Oracles.Config.Flag
import Oracles.Config.Setting
import Rules.Generators.Common
import Settings

generateConfigHs :: Expr String
generateConfigHs = do
    trackSource "Rules/Generators/ConfigHs.hs"
    cProjectName        <- getSetting ProjectName
    cProjectGitCommitId <- getSetting ProjectGitCommitId
    cProjectVersion     <- getSetting ProjectVersion
    cProjectVersionInt  <- getSetting ProjectVersionInt
    cProjectPatchLevel  <- getSetting ProjectPatchLevel
    cProjectPatchLevel1 <- getSetting ProjectPatchLevel1
    cProjectPatchLevel2 <- getSetting ProjectPatchLevel2
    cBooterVersion      <- getSetting GhcVersion
    let cIntegerLibraryType
            | integerLibrary flavour == integerGmp    = "IntegerGMP"
            | integerLibrary flavour == integerSimple = "IntegerSimple"
            | otherwise = error $ "Unknown integer library: " ++ integerLibraryName
    cSupportsSplitObjs         <- yesNo supportsSplitObjects
    cGhcWithInterpreter        <- yesNo ghcWithInterpreter
    cGhcWithNativeCodeGen      <- yesNo ghcWithNativeCodeGen
    cGhcWithSMP                <- yesNo ghcWithSMP
    cGhcEnableTablesNextToCode <- yesNo ghcEnableTablesNextToCode
    cLeadingUnderscore         <- yesNo $ flag LeadingUnderscore
    cGHC_UNLIT_PGM             <- fmap takeFileName $ getBuilderPath Unlit
    cLibFFI                    <- lift useLibFFIForAdjustors
    rtsWays                    <- getRtsWays
    cGhcRtsWithLibdw           <- getFlag WithLibdw
    let cGhcRTSWays = unwords $ map show rtsWays
    return $ unlines
        [ "{-# LANGUAGE CPP #-}"
        , "module Config where"
        , ""
        , "#include \"ghc_boot_platform.h\""
        , ""
        , "data IntegerLibrary = IntegerGMP"
        , "                    | IntegerSimple"
        , "                    deriving Eq"
        , ""
        , "cBuildPlatformString :: String"
        , "cBuildPlatformString = BuildPlatform_NAME"
        , "cHostPlatformString :: String"
        , "cHostPlatformString = HostPlatform_NAME"
        , "cTargetPlatformString :: String"
        , "cTargetPlatformString = TargetPlatform_NAME"
        , ""
        , "cProjectName          :: String"
        , "cProjectName          = " ++ show cProjectName
        , "cProjectGitCommitId   :: String"
        , "cProjectGitCommitId   = " ++ show cProjectGitCommitId
        , "cProjectVersion       :: String"
        , "cProjectVersion       = " ++ show cProjectVersion
        , "cProjectVersionInt    :: String"
        , "cProjectVersionInt    = " ++ show cProjectVersionInt
        , "cProjectPatchLevel    :: String"
        , "cProjectPatchLevel    = " ++ show cProjectPatchLevel
        , "cProjectPatchLevel1   :: String"
        , "cProjectPatchLevel1   = " ++ show cProjectPatchLevel1
        , "cProjectPatchLevel2   :: String"
        , "cProjectPatchLevel2   = " ++ show cProjectPatchLevel2
        , "cBooterVersion        :: String"
        , "cBooterVersion        = " ++ show cBooterVersion
        , "cStage                :: String"
        , "cStage                = show (STAGE :: Int)"
        , "cIntegerLibrary       :: String"
        , "cIntegerLibrary       = " ++ show integerLibraryName
        , "cIntegerLibraryType   :: IntegerLibrary"
        , "cIntegerLibraryType   = " ++ cIntegerLibraryType
        , "cSupportsSplitObjs    :: String"
        , "cSupportsSplitObjs    = " ++ show cSupportsSplitObjs
        , "cGhcWithInterpreter   :: String"
        , "cGhcWithInterpreter   = " ++ show cGhcWithInterpreter
        , "cGhcWithNativeCodeGen :: String"
        , "cGhcWithNativeCodeGen = " ++ show cGhcWithNativeCodeGen
        , "cGhcWithSMP           :: String"
        , "cGhcWithSMP           = " ++ show cGhcWithSMP
        , "cGhcRTSWays           :: String"
        , "cGhcRTSWays           = " ++ show cGhcRTSWays
        , "cGhcEnableTablesNextToCode :: String"
        , "cGhcEnableTablesNextToCode = " ++ show cGhcEnableTablesNextToCode
        , "cLeadingUnderscore    :: String"
        , "cLeadingUnderscore    = " ++ show cLeadingUnderscore
        , "cGHC_UNLIT_PGM        :: String"
        , "cGHC_UNLIT_PGM        = " ++ show cGHC_UNLIT_PGM
        , "cGHC_SPLIT_PGM        :: String"
        , "cGHC_SPLIT_PGM        = " ++ show "ghc-split"
        , "cLibFFI               :: Bool"
        , "cLibFFI               = " ++ show cLibFFI
        , "cGhcThreaded :: Bool"
        , "cGhcThreaded = " ++ show (threaded `elem` rtsWays)
        , "cGhcDebugged :: Bool"
        , "cGhcDebugged = " ++ show (ghcDebugged flavour)
        , "cGhcRtsWithLibdw :: Bool"
        , "cGhcRtsWithLibdw = " ++ show cGhcRtsWithLibdw ]
