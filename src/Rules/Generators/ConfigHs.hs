module Rules.Generators.ConfigHs (generateConfigHs) where

import Base
import Expression
import GHC
import Oracles
import Settings
import Rules.Generators.Common

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
    let cIntegerLibraryType | integerLibrary == integerGmp    = "IntegerGMP"
                            | integerLibrary == integerSimple = "IntegerSimple"
                            | otherwise = error $ "Unknown integer library: "
                                          ++ show integerLibrary ++ "."
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
        , "cProjectName          = " ++ quote cProjectName
        , "cProjectGitCommitId   :: String"
        , "cProjectGitCommitId   = " ++ quote cProjectGitCommitId
        , "cProjectVersion       :: String"
        , "cProjectVersion       = " ++ quote cProjectVersion
        , "cProjectVersionInt    :: String"
        , "cProjectVersionInt    = " ++ quote cProjectVersionInt
        , "cProjectPatchLevel    :: String"
        , "cProjectPatchLevel    = " ++ quote cProjectPatchLevel
        , "cProjectPatchLevel1   :: String"
        , "cProjectPatchLevel1   = " ++ quote cProjectPatchLevel1
        , "cProjectPatchLevel2   :: String"
        , "cProjectPatchLevel2   = " ++ quote cProjectPatchLevel2
        , "cBooterVersion        :: String"
        , "cBooterVersion        = " ++ quote cBooterVersion
        , "cStage                :: String"
        , "cStage                = show (STAGE :: Int)"
        , "cIntegerLibrary       :: String"
        , "cIntegerLibrary       = " ++ quote (pkgNameString integerLibrary)
        , "cIntegerLibraryType   :: IntegerLibrary"
        , "cIntegerLibraryType   = " ++ cIntegerLibraryType
        , "cSupportsSplitObjs    :: String"
        , "cSupportsSplitObjs    = " ++ quote cSupportsSplitObjs
        , "cGhcWithInterpreter   :: String"
        , "cGhcWithInterpreter   = " ++ quote cGhcWithInterpreter
        , "cGhcWithNativeCodeGen :: String"
        , "cGhcWithNativeCodeGen = " ++ quote cGhcWithNativeCodeGen
        , "cGhcWithSMP           :: String"
        , "cGhcWithSMP           = " ++ quote cGhcWithSMP
        , "cGhcRTSWays           :: String"
        , "cGhcRTSWays           = " ++ quote cGhcRTSWays
        , "cGhcEnableTablesNextToCode :: String"
        , "cGhcEnableTablesNextToCode = " ++ quote cGhcEnableTablesNextToCode
        , "cLeadingUnderscore    :: String"
        , "cLeadingUnderscore    = " ++ quote cLeadingUnderscore
        , "cGHC_UNLIT_PGM        :: String"
        , "cGHC_UNLIT_PGM        = " ++ quote cGHC_UNLIT_PGM
        , "cGHC_SPLIT_PGM        :: String"
        , "cGHC_SPLIT_PGM        = " ++ quote "ghc-split"
        , "cLibFFI               :: Bool"
        , "cLibFFI               = " ++ show cLibFFI
        , "cGhcThreaded :: Bool"
        , "cGhcThreaded = " ++ show (threaded `elem` rtsWays)
        , "cGhcDebugged :: Bool"
        , "cGhcDebugged = " ++ show ghcDebugged
        , "cGhcRtsWithLibdw :: Bool"
        , "cGhcRtsWithLibdw = " ++ show cGhcRtsWithLibdw ]
