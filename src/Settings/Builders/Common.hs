module Settings.Builders.Common (
    includesArgs, cIncludeArgs, cArgs, cWarnings,
    argSetting, argSettingList, argStagedBuilderPath, argStagedSettingList
    ) where

import Base
import Expression
import Oracles.Config.Flag
import Oracles.Config.Setting
import Oracles.PackageData
import Settings

includes :: [FilePath]
includes = [ "includes", "includes/dist-derivedconstants/header" ]

includesArgs :: Args
includesArgs = append $ map ("-I" ++) includes

cIncludeArgs :: Args
cIncludeArgs = do
    stage   <- getStage
    pkg     <- getPackage
    incDirs <- getPkgDataList IncludeDirs
    depDirs <- getPkgDataList DepIncludeDirs
    let buildPath = targetPath stage pkg -/- "build"
    mconcat [ arg $ "-I" ++ buildPath
            , arg $ "-I" ++ buildPath -/- "autogen"
            , append [ "-I" ++ pkgPath pkg -/- dir | dir <- incDirs ]
            , append [ "-I" ++                 dir | dir <- depDirs ] ]

-- TODO: put all validating options together in one file
cArgs :: Args
cArgs = validating ? cWarnings

-- TODO: should be in a different file
cWarnings :: Args
cWarnings = do
    let gccGe46 = notM $ (flag GccIsClang ||^ flag GccLt46)
    mconcat [ turnWarningsIntoErrors ? arg "-Werror"
            , arg "-Wall"
            , flag GccIsClang ? arg "-Wno-unknown-pragmas"
            , gccGe46 ? notM windowsHost ? arg "-Werror=unused-but-set-variable"
            , gccGe46 ? arg "-Wno-error=inline" ]

argM :: Action String -> Args
argM = (arg =<<) . lift

argSetting :: Setting -> Args
argSetting = argM . setting

argSettingList :: SettingList -> Args
argSettingList = (append =<<) . lift . settingList

argStagedSettingList :: (Stage -> SettingList) -> Args
argStagedSettingList ss = (argSettingList . ss) =<< getStage

argStagedBuilderPath :: (Stage -> Builder) -> Args
argStagedBuilderPath sb = (argM . builderPath . sb) =<< getStage
