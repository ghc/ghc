module Settings.Builders.Common (
    includesArgs, cIncludeArgs, ldArgs, cArgs, cWarnings,
    argSetting, argSettingList, argStagedBuilderPath, argStagedSettingList
    ) where

import Base
import Expression
import Oracles.Config.Flag
import Oracles.Config.Setting
import Oracles.PackageData
import Settings
import UserSettings

includes :: [FilePath]
includes = ["includes", "includes/dist-derivedconstants/header"]

includesArgs :: Args
includesArgs = append $ map ("-I" ++) includes

cIncludeArgs :: Args
cIncludeArgs = do
    pkg     <- getPackage
    path    <- getBuildPath
    incDirs <- getPkgDataList IncludeDirs
    depDirs <- getPkgDataList DepIncludeDirs
    mconcat [ arg $ "-I" ++ path
            , arg $ "-I" ++ path -/- "autogen"
            , append [ "-I" ++ pkgPath pkg -/- dir | dir <- incDirs ]
            , append [ "-I" ++       unifyPath dir | dir <- depDirs ] ]

ldArgs :: Args
ldArgs = mempty

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
argSettingList = (append =<<) . getSettingList

argStagedSettingList :: (Stage -> SettingList) -> Args
argStagedSettingList ss = argSettingList . ss =<< getStage

argStagedBuilderPath :: (Stage -> Builder) -> Args
argStagedBuilderPath sb = argM . builderPath . sb =<< getStage
