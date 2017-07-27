module Settings.Builders.Common (
    module Base,
    module Expression,
    module GHC,
    module Oracles.Config.Flag,
    module Oracles.Config.Setting,
    module Oracles.PackageData,
    module Oracles.Path,
    module Predicate,
    module Settings,
    module Settings.Path,
    module UserSettings,
    cIncludeArgs, ldArgs, cArgs, cWarnings, argStagedBuilderPath,
    argStagedSettingList, bootPackageDatabaseArgs
    ) where

import Base
import Expression
import GHC
import Oracles.Config.Flag
import Oracles.Config.Setting
import Oracles.PackageData
import Oracles.Path
import Predicate
import Settings
import Settings.Path
import UserSettings

cIncludeArgs :: Args
cIncludeArgs = do
    pkg     <- getPackage
    path    <- getBuildPath
    incDirs <- getPkgDataList IncludeDirs
    depDirs <- getPkgDataList DepIncludeDirs
    mconcat [ arg "-Iincludes"
            , arg $ "-I" ++ generatedPath
            , arg $ "-I" ++ path
            , append [ "-I" ++ pkgPath pkg -/- dir | dir <- incDirs ]
            , append [ "-I" ++       unifyPath dir | dir <- depDirs ] ]

ldArgs :: Args
ldArgs = mempty

cArgs :: Args
cArgs = mempty

-- TODO: should be in a different file
cWarnings :: Args
cWarnings = do
    let gccGe46 = notM $ (flag GccIsClang ||^ flag GccLt46)
    mconcat [ arg "-Wall"
            , flag GccIsClang ? arg "-Wno-unknown-pragmas"
            , gccGe46 ? notM windowsHost ? arg "-Werror=unused-but-set-variable"
            , gccGe46 ? arg "-Wno-error=inline" ]

argSettingList :: SettingList -> Args
argSettingList = (append =<<) . getSettingList

argStagedSettingList :: (Stage -> SettingList) -> Args
argStagedSettingList ss = argSettingList . ss =<< getStage

argStagedBuilderPath :: (Stage -> Builder) -> Args
argStagedBuilderPath sb = do
    stage <- getStage
    path <- expr $ builderPath (sb stage)
    arg path

bootPackageDatabaseArgs :: Args
bootPackageDatabaseArgs = do
    stage <- getStage
    expr $ need [packageDbStamp stage]
    stage0 ? do
        path   <- getTopDirectory
        prefix <- ifM (builder Ghc) (return "-package-db ") (return "--package-db=")
        arg $ prefix ++ path -/- inplacePackageDbDirectory Stage0
