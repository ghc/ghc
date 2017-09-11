module Settings.Builders.Common (
    module Base,
    module Expression,
    module Oracles.Flag,
    module Oracles.PackageData,
    module Oracles.Setting,
    module Settings,
    module UserSettings,
    cIncludeArgs, ldArgs, cArgs, cWarnings, bootPackageDatabaseArgs
    ) where

import Base
import Expression
import Oracles.Flag
import Oracles.PackageData
import Oracles.Setting
import Settings
import UserSettings

cIncludeArgs :: Args
cIncludeArgs = do
    pkg     <- getPackage
    root    <- getBuildRoot
    path    <- getBuildPath
    incDirs <- getPkgDataList IncludeDirs
    depDirs <- getPkgDataList DepIncludeDirs
    cross   <- expr crossCompiling
    compilerOrGhc <- package compiler ||^ package ghc
    mconcat [ not (cross && compilerOrGhc) ? arg "-Iincludes"
            , arg $ "-I" ++ root -/- generatedDir
            , arg $ "-I" ++ path
            , pure [ "-I" ++ pkgPath pkg -/- dir | dir <- incDirs ]
            , pure [ "-I" ++       unifyPath dir | dir <- depDirs ] ]

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

bootPackageDatabaseArgs :: Args
bootPackageDatabaseArgs = do
    root  <- getBuildRoot
    stage <- getStage
    let dbDir | stage == Stage0 = root -/- stage0PackageDbDir
              | otherwise       = inplacePackageDbPath
    expr $ need [dbDir -/- packageDbStamp]
    stage0 ? do
        top    <- expr topDirectory
        root   <- getBuildRoot
        prefix <- ifM (builder Ghc) (return "-package-db ") (return "--package-db=")
        arg $ prefix ++ top -/- root -/- stage0PackageDbDir
