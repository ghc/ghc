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
cWarnings =
    mconcat [ arg "-Wall"
            , flag GccIsClang ? arg "-Wno-unknown-pragmas"
            , notM (flag GccIsClang) ? notM windowsHost ? arg "-Werror=unused-but-set-variable"
            , notM (flag GccIsClang) ? arg "-Wno-error=inline" ]

bootPackageDatabaseArgs :: Args
bootPackageDatabaseArgs = do
    stage  <- getStage
    dbPath <- expr $ packageDbPath stage
    expr $ need [dbPath -/- packageDbStamp]
    stage0 ? do
        top    <- expr topDirectory
        root   <- getBuildRoot
        prefix <- ifM (builder Ghc) (return "-package-db ") (return "--package-db=")
        arg $ prefix ++ top -/- root -/- stage0PackageDbDir
