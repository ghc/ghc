module Settings.Builders.Common (
    module Base,
    module Expression,
    module Oracles.Flag,
    module Oracles.Setting,
    module Settings,
    module UserSettings,
    cIncludeArgs, ldArgs, cArgs, cWarnings,
    packageDatabaseArgs, bootPackageDatabaseArgs
    ) where

import Hadrian.Haskell.Cabal.Type

import Base
import Expression
import Oracles.Flag
import Oracles.Setting
import Packages
import Settings
import UserSettings

cIncludeArgs :: Args
cIncludeArgs = do
    pkg     <- getPackage
    path    <- getBuildPath
    incDirs <- getContextData includeDirs
    depDirs <- getContextData depIncludeDirs
    stage <- getStage
    iconvIncludeDir <- getSetting IconvIncludeDir
    gmpIncludeDir   <- getSetting GmpIncludeDir
    ffiIncludeDir   <- getSetting FfiIncludeDir
    libdwIncludeDir   <- getSetting FfiIncludeDir
    libPath <- expr $ stageLibPath stage
    mconcat [ notStage0 ||^ package compiler ? arg "-Iincludes"
            , arg $ "-I" ++ libPath
            , arg $ "-I" ++ path
            , pure . map ("-I"++) . filter (/= "") $ [iconvIncludeDir, gmpIncludeDir]
            , flag UseSystemFfi ? arg ("-I" ++ ffiIncludeDir)
            , flag WithLibdw ? if not (null libdwIncludeDir) then arg ("-I" ++ libdwIncludeDir) else mempty
            -- Add @incDirs@ in the build directory, since some files generated
            -- with @autoconf@ may end up in the build directory.
            , pure [ "-I" ++ path        -/- dir | dir <- incDirs ]
            -- Add @incDirs@ in the package directory for include files shipped
            -- with the package.
            , pure [ "-I" ++ pkgPath pkg -/- dir | dir <- incDirs ]
            , pure [ "-I" ++       unifyPath dir | dir <- depDirs ] ]

ldArgs :: Args
ldArgs = mempty

cArgs :: Args
cArgs = mempty

-- TODO: should be in a different file
cWarnings :: Args
cWarnings = mconcat
    [ arg "-Wall"
    , flag CcLlvmBackend ? arg "-Wno-unknown-pragmas"
    , notM (flag CcLlvmBackend) ? not windowsHost ? arg "-Werror=unused-but-set-variable"
    , notM (flag CcLlvmBackend) ? arg "-Wno-error=inline" ]

packageDatabaseArgs :: Args
packageDatabaseArgs = do
    stage <- getStage
    dbPath <- expr (packageDbPath stage)
    expr (need [dbPath -/- packageDbStamp])
    prefix <- ifM (builder Ghc) (return "-package-db ") (return "--package-db=")
    arg $ prefix ++ dbPath

bootPackageDatabaseArgs :: Args
bootPackageDatabaseArgs = do
    stage  <- getStage
    dbPath <- expr $ packageDbPath stage
    expr $ need [dbPath -/- packageDbStamp]
    stage0 ? packageDatabaseArgs
