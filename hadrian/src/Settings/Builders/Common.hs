module Settings.Builders.Common (
    module Base,
    module Expression,
    module Oracles.Flag,
    module Oracles.Setting,
    module Settings,
    module UserSettings,
    cIncludeArgs, ldArgs, cArgs, cppArgs, cWarnings,
    packageDatabaseArgs, bootPackageDatabaseArgs
    ) where

import Hadrian.Haskell.Cabal.Type

import Base
import Expression
import Oracles.Flag
import Oracles.Setting
import Settings
import UserSettings

cIncludeArgs :: Args
cIncludeArgs = do
    pkg     <- getPackage
    path    <- getBuildPath
    incDirs <- getContextData includeDirs
    depDirs <- getContextData depIncludeDirs
    mconcat [ notStage0 ? arg "-Irts/include"
            , arg $ "-I" ++ path
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

cppArgs :: Args
cppArgs = mempty

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
