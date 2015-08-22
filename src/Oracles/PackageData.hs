{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.PackageData (
    PackageData (..), PackageDataList (..),
    pkgData, pkgDataList, packageDataOracle
    ) where

import Base
import Util
import qualified Data.HashMap.Strict as Map

-- For each (PackageData path) the file 'path/package-data.mk' contains
-- a line of the form 'path_VERSION = 1.2.3.4'.
-- pkgData $ PackageData path is an action that consults the file and
-- returns "1.2.3.4".
--
-- PackageDataList is used for multiple string options separated by spaces,
-- such as 'path_MODULES = Data.Array Data.Array.Base ...'.
-- pkgListData Modules therefore returns ["Data.Array", "Data.Array.Base", ...]
data PackageData = Version      FilePath
                 | PackageKey   FilePath
                 | LibName      FilePath
                 | Synopsis     FilePath
                 | BuildGhciLib FilePath

data PackageDataList = Modules        FilePath
                     | HiddenModules  FilePath
                     | SrcDirs        FilePath
                     | IncludeDirs    FilePath
                     | Deps           FilePath
                     | DepIds         FilePath
                     | DepNames       FilePath
                     | CppArgs        FilePath
                     | HsArgs         FilePath
                     | CcArgs         FilePath
                     | CSrcs          FilePath
                     | DepIncludeDirs FilePath

newtype PackageDataKey = PackageDataKey (FilePath, String)
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

askPackageData :: FilePath -> String -> Action String
askPackageData path key = do
    let fullKey = replaceSeparators '_' $ path ++ "_" ++ key
        file    = path -/- "package-data.mk"
    maybeValue <- askOracle $ PackageDataKey (file, fullKey)
    case maybeValue of
        Nothing    -> putError $ "No key '" ++ key ++ "' in " ++ file ++ "."
        Just value -> return value

pkgData :: PackageData -> Action String
pkgData packageData = case packageData of
    Version      path -> askPackageData path "VERSION"
    PackageKey   path -> askPackageData path "PACKAGE_KEY"
    LibName      path -> askPackageData path "LIB_NAME"
    Synopsis     path -> askPackageData path "SYNOPSIS"
    BuildGhciLib path -> askPackageData path "BUILD_GHCI_LIB"

pkgDataList :: PackageDataList -> Action [String]
pkgDataList packageData = fmap (map unquote . words) $ case packageData of
    Modules        path -> askPackageData path "MODULES"
    HiddenModules  path -> askPackageData path "HIDDEN_MODULES"
    SrcDirs        path -> askPackageData path "HS_SRC_DIRS"
    IncludeDirs    path -> askPackageData path "INCLUDE_DIRS"
    Deps           path -> askPackageData path "DEPS"
    DepIds         path -> askPackageData path "DEP_IPIDS"
    DepNames       path -> askPackageData path "DEP_NAMES"
    CppArgs        path -> askPackageData path "CPP_OPTS"
    HsArgs         path -> askPackageData path "HC_OPTS"
    CcArgs         path -> askPackageData path "CC_OPTS"
    CSrcs          path -> askPackageData path "C_SRCS"
    DepIncludeDirs path -> askPackageData path "DEP_INCLUDE_DIRS_SINGLE_QUOTED"
  where
    unquote = dropWhile (== '\'') . dropWhileEnd (== '\'')

-- Oracle for 'package-data.mk' files
packageDataOracle :: Rules ()
packageDataOracle = do
    pkgDataContents <- newCache $ \file -> do
        need [file]
        putOracle $ "Reading " ++ file ++ "..."
        liftIO $ readConfigFile file
    _ <- addOracle $ \(PackageDataKey (file, key)) ->
        Map.lookup key <$> pkgDataContents file
    return ()
