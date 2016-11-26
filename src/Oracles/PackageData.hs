{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Oracles.PackageData (
    PackageData (..), PackageDataList (..), pkgData, pkgDataList, packageDataOracle
    ) where

import Development.Shake.Config
import qualified Data.HashMap.Strict as Map

import Base

data PackageData = BuildGhciLib FilePath
                 | ComponentId  FilePath
                 | Synopsis     FilePath
                 | Version      FilePath

data PackageDataList = AsmSrcs        FilePath
                     | CcArgs         FilePath
                     | CSrcs          FilePath
                     | CmmSrcs        FilePath
                     | CppArgs        FilePath
                     | DepCcArgs      FilePath
                     | DepExtraLibs   FilePath
                     | DepIds         FilePath
                     | DepIncludeDirs FilePath
                     | DepLdArgs      FilePath
                     | DepLibDirs     FilePath
                     | DepNames       FilePath
                     | Deps           FilePath
                     | HiddenModules  FilePath
                     | HsArgs         FilePath
                     | IncludeDirs    FilePath
                     | LdArgs         FilePath
                     | Modules        FilePath
                     | SrcDirs        FilePath

newtype PackageDataKey = PackageDataKey (FilePath, String)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

askPackageData :: FilePath -> String -> Action String
askPackageData path key = fromMaybe "" <$>
    askOracle (PackageDataKey (path -/- "package-data.mk", key))

-- | For each @PackageData path@ the file 'path/package-data.mk' contains a line
-- of the form 'path_VERSION = 1.2.3.4'. @pkgData (PackageData path)@ is an
-- Action that consults the file and returns "1.2.3.4".
pkgData :: PackageData -> Action String
pkgData packageData = case packageData of
    BuildGhciLib path -> askPackageData path "BUILD_GHCI_LIB"
    ComponentId  path -> askPackageData path "COMPONENT_ID"
    Synopsis     path -> askPackageData path "SYNOPSIS"
    Version      path -> askPackageData path "VERSION"

-- | @PackageDataList path@ is used for multiple string options separated by
-- spaces, such as @path_MODULES = Data.Array Data.Array.Base ...@.
-- @pkgListData Modules@ therefore returns ["Data.Array", "Data.Array.Base", ...]
pkgDataList :: PackageDataList -> Action [String]
pkgDataList packageData = fmap (map unquote . words) $ case packageData of
    AsmSrcs        path -> askPackageData path "S_SRCS"
    CcArgs         path -> askPackageData path "CC_OPTS"
    CSrcs          path -> askPackageData path "C_SRCS"
    CmmSrcs        path -> askPackageData path "CMM_SRCS"
    CppArgs        path -> askPackageData path "CPP_OPTS"
    DepCcArgs      path -> askPackageData path "DEP_CC_OPTS"
    DepExtraLibs   path -> askPackageData path "DEP_EXTRA_LIBS"
    DepIds         path -> askPackageData path "DEP_IPIDS"
    DepIncludeDirs path -> askPackageData path "DEP_INCLUDE_DIRS_SINGLE_QUOTED"
    DepLibDirs     path -> askPackageData path "DEP_LIB_DIRS_SINGLE_QUOTED"
    DepLdArgs      path -> askPackageData path "DEP_LD_OPTS"
    DepNames       path -> askPackageData path "DEP_NAMES"
    Deps           path -> askPackageData path "DEPS"
    HiddenModules  path -> askPackageData path "HIDDEN_MODULES"
    HsArgs         path -> askPackageData path "HC_OPTS"
    IncludeDirs    path -> askPackageData path "INCLUDE_DIRS"
    LdArgs         path -> askPackageData path "LD_OPTS"
    Modules        path -> askPackageData path "MODULES"
    SrcDirs        path -> askPackageData path "HS_SRC_DIRS"
  where
    unquote = dropWhile (== '\'') . dropWhileEnd (== '\'')

-- | Oracle for 'package-data.mk' files.
packageDataOracle :: Rules ()
packageDataOracle = void $ do
    keys <- newCache $ \file -> do
        need [file]
        putLoud $ "Reading " ++ file ++ "..."
        liftIO $ readConfigFile file
    addOracle $ \(PackageDataKey (file, key)) -> Map.lookup key <$> keys file
