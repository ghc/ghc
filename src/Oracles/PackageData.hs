{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.PackageData (
    PackageDataKey (..),
    PackageData (..)
    ) where

import Development.Shake.Classes
import Base
import Util

newtype PackageDataKey = PackageDataKey (FilePath, String)
                        deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

data PackageData = Version     FilePath
                 | Modules     FilePath
                 | SrcDirs     FilePath
                 | PackageKey  FilePath
                 | IncludeDirs FilePath
                 | Deps        FilePath
                 | DepKeys     FilePath
                 | DepNames    FilePath
                 | Synopsis    FilePath
                 | CppOpts     FilePath
                 | HsOpts      FilePath

instance ShowArgs PackageData where
    showArgs packageData = do
        let (key, file, defaultValue) = case packageData of
               Version     file -> ("VERSION"     , file, "" )
               Modules     file -> ("MODULES"     , file, "" )
               SrcDirs     file -> ("HS_SRC_DIRS" , file, ".")
               PackageKey  file -> ("PACKAGE_KEY" , file, "" )
               IncludeDirs file -> ("INCLUDE_DIRS", file, ".")
               Deps        file -> ("DEPS"        , file, "" )
               DepKeys     file -> ("DEP_KEYS"    , file, "" )
               DepNames    file -> ("DEP_NAMES"   , file, "" )
               Synopsis    file -> ("SYNOPSIS"    , file, "" )
               CppOpts     file -> ("CPP_OPTS"    , file, "" )
               HsOpts      file -> ("HC_OPTS"     , file, "" )
            fullKey = replaceSeparators '_' $ file ++ "_" ++ key
            pkgData = file </> "package-data.mk"
        res <- askOracle $ PackageDataKey (pkgData, fullKey)
        return $ words $ case res of
            Nothing    -> error $ "No key '" ++ key ++ "' in "
                                ++ toStandard pkgData ++ "."
            Just ""    -> defaultValue
            Just value -> value
