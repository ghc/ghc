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

data PackageData = Modules     FilePath
                 | SrcDirs     FilePath
                 | PackageKey  FilePath
                 | IncludeDirs FilePath
                 | Deps        FilePath
                 | DepKeys     FilePath
                 | Synopsis    FilePath

instance ShowArgs PackageData where
    showArgs packageData = do
        let (key, file, defaultValue) = case packageData of
               Modules     file -> ("MODULES"     , file, "" )
               SrcDirs     file -> ("HS_SRC_DIRS" , file, ".")
               PackageKey  file -> ("PACKAGE_KEY" , file, "" )
               IncludeDirs file -> ("INCLUDE_DIRS", file, ".")
               Deps        file -> ("DEPS"        , file, "" )
               DepKeys     file -> ("DEP_KEYS"    , file, "" )
               Synopsis    file -> ("SYNOPSIS"    , file, "" )
            fullKey = replaceSeparators '_' $ takeDirectory file ++ "_" ++ key
        res <- askOracle $ PackageDataKey (file, fullKey)
        return $ words $ case res of
            Nothing    -> error $ "No key '" ++ key ++ "' in " ++ file ++ "."
            Just ""    -> defaultValue
            Just value -> value
