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

data PackageData = Modules FilePath | SrcDirs FilePath | PackageKey FilePath 
                 | IncludeDirs FilePath | Deps FilePath | DepKeys FilePath

instance ShowArgs PackageData where
    showArgs key = do
        let (keyName, file, ifEmpty) = case key of
               Modules     file -> ("MODULES"     , file, "" )
               SrcDirs     file -> ("HS_SRC_DIRS" , file, ".")
               PackageKey  file -> ("PACKAGE_KEY" , file, "" )
               IncludeDirs file -> ("INCLUDE_DIRS", file, ".")
               Deps        file -> ("DEPS"        , file, "" )
               DepKeys     file -> ("DEP_KEYS"    , file, "" )
            keyFullName = replaceSeparators '_' $ takeDirectory file ++ "_" ++ keyName
        res <- askOracle $ PackageDataKey (file, keyFullName)
        return $ words $ case res of
            Nothing    -> error $ "\nCannot find key '" ++ keyName ++ "' in " ++ file ++ "."
            Just ""    -> ifEmpty
            Just value -> value
