{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.PackageData (
    PackageDataPair (..),
    PackageData (..)
    ) where

import Development.Shake.Classes
import Base
import Util

newtype PackageDataPair = PackageDataPair (FilePath, String)
                        deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

packagaDataWithDefault :: FilePath -> String -> Action String -> Action String
packagaDataWithDefault file key defaultAction = do
    maybeValue <- askOracle $ PackageDataPair (file, key) 
    case maybeValue of
        Just value -> return value
        Nothing    -> defaultAction

data PackageData = Modules FilePath | SrcDirs FilePath | PackageKey FilePath 
                 | IncludeDirs FilePath | Deps FilePath | DepKeys FilePath
                 deriving Show

instance ShowAction PackageData where
    showAction key = do
        let (keyName, file, ifEmpty) = case key of
               Modules     file -> ("MODULES"     , file, "" )
               SrcDirs     file -> ("HS_SRC_DIRS" , file, ".")
               PackageKey  file -> ("PACKAGE_KEY" , file, "" )
               IncludeDirs file -> ("INCLUDE_DIRS", file, ".")
               Deps        file -> ("DEPS"        , file, "" )
               DepKeys     file -> ("DEP_KEYS"    , file, "" )
            keyFullName = replaceSeparators '_' $ takeDirectory file ++ "_" ++ keyName
        res <- packagaDataWithDefault file keyFullName $
            error $ "\nCannot find key '" ++ keyName ++ "' in " ++ file ++ "."
        return $ words $ if res == "" then ifEmpty else res
