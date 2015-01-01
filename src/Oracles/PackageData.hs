{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.PackageData (
    PackageDataPair (..),
    packagaDataOption, PackageDataKey (..)
    ) where

import Development.Shake.Classes
import Base
import Util

newtype PackageDataPair = PackageDataPair (FilePath, String)
                        deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

packagaDataOptionWithDefault :: FilePath -> String -> Action String -> Action String
packagaDataOptionWithDefault file key defaultAction = do
    maybeValue <- askOracle $ PackageDataPair (file, key) 
    case maybeValue of
        Just value -> return value
        Nothing    -> defaultAction

data PackageDataKey = Modules | SrcDirs | PackageKey | IncludeDirs | Deps | DepKeys
                    deriving Show

packagaDataOption :: FilePath -> PackageDataKey -> Action String
packagaDataOption file key = do
    let (keyName, ifEmpty) = case key of
           Modules     -> ("MODULES"     , "" )
           SrcDirs     -> ("HS_SRC_DIRS" , ".")
           PackageKey  -> ("PACKAGE_KEY" , "" )
           IncludeDirs -> ("INCLUDE_DIRS", ".")
           Deps        -> ("DEPS"        , "" )
           DepKeys     -> ("DEP_KEYS"    , "" )
        keyFullName = replaceSeparators '_' $ takeDirectory file ++ "_" ++ keyName
    res <- packagaDataOptionWithDefault file keyFullName $
        error $ "\nCannot find key '" ++ keyName ++ "' in " ++ file ++ "."
    return $ if res == "" then ifEmpty else res
