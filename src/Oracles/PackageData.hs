{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.PackageData (
    PackageData (..), PackageDataList (..),
    pkgData, pkgDataList, packageDataOracle
    ) where

import Base
import Util
import Oracles.Base
import Data.List
import Data.Maybe
import Control.Applicative
import qualified Data.HashMap.Strict as Map

-- For each (PackageData path) the file 'path/package-data.mk' contains
-- a line of the form 'path_VERSION = 1.2.3.4'.
-- pkgData $ PackageData path is an action that consults the file and
-- returns "1.2.3.4".
--
-- PackageDataList is used for multiple string options separated by spaces,
-- such as 'path_MODULES = Data.Array Data.Array.Base ...'.
-- pkgListData Modules therefore returns ["Data.Array", "Data.Array.Base", ...]
data PackageData = Version     FilePath
                 | PackageKey  FilePath
                 | Synopsis    FilePath

data PackageDataList = Modules        FilePath
                     | SrcDirs        FilePath
                     | IncludeDirs    FilePath
                     | Deps           FilePath
                     | DepKeys        FilePath
                     | DepNames       FilePath
                     | CppArgs        FilePath
                     | HsArgs         FilePath
                     | CcArgs         FilePath
                     | CSrcs          FilePath
                     | DepIncludeDirs FilePath

newtype PackageDataKey = PackageDataKey (FilePath, String)
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- TODO: is this needed?
askPackageData :: FilePath -> String -> Action String
askPackageData path key = do
    let fullKey = replaceSeparators '_' $ path ++ "_" ++ key
        pkgData = path -/- "package-data.mk"
    value <- askOracle $ PackageDataKey (pkgData, fullKey)
    return $ fromMaybe
        (error $ "No key '" ++ key ++ "' in " ++ pkgData ++ ".") value

pkgData :: PackageData -> Action String
pkgData packageData = do
    let (key, path) = case packageData of
           Version     path -> ("VERSION"     , path)
           PackageKey  path -> ("PACKAGE_KEY" , path)
           Synopsis    path -> ("SYNOPSIS"    , path)
        fullKey = replaceSeparators '_' $ path ++ "_" ++ key
        pkgData = path -/- "package-data.mk"
    res <- askOracle $ PackageDataKey (pkgData, fullKey)
    return $ fromMaybe
        (error $ "No key '" ++ key ++ "' in " ++ pkgData ++ ".") res

pkgDataList :: PackageDataList -> Action [String]
pkgDataList packageData = do
    let (key, path, defaultValue) = case packageData of
           Modules        path -> ("MODULES"                       , path, "" )
           SrcDirs        path -> ("HS_SRC_DIRS"                   , path, ".")
           IncludeDirs    path -> ("INCLUDE_DIRS"                  , path, ".")
           Deps           path -> ("DEPS"                          , path, "" )
           DepKeys        path -> ("DEP_KEYS"                      , path, "" )
           DepNames       path -> ("DEP_NAMES"                     , path, "" )
           CppArgs        path -> ("CPP_OPTS"                      , path, "" )
           HsArgs         path -> ("HC_OPTS"                       , path, "" )
           CcArgs         path -> ("CC_OPTS"                       , path, "" )
           CSrcs          path -> ("C_SRCS"                        , path, "" )
           DepIncludeDirs path -> ("DEP_INCLUDE_DIRS_SINGLE_QUOTED", path, "" )
        fullKey = replaceSeparators '_' $ path ++ "_" ++ key
        pkgData = path -/- "package-data.mk"
        unquote = dropWhile (== '\'') . dropWhileEnd (== '\'')
    res <- askOracle $ PackageDataKey (pkgData, fullKey)
    return $ map unquote $ words $ case res of
        Nothing    -> error $ "No key '" ++ key ++ "' in " ++ pkgData ++ "."
        Just ""    -> defaultValue
        Just value -> value

-- Oracle for 'package-data.mk' files
packageDataOracle :: Rules ()
packageDataOracle = do
    pkgData <- newCache $ \file -> do
        need [file]
        putOracle $ "Reading " ++ file ++ "..."
        liftIO $ readConfigFile file
    addOracle $ \(PackageDataKey (file, key)) ->
        Map.lookup key <$> pkgData (unifyPath file)
    return ()
