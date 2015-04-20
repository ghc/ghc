{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.PackageData (
    PackageData (..), MultiPackageData (..),
    PackageDataKey (..), askPackageData
    ) where

import Development.Shake.Classes
import Base
import Util
import Data.Maybe

-- For each (PackageData path) the file 'path/package-data.mk' contains
-- a line of the form 'path_VERSION = 1.2.3.4'.
-- (showArg $ PackageData path) is an action that consults the file and
-- returns "1.2.3.4".
--
-- MultiPackageData is used for multiple string options separated by spaces,
-- such as 'path_MODULES = Data.Array Data.Array.Base ...'.
-- (showArgs Modules) therefore returns ["Data.Array", "Data.Array.Base", ...].

data PackageData = Version     FilePath
                 | PackageKey  FilePath
                 | Synopsis    FilePath

data MultiPackageData = Modules        FilePath
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

askPackageData :: FilePath -> String -> Action String
askPackageData path key = do
    let fullKey = replaceSeparators '_' $ path ++ "_" ++ key
        pkgData = path </> "package-data.mk"
    value <- askOracle $ PackageDataKey (pkgData, fullKey)
    return $ fromMaybe
        (error $ "No key '" ++ key ++ "' in " ++ pkgData ++ ".") value

-- TODO: remove
instance ShowArg PackageData where
    showArg packageData = do
        let (key, path) = case packageData of
               Version     path -> ("VERSION"     , path)
               PackageKey  path -> ("PACKAGE_KEY" , path)
               Synopsis    path -> ("SYNOPSIS"    , path)
            fullKey = replaceSeparators '_' $ path ++ "_" ++ key
            pkgData = path </> "package-data.mk"
        res <- askOracle $ PackageDataKey (pkgData, fullKey)
        return $ fromMaybe
            (error $ "No key '" ++ key ++ "' in " ++ unifyPath pkgData ++ ".")
            res

instance ShowArgs MultiPackageData where
    showArgs packageData = do
        let (key, path, defaultValue) = case packageData of
               Modules        path -> ("MODULES"         , path, "" )
               SrcDirs        path -> ("HS_SRC_DIRS"     , path, ".")
               IncludeDirs    path -> ("INCLUDE_DIRS"    , path, ".")
               Deps           path -> ("DEPS"            , path, "" )
               DepKeys        path -> ("DEP_KEYS"        , path, "" )
               DepNames       path -> ("DEP_NAMES"       , path, "" )
               CppArgs        path -> ("CPP_OPTS"        , path, "" )
               HsArgs         path -> ("HC_OPTS"         , path, "" )
               CcArgs         path -> ("CC_OPTS"         , path, "" )
               CSrcs          path -> ("C_SRCS"          , path, "" )
               DepIncludeDirs path -> ("DEP_INCLUDE_DIRS_SINGLE_QUOTED"
                                      , path, "")
            fullKey = replaceSeparators '_' $ path ++ "_" ++ key
            pkgData = path </> "package-data.mk"
            unquote = dropWhile (== '\'') . dropWhileEnd (== '\'')
        res <- askOracle $ PackageDataKey (pkgData, fullKey)
        return $ map unquote $ words $ case res of
            Nothing    -> error $ "No key '" ++ key ++ "' in "
                                ++ unifyPath pkgData ++ "."
            Just ""    -> defaultValue
            Just value -> value
