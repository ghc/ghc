{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.Dependencies (
    dependencies,
    dependenciesOracle
    ) where

import Base
import Util
import qualified Data.HashMap.Strict as Map

newtype DependenciesKey = DependenciesKey (FilePath, FilePath)
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- dependencies path obj is an action that looks up dependencies of an object
-- file in a generated dependecy file 'path/.dependencies'.
-- If the dependencies cannot be determined, an appropriate error is raised.
-- Otherwise, a pair (source, depFiles) is returned, such that obj can be
-- produced by compiling 'source'; the latter can also depend on a number of
-- other dependencies listed in depFiles.
dependencies :: FilePath -> FilePath -> Action (FilePath, [FilePath])
dependencies path obj = do
    let depFile = path -/- ".dependencies"
    res1 <- askOracle $ DependenciesKey (depFile, obj)
    -- if no dependencies found attempt to drop the way prefix (for *.c sources)
    res2 <- case res1 of
        Nothing -> askOracle $ DependenciesKey (depFile, obj -<.> "o")
        _       -> return res1
    case res2 of
        Nothing -> putError $ "No dependencies found for '" ++ obj ++ "'."
        Just [] -> putError $ "Empty dependency list for '" ++ obj ++ "'."
        Just (src:depFiles) -> return (src, depFiles)

-- Oracle for 'path/dist/.dependencies' files
dependenciesOracle :: Rules ()
dependenciesOracle = do
    deps <- newCache $ \file -> do
        putOracle $ "Reading dependencies from " ++ file ++ "..."
        contents <- parseMakefile <$> readFile' file
        return . Map.fromList . map (bimap unifyPath (map unifyPath))
                              . map (bimap head concat . unzip)
                              . groupBy ((==) `on` fst)
                              . sortBy (compare `on` fst) $ contents

    _ <- addOracle $ \(DependenciesKey (file, obj)) -> Map.lookup obj <$> deps file
    return ()
