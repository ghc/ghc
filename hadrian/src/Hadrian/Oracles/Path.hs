{-# LANGUAGE TypeFamilies #-}
module Hadrian.Oracles.Path (
    lookupInPath, pathOracle
    ) where

import Control.Monad
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import System.Directory

import Hadrian.Utilities

-- | Lookup a specified 'FilePath' in the system @PATH@.
lookupInPath :: FilePath -> Action FilePath
lookupInPath name
    | name == takeFileName name = askOracle $ LookupInPath name
    | otherwise                 = return name

newtype LookupInPath = LookupInPath String
    deriving (Binary, Eq, Hashable, NFData, Show)
type instance RuleResult LookupInPath = String

-- | Oracles for looking up paths. These are slow and require caching.
pathOracle :: Rules ()
pathOracle = do
    void $ addOracleCache $ \(LookupInPath name) -> do
        path <- liftIO getSearchPath
        exes <- liftIO (findExecutablesInDirectories path name)
        exe <- case exes of
          []      -> error $ "Cannot find executable " ++ quote name
          (exe:_) -> pure $ unifyPath exe
        putVerbose $ "| Executable found: " ++ name ++ " => " ++ exe
        return exe
