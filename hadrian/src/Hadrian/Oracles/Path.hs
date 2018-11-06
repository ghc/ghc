{-# LANGUAGE TypeFamilies #-}
module Hadrian.Oracles.Path (
    lookupInPath, bashPath, fixAbsolutePathOnWindows, pathOracle
    ) where

import Control.Monad
import Data.Maybe
import Data.Char
import Data.List.Extra
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import System.Directory
import System.Info.Extra

import Hadrian.Utilities

-- | Lookup a specified 'FilePath' in the system @PATH@.
lookupInPath :: FilePath -> Action FilePath
lookupInPath name
    | name == takeFileName name = askOracle $ LookupInPath name
    | otherwise                 = return name

-- | Lookup the path to the @bash@ interpreter.
bashPath :: Action FilePath
bashPath = lookupInPath "bash"

-- | Fix an absolute path on Windows:
-- * "/c/" => "C:/"
-- * "/usr/bin/tar.exe" => "C:/msys/usr/bin/tar.exe"
fixAbsolutePathOnWindows :: FilePath -> Action FilePath
fixAbsolutePathOnWindows path =
    if isWindows
    then do
        let (dir, file) = splitFileName path
        winDir <- askOracle $ WindowsPath dir
        return $ winDir -/- file
    else
        return path

newtype LookupInPath = LookupInPath String
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult LookupInPath = String

newtype WindowsPath = WindowsPath FilePath
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult WindowsPath = String

-- | Oracles for looking up paths. These are slow and require caching.
pathOracle :: Rules ()
pathOracle = do
    void $ addOracleCache $ \(WindowsPath path) -> do
        Stdout out <- quietly $ cmd ["cygpath", "-m", path]
        let windowsPath = unifyPath $ dropWhileEnd isSpace out
        putLoud $ "| Windows path mapping: " ++ path ++ " => " ++ windowsPath
        return windowsPath

    void $ addOracleCache $ \(LookupInPath name) -> do
        let unpack = fromMaybe . error $ "Cannot find executable " ++ quote name
        path <- unifyPath . unpack <$> liftIO (findExecutable name)
        putLoud $ "| Executable found: " ++ name ++ " => " ++ path
        return path
