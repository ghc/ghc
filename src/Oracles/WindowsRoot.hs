{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Oracles.WindowsRoot (
    windowsRoot, fixAbsolutePathOnWindows, lookupInPath, topDirectory, windowsRootOracle
    ) where

import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Base
import Oracles.Config.Setting

newtype WindowsRoot = WindowsRoot ()
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- Looks up cygwin/msys root on Windows
windowsRoot :: Action String
windowsRoot = askOracle $ WindowsRoot ()

topDirectory :: Action FilePath
topDirectory = do
    ghcSourcePath <- setting GhcSourcePath
    fixAbsolutePathOnWindows ghcSourcePath

-- TODO: this is fragile, e.g. we currently only handle C: drive
-- On Windows:
-- * if the path starts with "/c/" change the prefix to "C:/"
-- * otherwise, if the path starts with "/", prepend it with the correct path
-- to the root, e.g: "/usr/local/bin/ghc.exe" => "C:/msys/usr/local/bin/ghc.exe"
fixAbsolutePathOnWindows :: FilePath -> Action FilePath
fixAbsolutePathOnWindows path = do
    windows <- windowsHost
    -- Note, below is different from FilePath.isAbsolute:
    if (windows && "/" `isPrefixOf` path)
    then do
        if ("/c/" `isPrefixOf` path)
        then return $ "C:" ++ drop 2 path
        else do
            root <- windowsRoot
            return . unifyPath $ root ++ drop 1 path
    else
        return path

-- | Lookup a @command@ in @PATH@ environment.
lookupInPath :: FilePath -> Action FilePath
lookupInPath c
    | c /= takeFileName c = return c
    | otherwise = do
        envPaths <- splitOn ":" <$> getEnvWithDefault "" "PATH"
        let candidates = map (-/- c) envPaths in
        mapM_ putStrLn candidates
        -- this will crash if we do not find any valid candidate.
        head <$> filterM doesFileExist candidates

-- Oracle for windowsRoot. This operation requires caching as looking up
-- the root is slow (at least the current implementation).
windowsRootOracle :: Rules ()
windowsRootOracle = do
    root <- newCache $ \_ -> do
        Stdout out <- quietly $ cmd ["cygpath", "-m", "/"]
        let root = dropWhileEnd isSpace out
        putOracle $ "Detected root on Windows: " ++ root
        return root
    _ <- addOracle $ \WindowsRoot{} -> root ()
    return ()
