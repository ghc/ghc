{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Oracles.WindowsPath (
    fixAbsolutePathOnWindows, topDirectory, windowsPathOracle
    ) where

import Data.Char

import Base
import Oracles.Config.Setting

newtype WindowsPath = WindowsPath FilePath
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

topDirectory :: Action FilePath
topDirectory = do
    ghcSourcePath <- setting GhcSourcePath
    fixAbsolutePathOnWindows ghcSourcePath

-- | Fix an absolute path on Windows:
-- * "/c/" => "C:/"
-- * "/usr/bin/tar.exe" => "C:/msys/usr/bin/tar.exe"
fixAbsolutePathOnWindows :: FilePath -> Action FilePath
fixAbsolutePathOnWindows path = do
    windows <- windowsHost
    if windows
    then do
        let (dir, file) = splitFileName path
        winDir <- askOracle $ WindowsPath dir
        return $ winDir -/- file
    else
        return path

-- | Compute path mapping on Windows. This is slow and requires caching.
windowsPathOracle :: Rules ()
windowsPathOracle = void $
    addOracle $ \(WindowsPath path) -> do
        Stdout out <- quietly $ cmd ["cygpath", "-m", path]
        let windowsPath = unifyPath $ dropWhileEnd isSpace out
        putLoud $ "Windows path mapping: " ++ path ++ " => " ++ windowsPath
        return windowsPath
