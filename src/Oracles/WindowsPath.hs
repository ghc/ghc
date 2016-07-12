{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Oracles.WindowsPath (
    fixAbsolutePathOnWindows, topDirectory, getTopDirectory, windowsPathOracle
    ) where

-- TODO: Rename to Oracles.Path.

import Control.Monad.Trans.Reader
import Data.Char

import Base
import Oracles.Config.Setting

newtype WindowsPath = WindowsPath FilePath
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

-- | Path to the GHC source tree.
topDirectory :: Action FilePath
topDirectory = fixAbsolutePathOnWindows =<< setting GhcSourcePath

getTopDirectory :: ReaderT a Action FilePath
getTopDirectory = lift topDirectory

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
