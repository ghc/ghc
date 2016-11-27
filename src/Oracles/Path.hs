{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Oracles.Path (
    topDirectory, getTopDirectory, systemBuilderPath, pathOracle
    ) where

import Control.Monad.Trans.Reader
import Data.Char
import System.Directory

import Base
import Builder
import Oracles.Config
import Oracles.Config.Setting
import Stage

-- | Path to the GHC source tree.
topDirectory :: Action FilePath
topDirectory = fixAbsolutePathOnWindows =<< setting GhcSourcePath

getTopDirectory :: ReaderT a Action FilePath
getTopDirectory = lift topDirectory

-- | Determine the location of a system 'Builder'.
systemBuilderPath :: Builder -> Action FilePath
systemBuilderPath builder = case builder of
    Alex            -> fromKey "alex"
    Ar              -> fromKey "ar"
    Cc  _  Stage0   -> fromKey "system-cc"
    Cc  _  _        -> fromKey "cc"
    -- We can't ask configure for the path to configure!
    Configure _     -> return "bash configure"
    Ghc _  Stage0   -> fromKey "system-ghc"
    GhcPkg _ Stage0 -> fromKey "system-ghc-pkg"
    Happy           -> fromKey "happy"
    HsColour        -> fromKey "hscolour"
    HsCpp           -> fromKey "hs-cpp"
    Ld              -> fromKey "ld"
    Make _          -> fromKey "make"
    Nm              -> fromKey "nm"
    Objdump         -> fromKey "objdump"
    Patch           -> fromKey "patch"
    Perl            -> fromKey "perl"
    Ranlib          -> fromKey "ranlib"
    Tar             -> fromKey "tar"
    _               -> error $ "No system.config entry for " ++ show builder
  where
    fromKey key = do
        let unpack = fromMaybe . error $ "Cannot find path to builder "
                ++ quote key ++ " in system.config file. Did you skip configure?"
        path <- unpack <$> askConfig key
        if null path
        then do
            unless (isOptional builder) . error $ "Non optional builder "
                ++ quote key ++ " is not specified in system.config file."
            return "" -- TODO: Use a safe interface.
        else fixAbsolutePathOnWindows =<< lookupInPath path

-- | Lookup an executable in @PATH@.
lookupInPath :: FilePath -> Action FilePath
lookupInPath name
    | name == takeFileName name = askOracle $ LookupInPath name
    | otherwise                 = return name

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

newtype LookupInPath = LookupInPath String
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

newtype WindowsPath = WindowsPath FilePath
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

-- | Oracles for looking up paths. These are slow and require caching.
pathOracle :: Rules ()
pathOracle = do
    void $ addOracle $ \(WindowsPath path) -> do
        Stdout out <- quietly $ cmd ["cygpath", "-m", path]
        let windowsPath = unifyPath $ dropWhileEnd isSpace out
        putLoud $ "Windows path mapping: " ++ path ++ " => " ++ windowsPath
        return windowsPath

    void $ addOracle $ \(LookupInPath name) -> do
        let unpack = fromMaybe . error $ "Cannot find executable " ++ quote name
        path <- unifyPath <$> unpack <$> liftIO (findExecutable name)
        putLoud $ "Executable found: " ++ name ++ " => " ++ path
        return path

