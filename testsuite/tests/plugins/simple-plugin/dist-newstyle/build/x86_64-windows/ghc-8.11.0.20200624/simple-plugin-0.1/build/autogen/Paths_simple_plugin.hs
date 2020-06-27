{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_simple_plugin (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Andi\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Andi\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.11.0.20200624\\simple-plugin-0.1-inplace"
dynlibdir  = "C:\\Users\\Andi\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.11.0.20200624"
datadir    = "C:\\Users\\Andi\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.11.0.20200624\\simple-plugin-0.1"
libexecdir = "C:\\Users\\Andi\\AppData\\Roaming\\cabal\\simple-plugin-0.1-inplace\\x86_64-windows-ghc-8.11.0.20200624\\simple-plugin-0.1"
sysconfdir = "C:\\Users\\Andi\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "simple_plugin_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "simple_plugin_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "simple_plugin_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "simple_plugin_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "simple_plugin_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "simple_plugin_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
