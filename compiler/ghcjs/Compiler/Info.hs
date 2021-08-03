{-# LANGUAGE CPP, ScopedTypeVariables, OverloadedStrings #-}
module Compiler.Info where

import qualified Control.Exception as E

import           Data.Function      (on)
import           Data.List          (nubBy)
import qualified Data.Version as Version

import           System.Directory   (getAppUserDataDirectory)
import           System.FilePath    ((</>))
import           System.Info
import Prelude

import           Config             (cProjectVersion)
import           DynFlags

-- import qualified Paths_ghcjs

compilerInfo :: DynFlags
             -> [(String, String)]
compilerInfo dflags = do
      let topDir = getTopDir dflags
      nubBy ((==) `on` fst) $
           [ ("Project name"
           , "The Glorious Glasgow Haskell Compilation System for JavaScript")
           , ("Global Package DB", getGlobalPackageDB topDir)
           , ("Project version"  , getCompilerVersion)
           , ("LibDir"           , topDir)
           ] ++ DynFlags.compilerInfo dflags

getTopDir :: DynFlags -> FilePath
getTopDir = sTopDir . settings

-- | get the library directory (ghcjs --print-libdir).
getLibDir :: DynFlags -> FilePath
getLibDir = sTopDir . settings

{- | get the library directory from the unsafe global DynFlags
     throws an exception if called before a Ghc session has been started
 -}
unsafeGetLibDir :: FilePath
unsafeGetLibDir = getLibDir unsafeGlobalDynFlags

-- | find location of the global package database
getGlobalPackageDB :: FilePath
                   -> FilePath
getGlobalPackageDB libDir = libDir </> "package.conf.d"

getUserTopDir :: IO (Maybe FilePath)
getUserTopDir = fmap Just getUserTopDir' `E.catch`
                   \(E.SomeException _) -> return Nothing

getUserTopDir' :: IO FilePath
getUserTopDir' =  (</> subdir) <$> getAppUserDataDirectory "ghcjs"
  where
    targetARCH = arch
    targetOS   = os
    subdir     = targetARCH ++ '-':targetOS ++ '-':getFullCompilerVersion

-- | find location of the user package database
getUserPackageDir :: IO (Maybe FilePath)
getUserPackageDir = getUserTopDir

getUserPackageDir' :: IO FilePath
getUserPackageDir' = getUserTopDir'

getUserCacheDir :: IO (Maybe FilePath)
getUserCacheDir = fmap (</> "cache") <$> getUserTopDir

-- | Just the GHC version
getGhcCompilerVersion :: String
getGhcCompilerVersion = cProjectVersion

-- | GHCJS-GHC
getFullCompilerVersion :: [Char]
getFullCompilerVersion = getGhcCompilerVersion ++
                         "-" ++
                         getGhcCompilerVersion

-- | Just the GHCJS version
getCompilerVersion :: String
getCompilerVersion = getGhcCompilerVersion

-- | version in GHC format, e.g. 7.8.2 -> 708
getShortCompilerVersion :: String
getShortCompilerVersion = "810" -- XXX which constant is this?
{-
  case Version.versionBranch Paths_ghcjs.version of
    []      -> "0"
    [x]     -> show (100 * x)
    (x:y:_) -> show (100 * x + min 99 y)
-}

getCompilerSubdir :: [Char]
getCompilerSubdir = "ghcjs-" ++ getCompilerVersion

-- | find location for static data installed by ghcjs-boot
getDataDir :: FilePath
           -> FilePath
getDataDir topDir = topDir

-- | default location to get data files when booting: Cabal data directory
ghcjsBootDefaultDataDir :: IO FilePath
ghcjsBootDefaultDataDir = pure "/nonexistent" -- Paths_ghcjs.getDataDir
