{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-
   compatibility with older GHC
-}
module Compiler.Compat ( PackageKey
                       , packageKeyString
                       , modulePackageKey
                       , stringToPackageKey
                       , primPackageKey
                       , mainPackageKey
                       , modulePackageName
                       , getPackageName
                       , getInstalledPackageName
                       , getPackageVersion
                       , getInstalledPackageVersion
                       , getPackageLibDirs
                       , getInstalledPackageLibDirs
                       , getPackageHsLibs
                       , getInstalledPackageHsLibs
                       , searchModule
                       , Version(..)
                       , showVersion
                       , isEmptyVersion
                       ) where

import Module
import DynFlags
import FastString

import Prelude

import Packages hiding ( Version )

import           Data.Binary
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Version as DV


-- we do not support version tags since support for them has
-- been broken for a long time anyway
newtype Version = Version { unVersion :: [Integer] }
  deriving (Ord, Eq, Show, Binary)

showVersion :: Version -> Text
showVersion = T.intercalate "." . map (T.pack . show) . unVersion

isEmptyVersion :: Version -> Bool
isEmptyVersion = null . unVersion

convertVersion :: DV.Version -> Version
convertVersion v = Version (map fromIntegral $ versionBranch v)

type PackageKey = UnitId

packageKeyString :: UnitId -> String
packageKeyString = unitIdString

modulePackageKey :: Module -> UnitId
modulePackageKey = moduleUnitId

stringToPackageKey :: String -> InstalledUnitId
stringToPackageKey = stringToInstalledUnitId

primPackageKey :: UnitId
primPackageKey = primUnitId

mainPackageKey :: UnitId
mainPackageKey = mainUnitId

getPackageName :: DynFlags -> UnitId -> String
getPackageName dflags
  = maybe "" ((\(PackageName n) -> unpackFS n) . packageName)
  . lookupPackage dflags

getInstalledPackageName :: DynFlags -> InstalledUnitId -> String
getInstalledPackageName dflags
  = maybe "" ((\(PackageName n) -> unpackFS n) . packageName)
  . lookupInstalledPackage dflags

modulePackageName :: DynFlags -> Module -> String
modulePackageName dflags
  = getPackageName dflags . moduleUnitId

getPackageVersion :: DynFlags -> UnitId -> Maybe Version
getPackageVersion dflags
  = fmap (convertVersion . packageVersion)
  . lookupPackage dflags

getInstalledPackageVersion :: DynFlags -> InstalledUnitId -> Maybe Version
getInstalledPackageVersion dflags
  = fmap (convertVersion . packageVersion)
  . lookupInstalledPackage dflags

getPackageLibDirs :: DynFlags -> UnitId -> [FilePath]
getPackageLibDirs dflags
  = maybe [] libraryDirs . lookupPackage dflags

getInstalledPackageLibDirs :: DynFlags -> InstalledUnitId -> [FilePath]
getInstalledPackageLibDirs dflags
  = maybe [] libraryDirs . lookupInstalledPackage dflags

getPackageHsLibs :: DynFlags -> UnitId -> [String]
getPackageHsLibs dflags
  = maybe [] hsLibraries . lookupPackage dflags

getInstalledPackageHsLibs :: DynFlags -> InstalledUnitId -> [String]
getInstalledPackageHsLibs dflags
  = maybe [] hsLibraries . lookupInstalledPackage dflags

searchModule :: DynFlags -> ModuleName -> [(String, UnitId)]
searchModule dflags
  = map ((\k -> (getPackageName dflags k, k)) . moduleUnitId . fst)
--  $ fromLookupResult
--  $ lookupModuleWithSuggestions dflags mn Nothing
  . lookupModuleInAllPackages dflags
