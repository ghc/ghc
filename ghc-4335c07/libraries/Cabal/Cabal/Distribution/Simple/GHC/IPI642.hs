-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.GHC.IPI642
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--

module Distribution.Simple.GHC.IPI642 (
    InstalledPackageInfo(..),
    toCurrent,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import qualified Distribution.InstalledPackageInfo as Current
import qualified Distribution.Types.AbiHash        as Current
import qualified Distribution.Types.ComponentId    as Current
import qualified Distribution.Types.UnitId         as Current
import Distribution.Simple.GHC.IPIConvert
import Distribution.Text

-- | This is the InstalledPackageInfo type used by ghc-6.4.2 and later.
--
-- It's here purely for the 'Read' instance so that we can read the package
-- database used by those ghc versions. It is a little hacky to read the
-- package db directly, but we do need the info and until ghc-6.9 there was
-- no better method.
--
-- In ghc-6.4.1 and before the format was slightly different.
-- See "Distribution.Simple.GHC.IPI642"
--
data InstalledPackageInfo = InstalledPackageInfo {
    package           :: PackageIdentifier,
    license           :: License,
    copyright         :: String,
    maintainer        :: String,
    author            :: String,
    stability         :: String,
    homepage          :: String,
    pkgUrl            :: String,
    description       :: String,
    category          :: String,
    exposed           :: Bool,
    exposedModules    :: [String],
    hiddenModules     :: [String],
    importDirs        :: [FilePath],
    libraryDirs       :: [FilePath],
    hsLibraries       :: [String],
    extraLibraries    :: [String],
    extraGHCiLibraries:: [String],
    includeDirs       :: [FilePath],
    includes          :: [String],
    depends           :: [PackageIdentifier],
    hugsOptions       :: [String],
    ccOptions         :: [String],
    ldOptions         :: [String],
    frameworkDirs     :: [FilePath],
    frameworks        :: [String],
    haddockInterfaces :: [FilePath],
    haddockHTMLs      :: [FilePath]
  }
  deriving Read

toCurrent :: InstalledPackageInfo -> Current.InstalledPackageInfo
toCurrent ipi@InstalledPackageInfo{} =
  let mkExposedModule m = Current.ExposedModule m Nothing
      pid = convertPackageId (package ipi)
  in Current.InstalledPackageInfo {
    Current.sourcePackageId    = pid,
    Current.installedUnitId    = Current.mkLegacyUnitId pid,
    Current.installedComponentId_ = Current.mkComponentId (display pid),
    Current.instantiatedWith   = [],
    -- Internal libraries not supported!
    Current.sourceLibName      = Nothing,
    Current.compatPackageKey   = "",
    Current.abiHash            = Current.mkAbiHash "", -- bogus but old GHCs don't care.
    Current.license            = convertLicense (license ipi),
    Current.copyright          = copyright ipi,
    Current.maintainer         = maintainer ipi,
    Current.author             = author ipi,
    Current.stability          = stability ipi,
    Current.homepage           = homepage ipi,
    Current.pkgUrl             = pkgUrl ipi,
    Current.synopsis           = "",
    Current.description        = description ipi,
    Current.category           = category ipi,
    Current.indefinite         = False,
    Current.exposed            = exposed ipi,
    Current.exposedModules     = map (mkExposedModule . convertModuleName) (exposedModules ipi),
    Current.hiddenModules      = map convertModuleName (hiddenModules ipi),
    Current.trusted            = Current.trusted Current.emptyInstalledPackageInfo,
    Current.importDirs         = importDirs ipi,
    Current.libraryDirs        = libraryDirs ipi,
    Current.libraryDynDirs     = [],
    Current.dataDir            = "",
    Current.hsLibraries        = hsLibraries ipi,
    Current.extraLibraries     = extraLibraries ipi,
    Current.extraGHCiLibraries = extraGHCiLibraries ipi,
    Current.includeDirs        = includeDirs ipi,
    Current.includes           = includes ipi,
    Current.depends            = map (Current.mkLegacyUnitId . convertPackageId) (depends ipi),
    Current.abiDepends         = [],
    Current.ccOptions          = ccOptions ipi,
    Current.ldOptions          = ldOptions ipi,
    Current.frameworkDirs      = frameworkDirs ipi,
    Current.frameworks         = frameworks ipi,
    Current.haddockInterfaces  = haddockInterfaces ipi,
    Current.haddockHTMLs       = haddockHTMLs ipi,
    Current.pkgRoot            = Nothing
  }
