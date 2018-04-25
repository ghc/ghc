module Distribution.Types.PackageDescription.Lens (
    PackageDescription,
    module Distribution.Types.PackageDescription.Lens,
    ) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Compat.Lens

import Distribution.Compiler                 (CompilerFlavor)
import Distribution.License                  (License)
import Distribution.Types.Benchmark          (Benchmark)
import Distribution.Types.BuildType          (BuildType)
import Distribution.Types.Dependency         (Dependency)
import Distribution.Types.Executable         (Executable)
import Distribution.Types.ForeignLib         (ForeignLib)
import Distribution.Types.Library            (Library)
import Distribution.Types.PackageDescription (PackageDescription)
import Distribution.Types.PackageId          (PackageIdentifier)
import Distribution.Types.SetupBuildInfo     (SetupBuildInfo)
import Distribution.Types.SourceRepo         (SourceRepo)
import Distribution.Types.TestSuite          (TestSuite)
import Distribution.Version                  (Version, VersionRange)

import qualified Distribution.Types.PackageDescription as T

package :: Lens' PackageDescription PackageIdentifier
package f s = fmap (\x -> s { T.package = x }) (f (T.package s))
{-# INLINE package #-}

license :: Lens' PackageDescription License
license f s = fmap (\x -> s { T.license = x }) (f (T.license s))
{-# INLINE license #-}

licenseFiles :: Lens' PackageDescription [String]
licenseFiles f s = fmap (\x -> s { T.licenseFiles = x }) (f (T.licenseFiles s))
{-# INLINE licenseFiles #-}

copyright :: Lens' PackageDescription String
copyright f s = fmap (\x -> s { T.copyright = x }) (f (T.copyright s))
{-# INLINE copyright #-}

maintainer :: Lens' PackageDescription String
maintainer f s = fmap (\x -> s { T.maintainer = x }) (f (T.maintainer s))
{-# INLINE maintainer #-}

author :: Lens' PackageDescription String
author f s = fmap (\x -> s { T.author = x }) (f (T.author s))
{-# INLINE author #-}

stability :: Lens' PackageDescription String
stability f s = fmap (\x -> s { T.stability = x }) (f (T.stability s))
{-# INLINE stability #-}

testedWith :: Lens' PackageDescription [(CompilerFlavor,VersionRange)]
testedWith f s = fmap (\x -> s { T.testedWith = x }) (f (T.testedWith s))
{-# INLINE testedWith #-}

homepage :: Lens' PackageDescription String
homepage f s = fmap (\x -> s { T.homepage = x }) (f (T.homepage s))
{-# INLINE homepage #-}

pkgUrl :: Lens' PackageDescription String
pkgUrl f s = fmap (\x -> s { T.pkgUrl = x }) (f (T.pkgUrl s))
{-# INLINE pkgUrl #-}

bugReports :: Lens' PackageDescription String
bugReports f s = fmap (\x -> s { T.bugReports = x }) (f (T.bugReports s))
{-# INLINE bugReports #-}

sourceRepos :: Lens' PackageDescription [SourceRepo]
sourceRepos f s = fmap (\x -> s { T.sourceRepos = x }) (f (T.sourceRepos s))
{-# INLINE sourceRepos #-}

synopsis :: Lens' PackageDescription String
synopsis f s = fmap (\x -> s { T.synopsis = x }) (f (T.synopsis s))
{-# INLINE synopsis #-}

description :: Lens' PackageDescription String
description f s = fmap (\x -> s { T.description = x }) (f (T.description s))
{-# INLINE description #-}

category :: Lens' PackageDescription String
category f s = fmap (\x -> s { T.category = x }) (f (T.category s))
{-# INLINE category #-}

customFieldsPD :: Lens' PackageDescription [(String,String)]
customFieldsPD f s = fmap (\x -> s { T.customFieldsPD = x }) (f (T.customFieldsPD s))
{-# INLINE customFieldsPD #-}

buildDepends :: Lens' PackageDescription [Dependency]
buildDepends f s = fmap (\x -> s { T.buildDepends = x }) (f (T.buildDepends s))
{-# INLINE buildDepends #-}

specVersionRaw :: Lens' PackageDescription (Either Version VersionRange)
specVersionRaw f s = fmap (\x -> s { T.specVersionRaw = x }) (f (T.specVersionRaw s))
{-# INLINE specVersionRaw #-}

buildType :: Lens' PackageDescription (Maybe BuildType)
buildType f s = fmap (\x -> s { T.buildType = x }) (f (T.buildType s))
{-# INLINE buildType #-}

setupBuildInfo :: Lens' PackageDescription (Maybe SetupBuildInfo)
setupBuildInfo f s = fmap (\x -> s { T.setupBuildInfo = x }) (f (T.setupBuildInfo s))
{-# INLINE setupBuildInfo #-}

library :: Lens' PackageDescription (Maybe Library)
library f s = fmap (\x -> s { T.library = x }) (f (T.library s))
{-# INLINE library #-}

subLibraries :: Lens' PackageDescription [Library]
subLibraries f s = fmap (\x -> s { T.subLibraries = x }) (f (T.subLibraries s))
{-# INLINE subLibraries #-}

executables :: Lens' PackageDescription [Executable]
executables f s = fmap (\x -> s { T.executables = x }) (f (T.executables s))
{-# INLINE executables #-}

foreignLibs :: Lens' PackageDescription [ForeignLib]
foreignLibs f s = fmap (\x -> s { T.foreignLibs = x }) (f (T.foreignLibs s))
{-# INLINE foreignLibs #-}

testSuites :: Lens' PackageDescription [TestSuite]
testSuites f s = fmap (\x -> s { T.testSuites = x }) (f (T.testSuites s))
{-# INLINE testSuites #-}

benchmarks :: Lens' PackageDescription [Benchmark]
benchmarks f s = fmap (\x -> s { T.benchmarks = x }) (f (T.benchmarks s))
{-# INLINE benchmarks #-}

dataFiles :: Lens' PackageDescription [FilePath]
dataFiles f s = fmap (\x -> s { T.dataFiles = x }) (f (T.dataFiles s))
{-# INLINE dataFiles #-}

dataDir :: Lens' PackageDescription FilePath
dataDir f s = fmap (\x -> s { T.dataDir = x }) (f (T.dataDir s))
{-# INLINE dataDir #-}

extraSrcFiles :: Lens' PackageDescription [String]
extraSrcFiles f s = fmap (\x -> s { T.extraSrcFiles = x }) (f (T.extraSrcFiles s))
{-# INLINE extraSrcFiles #-}

extraTmpFiles :: Lens' PackageDescription [String]
extraTmpFiles f s = fmap (\x -> s { T.extraTmpFiles = x }) (f (T.extraTmpFiles s))
{-# INLINE extraTmpFiles #-}

extraDocFiles :: Lens' PackageDescription [String]
extraDocFiles f s = fmap (\x -> s { T.extraDocFiles = x }) (f (T.extraDocFiles s))
{-# INLINE extraDocFiles #-}
