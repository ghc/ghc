{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Types.PackageDescription
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defines the data structure for the @.cabal@ file format. There are
-- several parts to this structure. It has top level info and then 'Library',
-- 'Executable', 'TestSuite', and 'Benchmark' sections each of which have
-- associated 'BuildInfo' data that's used to build the library, exe, test, or
-- benchmark.  To further complicate things there is both a 'PackageDescription'
-- and a 'GenericPackageDescription'. This distinction relates to cabal
-- configurations. When we initially read a @.cabal@ file we get a
-- 'GenericPackageDescription' which has all the conditional sections.
-- Before actually building a package we have to decide
-- on each conditional. Once we've done that we get a 'PackageDescription'.
-- It was done this way initially to avoid breaking too much stuff when the
-- feature was introduced. It could probably do with being rationalised at some
-- point to make it simpler.

module Distribution.Types.PackageDescription (
    PackageDescription(..),
    specVersion,
    descCabalVersion,
    emptyPackageDescription,
    hasPublicLib,
    hasLibs,
    allLibraries,
    withLib,
    hasExes,
    withExe,
    hasTests,
    withTest,
    hasBenchmarks,
    withBenchmark,
    hasForeignLibs,
    withForeignLib,
    allBuildInfo,
    enabledBuildInfos,
    updatePackageDescription,
    pkgComponents,
    pkgBuildableComponents,
    enabledComponents,
    lookupComponent,
    getComponent,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.Library
import Distribution.Types.TestSuite
import Distribution.Types.Executable
import Distribution.Types.Benchmark
import Distribution.Types.ForeignLib

import Distribution.Types.Component
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.Dependency
import Distribution.Types.PackageId
import Distribution.Types.ComponentName
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
import Distribution.Types.SetupBuildInfo
import Distribution.Types.BuildInfo
import Distribution.Types.BuildType
import Distribution.Types.SourceRepo
import Distribution.Types.HookedBuildInfo

import Distribution.Package
import Distribution.Version
import Distribution.License
import Distribution.Compiler

-- -----------------------------------------------------------------------------
-- The PackageDescription type

-- | This data type is the internal representation of the file @pkg.cabal@.
-- It contains two kinds of information about the package: information
-- which is needed for all packages, such as the package name and version, and
-- information which is needed for the simple build system only, such as
-- the compiler options and library name.
--
data PackageDescription
    =  PackageDescription {
        -- the following are required by all packages:
        package        :: PackageIdentifier,
        license        :: License,
        licenseFiles   :: [FilePath],
        copyright      :: String,
        maintainer     :: String,
        author         :: String,
        stability      :: String,
        testedWith     :: [(CompilerFlavor,VersionRange)],
        homepage       :: String,
        pkgUrl         :: String,
        bugReports     :: String,
        sourceRepos    :: [SourceRepo],
        synopsis       :: String, -- ^A one-line summary of this package
        description    :: String, -- ^A more verbose description of this package
        category       :: String,
        customFieldsPD :: [(String,String)], -- ^Custom fields starting
                                             -- with x-, stored in a
                                             -- simple assoc-list.

        -- | YOU PROBABLY DON'T WANT TO USE THIS FIELD. This field is
        -- special! Depending on how far along processing the
        -- PackageDescription we are, the contents of this field are
        -- either nonsense, or the collected dependencies of *all* the
        -- components in this package.  buildDepends is initialized by
        -- 'finalizePD' and 'flattenPackageDescription';
        -- prior to that, dependency info is stored in the 'CondTree'
        -- built around a 'GenericPackageDescription'.  When this
        -- resolution is done, dependency info is written to the inner
        -- 'BuildInfo' and this field.  This is all horrible, and #2066
        -- tracks progress to get rid of this field.
        buildDepends   :: [Dependency],
        -- | The version of the Cabal spec that this package description uses.
        -- For historical reasons this is specified with a version range but
        -- only ranges of the form @>= v@ make sense. We are in the process of
        -- transitioning to specifying just a single version, not a range.
        specVersionRaw :: Either Version VersionRange,
        buildType      :: Maybe BuildType,
        setupBuildInfo :: Maybe SetupBuildInfo,
        -- components
        library        :: Maybe Library,
        subLibraries   :: [Library],
        executables    :: [Executable],
        foreignLibs    :: [ForeignLib],
        testSuites     :: [TestSuite],
        benchmarks     :: [Benchmark],
        -- files
        dataFiles      :: [FilePath],
        dataDir        :: FilePath,
        extraSrcFiles  :: [FilePath],
        extraTmpFiles  :: [FilePath],
        extraDocFiles  :: [FilePath]
    }
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary PackageDescription

instance Package PackageDescription where
  packageId = package

-- | The version of the Cabal spec that this package should be interpreted
-- against.
--
-- Historically we used a version range but we are switching to using a single
-- version. Currently we accept either. This function converts into a single
-- version by ignoring upper bounds in the version range.
--
specVersion :: PackageDescription -> Version
specVersion pkg = case specVersionRaw pkg of
  Left  version      -> version
  Right versionRange -> case asVersionIntervals versionRange of
                          []                            -> mkVersion [0]
                          ((LowerBound version _, _):_) -> version

-- | The range of versions of the Cabal tools that this package is intended to
-- work with.
--
-- This function is deprecated and should not be used for new purposes, only to
-- support old packages that rely on the old interpretation.
--
descCabalVersion :: PackageDescription -> VersionRange
descCabalVersion pkg = case specVersionRaw pkg of
  Left  version      -> orLaterVersion version
  Right versionRange -> versionRange
{-# DEPRECATED descCabalVersion "Use specVersion instead" #-}

emptyPackageDescription :: PackageDescription
emptyPackageDescription
    =  PackageDescription {
                      package      = PackageIdentifier (mkPackageName "")
                                                       nullVersion,
                      license      = UnspecifiedLicense,
                      licenseFiles = [],
                      specVersionRaw = Right anyVersion,
                      buildType    = Nothing,
                      copyright    = "",
                      maintainer   = "",
                      author       = "",
                      stability    = "",
                      testedWith   = [],
                      buildDepends = [],
                      homepage     = "",
                      pkgUrl       = "",
                      bugReports   = "",
                      sourceRepos  = [],
                      synopsis     = "",
                      description  = "",
                      category     = "",
                      customFieldsPD = [],
                      setupBuildInfo = Nothing,
                      library      = Nothing,
                      subLibraries = [],
                      foreignLibs  = [],
                      executables  = [],
                      testSuites   = [],
                      benchmarks   = [],
                      dataFiles    = [],
                      dataDir      = "",
                      extraSrcFiles = [],
                      extraTmpFiles = [],
                      extraDocFiles = []
                     }

-- ---------------------------------------------------------------------------
-- The Library type

-- | Does this package have a buildable PUBLIC library?
hasPublicLib :: PackageDescription -> Bool
hasPublicLib p =
    case library p of
        Just lib -> buildable (libBuildInfo lib)
        Nothing  -> False

-- | Does this package have any libraries?
hasLibs :: PackageDescription -> Bool
hasLibs p = any (buildable . libBuildInfo) (allLibraries p)

allLibraries :: PackageDescription -> [Library]
allLibraries p = maybeToList (library p) ++ subLibraries p

-- | If the package description has a buildable library section,
-- call the given function with the library build info as argument.
-- You probably want 'withLibLBI' if you have a 'LocalBuildInfo',
-- see the note in
-- "Distribution.Types.ComponentRequestedSpec#buildable_vs_enabled_components"
-- for more information.
withLib :: PackageDescription -> (Library -> IO ()) -> IO ()
withLib pkg_descr f =
   sequence_ [f lib | lib <- allLibraries pkg_descr, buildable (libBuildInfo lib)]

-- ---------------------------------------------------------------------------
-- The Executable type

-- |does this package have any executables?
hasExes :: PackageDescription -> Bool
hasExes p = any (buildable . buildInfo) (executables p)

-- | Perform the action on each buildable 'Executable' in the package
-- description.  You probably want 'withExeLBI' if you have a
-- 'LocalBuildInfo', see the note in
-- "Distribution.Types.ComponentRequestedSpec#buildable_vs_enabled_components"
-- for more information.
withExe :: PackageDescription -> (Executable -> IO ()) -> IO ()
withExe pkg_descr f =
  sequence_ [f exe | exe <- executables pkg_descr, buildable (buildInfo exe)]

-- ---------------------------------------------------------------------------
-- The TestSuite type

-- | Does this package have any test suites?
hasTests :: PackageDescription -> Bool
hasTests = any (buildable . testBuildInfo) . testSuites

-- | Perform an action on each buildable 'TestSuite' in a package.
-- You probably want 'withTestLBI' if you have a 'LocalBuildInfo', see the note in
-- "Distribution.Types.ComponentRequestedSpec#buildable_vs_enabled_components"
-- for more information.

withTest :: PackageDescription -> (TestSuite -> IO ()) -> IO ()
withTest pkg_descr f =
    sequence_ [ f test | test <- testSuites pkg_descr, buildable (testBuildInfo test) ]

-- ---------------------------------------------------------------------------
-- The Benchmark type

-- | Does this package have any benchmarks?
hasBenchmarks :: PackageDescription -> Bool
hasBenchmarks = any (buildable . benchmarkBuildInfo) . benchmarks

-- | Perform an action on each buildable 'Benchmark' in a package.
-- You probably want 'withBenchLBI' if you have a 'LocalBuildInfo', see the note in
-- "Distribution.Types.ComponentRequestedSpec#buildable_vs_enabled_components"
-- for more information.

withBenchmark :: PackageDescription -> (Benchmark -> IO ()) -> IO ()
withBenchmark pkg_descr f =
    sequence_ [f bench | bench <- benchmarks pkg_descr, buildable (benchmarkBuildInfo bench)]

-- ---------------------------------------------------------------------------
-- The ForeignLib type

-- | Does this package have any foreign libraries?
hasForeignLibs :: PackageDescription -> Bool
hasForeignLibs p = any (buildable . foreignLibBuildInfo) (foreignLibs p)

-- | Perform the action on each buildable 'ForeignLib' in the package
-- description.
withForeignLib :: PackageDescription -> (ForeignLib -> IO ()) -> IO ()
withForeignLib pkg_descr f =
  sequence_ [ f flib
            | flib <- foreignLibs pkg_descr
            , buildable (foreignLibBuildInfo flib)
            ]

-- ---------------------------------------------------------------------------
-- The BuildInfo type

-- | The 'BuildInfo' for the library (if there is one and it's buildable), and
-- all buildable executables, test suites and benchmarks.  Useful for gathering
-- dependencies.
allBuildInfo :: PackageDescription -> [BuildInfo]
allBuildInfo pkg_descr = [ bi | lib <- allLibraries pkg_descr
                              , let bi = libBuildInfo lib
                              , buildable bi ]
                      ++ [ bi | flib <- foreignLibs pkg_descr
                              , let bi = foreignLibBuildInfo flib
                              , buildable bi ]
                      ++ [ bi | exe <- executables pkg_descr
                              , let bi = buildInfo exe
                              , buildable bi ]
                      ++ [ bi | tst <- testSuites pkg_descr
                              , let bi = testBuildInfo tst
                              , buildable bi ]
                      ++ [ bi | tst <- benchmarks pkg_descr
                              , let bi = benchmarkBuildInfo tst
                              , buildable bi ]
  --FIXME: many of the places where this is used, we actually want to look at
  --       unbuildable bits too, probably need separate functions

-- | Return all of the 'BuildInfo's of enabled components, i.e., all of
-- the ones that would be built if you run @./Setup build@.
enabledBuildInfos :: PackageDescription -> ComponentRequestedSpec -> [BuildInfo]
enabledBuildInfos pkg enabled =
    [ componentBuildInfo comp
    | comp <- enabledComponents pkg enabled ]


-- ------------------------------------------------------------
-- * Utils
-- ------------------------------------------------------------

updatePackageDescription :: HookedBuildInfo -> PackageDescription -> PackageDescription
updatePackageDescription (mb_lib_bi, exe_bi) p
    = p{ executables = updateExecutables exe_bi    (executables p)
       , library     = updateLibrary     mb_lib_bi (library     p) }
    where
      updateLibrary :: Maybe BuildInfo -> Maybe Library -> Maybe Library
      updateLibrary (Just bi) (Just lib) = Just (lib{libBuildInfo = bi `mappend` libBuildInfo lib})
      updateLibrary Nothing   mb_lib     = mb_lib
      updateLibrary (Just _)  Nothing    = Nothing

      updateExecutables :: [(UnqualComponentName, BuildInfo)] -- ^[(exeName, new buildinfo)]
        -> [Executable]                                       -- ^list of executables to update
        -> [Executable]                                       -- ^list with exeNames updated
      updateExecutables exe_bi' executables' = foldr updateExecutable executables' exe_bi'

      updateExecutable :: (UnqualComponentName, BuildInfo) -- ^(exeName, new buildinfo)
                       -> [Executable]                     -- ^list of executables to update
                       -> [Executable]                     -- ^list with exeName updated
      updateExecutable _                 []         = []
      updateExecutable exe_bi'@(name,bi) (exe:exes)
        | exeName exe == name = exe{buildInfo = bi `mappend` buildInfo exe} : exes
        | otherwise           = exe : updateExecutable exe_bi' exes

-- -----------------------------------------------------------------------------
-- Source-representation of buildable components

-- | All the components in the package.
--
pkgComponents :: PackageDescription -> [Component]
pkgComponents pkg =
    [ CLib  lib | lib <- allLibraries pkg ]
 ++ [ CFLib flib | flib <- foreignLibs pkg ]
 ++ [ CExe  exe | exe <- executables pkg ]
 ++ [ CTest tst | tst <- testSuites  pkg ]
 ++ [ CBench bm | bm  <- benchmarks  pkg ]

-- | A list of all components in the package that are buildable,
-- i.e., were not marked with @buildable: False@.  This does NOT
-- indicate if we are actually going to build the component,
-- see 'enabledComponents' instead.
--
-- @since 2.0.0.2
--
pkgBuildableComponents :: PackageDescription -> [Component]
pkgBuildableComponents = filter componentBuildable . pkgComponents

-- | A list of all components in the package that are enabled.
--
-- @since 2.0.0.2
--
enabledComponents :: PackageDescription -> ComponentRequestedSpec -> [Component]
enabledComponents pkg enabled = filter (componentEnabled enabled) $ pkgBuildableComponents pkg

lookupComponent :: PackageDescription -> ComponentName -> Maybe Component
lookupComponent pkg CLibName = fmap CLib (library pkg)
lookupComponent pkg (CSubLibName name) =
    fmap CLib $ find ((Just name ==) . libName) (subLibraries pkg)
lookupComponent pkg (CFLibName name) =
    fmap CFLib $ find ((name ==) . foreignLibName) (foreignLibs pkg)
lookupComponent pkg (CExeName name) =
    fmap CExe $ find ((name ==) . exeName) (executables pkg)
lookupComponent pkg (CTestName name) =
    fmap CTest $ find ((name ==) . testName) (testSuites pkg)
lookupComponent pkg (CBenchName name) =
    fmap CBench $ find ((name ==) . benchmarkName) (benchmarks pkg)

getComponent :: PackageDescription -> ComponentName -> Component
getComponent pkg cname =
    case lookupComponent pkg cname of
      Just cpnt -> cpnt
      Nothing   -> missingComponent
  where
    missingComponent =
      error $ "internal error: the package description contains no "
           ++ "component corresponding to " ++ show cname
