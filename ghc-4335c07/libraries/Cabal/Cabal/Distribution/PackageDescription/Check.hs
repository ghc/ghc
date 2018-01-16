-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription.Check
-- Copyright   :  Lennart Kolmodin 2008
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This has code for checking for various problems in packages. There is one
-- set of checks that just looks at a 'PackageDescription' in isolation and
-- another set of checks that also looks at files in the package. Some of the
-- checks are basic sanity checks, others are portability standards that we'd
-- like to encourage. There is a 'PackageCheck' type that distinguishes the
-- different kinds of check so we can see which ones are appropriate to report
-- in different situations. This code gets uses when configuring a package when
-- we consider only basic problems. The higher standard is uses when when
-- preparing a source tarball and by Hackage when uploading new packages. The
-- reason for this is that we want to hold packages that are expected to be
-- distributed to a higher standard than packages that are only ever expected
-- to be used on the author's own environment.

module Distribution.PackageDescription.Check (
        -- * Package Checking
        PackageCheck(..),
        checkPackage,
        checkConfiguredPackage,

        -- ** Checking package contents
        checkPackageFiles,
        checkPackageContent,
        CheckPackageContentOps(..),
        checkPackageFileNames,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import qualified Distribution.Compat.DList as DList
import Distribution.Compiler
import Distribution.System
import Distribution.License
import Distribution.Simple.BuildPaths (autogenPathsModuleName)
import Distribution.Simple.BuildToolDepends
import Distribution.Simple.CCompiler
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.CondTree
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.PackageName
import Distribution.Types.ExecutableScope
import Distribution.Types.UnqualComponentName
import Distribution.Simple.Utils hiding (findPackageDesc, notice)
import Distribution.Version
import Distribution.Package
import Distribution.Text
import Distribution.Utils.Generic (isAscii)
import Language.Haskell.Extension

import Control.Monad (mapM)
import qualified Data.ByteString.Lazy as BS
import Data.List  (group)
import qualified System.Directory as System
         ( doesFileExist, doesDirectoryExist )
import qualified Data.Map as Map

import qualified System.Directory (getDirectoryContents)
import System.FilePath
         ( (</>), (<.>), takeExtension, takeFileName, splitDirectories
         , splitPath, splitExtension )
import System.FilePath.Windows as FilePath.Windows
         ( isValid )

import qualified Data.Set as Set

import Distribution.Compat.Lens
import qualified Distribution.Types.BuildInfo.Lens as L
import qualified Distribution.Types.PackageDescription.Lens as L
import qualified Distribution.Types.GenericPackageDescription.Lens as L

-- | Results of some kind of failed package check.
--
-- There are a range of severities, from merely dubious to totally insane.
-- All of them come with a human readable explanation. In future we may augment
-- them with more machine readable explanations, for example to help an IDE
-- suggest automatic corrections.
--
data PackageCheck =

       -- | This package description is no good. There's no way it's going to
       -- build sensibly. This should give an error at configure time.
       PackageBuildImpossible { explanation :: String }

       -- | A problem that is likely to affect building the package, or an
       -- issue that we'd like every package author to be aware of, even if
       -- the package is never distributed.
     | PackageBuildWarning { explanation :: String }

       -- | An issue that might not be a problem for the package author but
       -- might be annoying or detrimental when the package is distributed to
       -- users. We should encourage distributed packages to be free from these
       -- issues, but occasionally there are justifiable reasons so we cannot
       -- ban them entirely.
     | PackageDistSuspicious { explanation :: String }

       -- | Like PackageDistSuspicious but will only display warnings
       -- rather than causing abnormal exit when you run 'cabal check'.
     | PackageDistSuspiciousWarn { explanation :: String }

       -- | An issue that is OK in the author's environment but is almost
       -- certain to be a portability problem for other environments. We can
       -- quite legitimately refuse to publicly distribute packages with these
       -- problems.
     | PackageDistInexcusable { explanation :: String }
  deriving (Eq)

instance Show PackageCheck where
    show notice = explanation notice

check :: Bool -> PackageCheck -> Maybe PackageCheck
check False _  = Nothing
check True  pc = Just pc

checkSpecVersion :: PackageDescription -> [Int] -> Bool -> PackageCheck
                 -> Maybe PackageCheck
checkSpecVersion pkg specver cond pc
  | specVersion pkg >= mkVersion specver  = Nothing
  | otherwise                             = check cond pc

-- ------------------------------------------------------------
-- * Standard checks
-- ------------------------------------------------------------

-- | Check for common mistakes and problems in package descriptions.
--
-- This is the standard collection of checks covering all aspects except
-- for checks that require looking at files within the package. For those
-- see 'checkPackageFiles'.
--
-- It requires the 'GenericPackageDescription' and optionally a particular
-- configuration of that package. If you pass 'Nothing' then we just check
-- a version of the generic description using 'flattenPackageDescription'.
--
checkPackage :: GenericPackageDescription
             -> Maybe PackageDescription
             -> [PackageCheck]
checkPackage gpkg mpkg =
     checkConfiguredPackage pkg
  ++ checkConditionals gpkg
  ++ checkPackageVersions gpkg
  ++ checkDevelopmentOnlyFlags gpkg
  ++ checkFlagNames gpkg
  ++ checkUnusedFlags gpkg
  ++ checkUnicodeXFields gpkg
  where
    pkg = fromMaybe (flattenPackageDescription gpkg) mpkg

--TODO: make this variant go away
--      we should always know the GenericPackageDescription
checkConfiguredPackage :: PackageDescription -> [PackageCheck]
checkConfiguredPackage pkg =
    checkSanity pkg
 ++ checkFields pkg
 ++ checkLicense pkg
 ++ checkSourceRepos pkg
 ++ checkGhcOptions pkg
 ++ checkCCOptions pkg
 ++ checkCPPOptions pkg
 ++ checkPaths pkg
 ++ checkCabalVersion pkg


-- ------------------------------------------------------------
-- * Basic sanity checks
-- ------------------------------------------------------------

-- | Check that this package description is sane.
--
checkSanity :: PackageDescription -> [PackageCheck]
checkSanity pkg =
  catMaybes [

    check (null . unPackageName . packageName $ pkg) $
      PackageBuildImpossible "No 'name' field."

  , check (nullVersion == packageVersion pkg) $
      PackageBuildImpossible "No 'version' field."

  , check (all ($ pkg) [ null . executables
                       , null . testSuites
                       , null . benchmarks
                       , null . allLibraries
                       , null . foreignLibs ]) $
      PackageBuildImpossible
        "No executables, libraries, tests, or benchmarks found. Nothing to do."

  , check (any isNothing (map libName $ subLibraries pkg)) $
      PackageBuildImpossible $ "Found one or more unnamed internal libraries. "
        ++ "Only the non-internal library can have the same name as the package."

  , check (not (null duplicateNames)) $
      PackageBuildImpossible $ "Duplicate sections: "
        ++ commaSep (map unUnqualComponentName duplicateNames)
        ++ ". The name of every library, executable, test suite,"
        ++ " and benchmark section in"
        ++ " the package must be unique."

  -- NB: but it's OK for executables to have the same name!
  -- TODO shouldn't need to compare on the string level
  , check (any (== display (packageName pkg)) (display <$> subLibNames)) $
      PackageBuildImpossible $ "Illegal internal library name "
        ++ display (packageName pkg)
        ++ ". Internal libraries cannot have the same name as the package."
        ++ " Maybe you wanted a non-internal library?"
        ++ " If so, rewrite the section stanza"
        ++ " from 'library: '" ++ display (packageName pkg) ++ "' to 'library'."
  ]
  --TODO: check for name clashes case insensitively: windows file systems cannot
  --cope.

  ++ concatMap (checkLibrary    pkg) (allLibraries pkg)
  ++ concatMap (checkExecutable pkg) (executables pkg)
  ++ concatMap (checkTestSuite  pkg) (testSuites pkg)
  ++ concatMap (checkBenchmark  pkg) (benchmarks pkg)

  ++ catMaybes [

    check (specVersion pkg > cabalVersion) $
      PackageBuildImpossible $
           "This package description follows version "
        ++ display (specVersion pkg) ++ " of the Cabal specification. This "
        ++ "tool only supports up to version " ++ display cabalVersion ++ "."
  ]
  where
    -- The public 'library' gets special dispensation, because it
    -- is common practice to export a library and name the executable
    -- the same as the package.
    subLibNames = catMaybes . map libName $ subLibraries pkg
    exeNames = map exeName $ executables pkg
    testNames = map testName $ testSuites pkg
    bmNames = map benchmarkName $ benchmarks pkg
    duplicateNames = dups $ subLibNames ++ exeNames ++ testNames ++ bmNames

checkLibrary :: PackageDescription -> Library -> [PackageCheck]
checkLibrary pkg lib =
  catMaybes [

    check (not (null moduleDuplicates)) $
       PackageBuildImpossible $
            "Duplicate modules in library: "
         ++ commaSep (map display moduleDuplicates)

  -- TODO: This check is bogus if a required-signature was passed through
  , check (null (explicitLibModules lib) && null (reexportedModules lib)) $
      PackageDistSuspiciousWarn $
           "Library " ++ (case libName lib of
                            Nothing -> ""
                            Just n -> display n
                            ) ++ "does not expose any modules"

    -- check use of signatures sections
  , checkVersion [1,25] (not (null (signatures lib))) $
      PackageDistInexcusable $
           "To use the 'signatures' field the package needs to specify "
        ++ "at least 'cabal-version: >= 1.25'."

    -- check that all autogen-modules appear on other-modules or exposed-modules
  , check
      (not $ and $ map (flip elem (explicitLibModules lib)) (libModulesAutogen lib)) $
      PackageBuildImpossible $
           "An 'autogen-module' is neither on 'exposed-modules' or "
        ++ "'other-modules'."

  ]

  where
    checkVersion :: [Int] -> Bool -> PackageCheck -> Maybe PackageCheck
    checkVersion ver cond pc
      | specVersion pkg >= mkVersion ver       = Nothing
      | otherwise                              = check cond pc

    -- TODO: not sure if this check is always right in Backpack
    moduleDuplicates = dups (explicitLibModules lib ++
                             map moduleReexportName (reexportedModules lib))

checkExecutable :: PackageDescription -> Executable -> [PackageCheck]
checkExecutable pkg exe =
  catMaybes [

    check (null (modulePath exe)) $
      PackageBuildImpossible $
        "No 'main-is' field found for executable " ++ display (exeName exe)

  , check (not (null (modulePath exe))
       && (not $ fileExtensionSupportedLanguage $ modulePath exe)) $
      PackageBuildImpossible $
           "The 'main-is' field must specify a '.hs' or '.lhs' file "
        ++ "(even if it is generated by a preprocessor), "
        ++ "or it may specify a C/C++/obj-C source file."

  , checkSpecVersion pkg [1,17]
          (fileExtensionSupportedLanguage (modulePath exe)
        && takeExtension (modulePath exe) `notElem` [".hs", ".lhs"]) $
      PackageDistInexcusable $
           "The package uses a C/C++/obj-C source file for the 'main-is' field. "
        ++ "To use this feature you must specify 'cabal-version: >= 1.18'."

  , check (not (null moduleDuplicates)) $
       PackageBuildImpossible $
            "Duplicate modules in executable '" ++ display (exeName exe) ++ "': "
         ++ commaSep (map display moduleDuplicates)

    -- check that all autogen-modules appear on other-modules
  , check
      (not $ and $ map (flip elem (exeModules exe)) (exeModulesAutogen exe)) $
      PackageBuildImpossible $
           "On executable '" ++ display (exeName exe) ++ "' an 'autogen-module' is not "
        ++ "on 'other-modules'"

  , checkSpecVersion pkg [2,0] (exeScope exe /= ExecutableScopeUnknown) $
      PackageDistSuspiciousWarn $
           "To use the 'scope' field the package needs to specify "
        ++ "at least 'cabal-version: >= 2.0'."

  ]
  where
    moduleDuplicates = dups (exeModules exe)

checkTestSuite :: PackageDescription -> TestSuite -> [PackageCheck]
checkTestSuite pkg test =
  catMaybes [

    case testInterface test of
      TestSuiteUnsupported tt@(TestTypeUnknown _ _) -> Just $
        PackageBuildWarning $
             quote (display tt) ++ " is not a known type of test suite. "
          ++ "The known test suite types are: "
          ++ commaSep (map display knownTestTypes)

      TestSuiteUnsupported tt -> Just $
        PackageBuildWarning $
             quote (display tt) ++ " is not a supported test suite version. "
          ++ "The known test suite types are: "
          ++ commaSep (map display knownTestTypes)
      _ -> Nothing

  , check (not $ null moduleDuplicates) $
      PackageBuildImpossible $
           "Duplicate modules in test suite '" ++ display (testName test) ++ "': "
        ++ commaSep (map display moduleDuplicates)

  , check mainIsWrongExt $
      PackageBuildImpossible $
           "The 'main-is' field must specify a '.hs' or '.lhs' file "
        ++ "(even if it is generated by a preprocessor), "
        ++ "or it may specify a C/C++/obj-C source file."

  , checkSpecVersion pkg [1,17] (mainIsNotHsExt && not mainIsWrongExt) $
      PackageDistInexcusable $
           "The package uses a C/C++/obj-C source file for the 'main-is' field. "
        ++ "To use this feature you must specify 'cabal-version: >= 1.18'."

    -- check that all autogen-modules appear on other-modules
  , check
      (not $ and $ map
        (flip elem (testModules test))
        (testModulesAutogen test)
      ) $
      PackageBuildImpossible $
           "On test suite '" ++ display (testName test) ++ "' an 'autogen-module' is not "
        ++ "on 'other-modules'"
  ]
  where
    moduleDuplicates = dups $ testModules test

    mainIsWrongExt = case testInterface test of
      TestSuiteExeV10 _ f -> not $ fileExtensionSupportedLanguage f
      _                   -> False

    mainIsNotHsExt = case testInterface test of
      TestSuiteExeV10 _ f -> takeExtension f `notElem` [".hs", ".lhs"]
      _                   -> False

checkBenchmark :: PackageDescription -> Benchmark -> [PackageCheck]
checkBenchmark _pkg bm =
  catMaybes [

    case benchmarkInterface bm of
      BenchmarkUnsupported tt@(BenchmarkTypeUnknown _ _) -> Just $
        PackageBuildWarning $
             quote (display tt) ++ " is not a known type of benchmark. "
          ++ "The known benchmark types are: "
          ++ commaSep (map display knownBenchmarkTypes)

      BenchmarkUnsupported tt -> Just $
        PackageBuildWarning $
             quote (display tt) ++ " is not a supported benchmark version. "
          ++ "The known benchmark types are: "
          ++ commaSep (map display knownBenchmarkTypes)
      _ -> Nothing

  , check (not $ null moduleDuplicates) $
      PackageBuildImpossible $
           "Duplicate modules in benchmark '" ++ display (benchmarkName bm) ++ "': "
        ++ commaSep (map display moduleDuplicates)

  , check mainIsWrongExt $
      PackageBuildImpossible $
           "The 'main-is' field must specify a '.hs' or '.lhs' file "
        ++ "(even if it is generated by a preprocessor)."

    -- check that all autogen-modules appear on other-modules
  , check
      (not $ and $ map
        (flip elem (benchmarkModules bm))
        (benchmarkModulesAutogen bm)
      ) $
      PackageBuildImpossible $
             "On benchmark '" ++ display (benchmarkName bm) ++ "' an 'autogen-module' is "
          ++ "not on 'other-modules'"
  ]
  where
    moduleDuplicates = dups $ benchmarkModules bm

    mainIsWrongExt = case benchmarkInterface bm of
      BenchmarkExeV10 _ f -> takeExtension f `notElem` [".hs", ".lhs"]
      _                   -> False

-- ------------------------------------------------------------
-- * Additional pure checks
-- ------------------------------------------------------------

checkFields :: PackageDescription -> [PackageCheck]
checkFields pkg =
  catMaybes [

    check (not . FilePath.Windows.isValid . display . packageName $ pkg) $
      PackageDistInexcusable $
           "Unfortunately, the package name '" ++ display (packageName pkg)
        ++ "' is one of the reserved system file names on Windows. Many tools "
        ++ "need to convert package names to file names so using this name "
        ++ "would cause problems."

  , check ((isPrefixOf "z-") . display . packageName $ pkg) $
      PackageDistInexcusable $
           "Package names with the prefix 'z-' are reserved by Cabal and "
        ++ "cannot be used."

  , check (isNothing (buildType pkg)) $
      PackageBuildWarning $
           "No 'build-type' specified. If you do not need a custom Setup.hs or "
        ++ "./configure script then use 'build-type: Simple'."

  , case buildType pkg of
      Just (UnknownBuildType unknown) -> Just $
        PackageBuildWarning $
             quote unknown ++ " is not a known 'build-type'. "
          ++ "The known build types are: "
          ++ commaSep (map display knownBuildTypes)
      _ -> Nothing

  , check (isJust (setupBuildInfo pkg) && buildType pkg /= Just Custom) $
      PackageBuildWarning $
           "Ignoring the 'custom-setup' section because the 'build-type' is "
        ++ "not 'Custom'. Use 'build-type: Custom' if you need to use a "
        ++ "custom Setup.hs script."

  , check (not (null unknownCompilers)) $
      PackageBuildWarning $
        "Unknown compiler " ++ commaSep (map quote unknownCompilers)
                            ++ " in 'tested-with' field."

  , check (not (null unknownLanguages)) $
      PackageBuildWarning $
        "Unknown languages: " ++ commaSep unknownLanguages

  , check (not (null unknownExtensions)) $
      PackageBuildWarning $
        "Unknown extensions: " ++ commaSep unknownExtensions

  , check (not (null languagesUsedAsExtensions)) $
      PackageBuildWarning $
           "Languages listed as extensions: "
        ++ commaSep languagesUsedAsExtensions
        ++ ". Languages must be specified in either the 'default-language' "
        ++ " or the 'other-languages' field."

  , check (not (null ourDeprecatedExtensions)) $
      PackageDistSuspicious $
           "Deprecated extensions: "
        ++ commaSep (map (quote . display . fst) ourDeprecatedExtensions)
        ++ ". " ++ unwords
             [ "Instead of '" ++ display ext
            ++ "' use '" ++ display replacement ++ "'."
             | (ext, Just replacement) <- ourDeprecatedExtensions ]

  , check (null (category pkg)) $
      PackageDistSuspicious "No 'category' field."

  , check (null (maintainer pkg)) $
      PackageDistSuspicious "No 'maintainer' field."

  , check (null (synopsis pkg) && null (description pkg)) $
      PackageDistInexcusable "No 'synopsis' or 'description' field."

  , check (null (description pkg) && not (null (synopsis pkg))) $
      PackageDistSuspicious "No 'description' field."

  , check (null (synopsis pkg) && not (null (description pkg))) $
      PackageDistSuspicious "No 'synopsis' field."

    --TODO: recommend the bug reports URL, author and homepage fields
    --TODO: recommend not using the stability field
    --TODO: recommend specifying a source repo

  , check (length (synopsis pkg) >= 80) $
      PackageDistSuspicious
        "The 'synopsis' field is rather long (max 80 chars is recommended)."

    -- See also https://github.com/haskell/cabal/pull/3479
  , check (not (null (description pkg))
           && length (description pkg) <= length (synopsis pkg)) $
      PackageDistSuspicious $
           "The 'description' field should be longer than the 'synopsis' "
        ++ "field. "
        ++ "It's useful to provide an informative 'description' to allow "
        ++ "Haskell programmers who have never heard about your package to "
        ++ "understand the purpose of your package. "
        ++ "The 'description' field content is typically shown by tooling "
        ++ "(e.g. 'cabal info', Haddock, Hackage) below the 'synopsis' which "
        ++ "serves as a headline. "
        ++ "Please refer to <https://www.haskell.org/"
        ++ "cabal/users-guide/developing-packages.html#package-properties>"
        ++ " for more details."

    -- check use of impossible constraints "tested-with: GHC== 6.10 && ==6.12"
  , check (not (null testedWithImpossibleRanges)) $
      PackageDistInexcusable $
           "Invalid 'tested-with' version range: "
        ++ commaSep (map display testedWithImpossibleRanges)
        ++ ". To indicate that you have tested a package with multiple "
        ++ "different versions of the same compiler use multiple entries, "
        ++ "for example 'tested-with: GHC==6.10.4, GHC==6.12.3' and not "
        ++ "'tested-with: GHC==6.10.4 && ==6.12.3'."

  , check (not (null depInternalLibraryWithExtraVersion)) $
      PackageBuildWarning $
           "The package has an extraneous version range for a dependency on an "
        ++ "internal library: "
        ++ commaSep (map display depInternalLibraryWithExtraVersion)
        ++ ". This version range includes the current package but isn't needed "
        ++ "as the current package's library will always be used."

  , check (not (null depInternalLibraryWithImpossibleVersion)) $
      PackageBuildImpossible $
           "The package has an impossible version range for a dependency on an "
        ++ "internal library: "
        ++ commaSep (map display depInternalLibraryWithImpossibleVersion)
        ++ ". This version range does not include the current package, and must "
        ++ "be removed as the current package's library will always be used."

  , check (not (null depInternalExecutableWithExtraVersion)) $
      PackageBuildWarning $
           "The package has an extraneous version range for a dependency on an "
        ++ "internal executable: "
        ++ commaSep (map display depInternalExecutableWithExtraVersion)
        ++ ". This version range includes the current package but isn't needed "
        ++ "as the current package's executable will always be used."

  , check (not (null depInternalExecutableWithImpossibleVersion)) $
      PackageBuildImpossible $
           "The package has an impossible version range for a dependency on an "
        ++ "internal executable: "
        ++ commaSep (map display depInternalExecutableWithImpossibleVersion)
        ++ ". This version range does not include the current package, and must "
        ++ "be removed as the current package's executable will always be used."

  , check (not (null depMissingInternalExecutable)) $
      PackageBuildImpossible $
           "The package depends on a missing internal executable: "
        ++ commaSep (map display depInternalExecutableWithImpossibleVersion)
  ]
  where
    unknownCompilers  = [ name | (OtherCompiler name, _) <- testedWith pkg ]
    unknownLanguages  = [ name | bi <- allBuildInfo pkg
                               , UnknownLanguage name <- allLanguages bi ]
    unknownExtensions = [ name | bi <- allBuildInfo pkg
                               , UnknownExtension name <- allExtensions bi
                               , name `notElem` map display knownLanguages ]
    ourDeprecatedExtensions = nub $ catMaybes
      [ find ((==ext) . fst) deprecatedExtensions
      | bi <- allBuildInfo pkg
      , ext <- allExtensions bi ]
    languagesUsedAsExtensions =
      [ name | bi <- allBuildInfo pkg
             , UnknownExtension name <- allExtensions bi
             , name `elem` map display knownLanguages ]

    testedWithImpossibleRanges =
      [ Dependency (mkPackageName (display compiler)) vr
      | (compiler, vr) <- testedWith pkg
      , isNoVersion vr ]

    internalLibraries =
        map (maybe (packageName pkg) (unqualComponentNameToPackageName) . libName)
            (allLibraries pkg)

    internalExecutables = map exeName $ executables pkg

    internalLibDeps =
      [ dep
      | bi <- allBuildInfo pkg
      , dep@(Dependency name _) <- targetBuildDepends bi
      , name `elem` internalLibraries
      ]

    internalExeDeps =
      [ dep
      | bi <- allBuildInfo pkg
      , dep <- getAllToolDependencies pkg bi
      , isInternal pkg dep
      ]

    depInternalLibraryWithExtraVersion =
      [ dep
      | dep@(Dependency _ versionRange) <- internalLibDeps
      , not $ isAnyVersion versionRange
      , packageVersion pkg `withinRange` versionRange
      ]

    depInternalLibraryWithImpossibleVersion =
      [ dep
      | dep@(Dependency _ versionRange) <- internalLibDeps
      , not $ packageVersion pkg `withinRange` versionRange
      ]

    depInternalExecutableWithExtraVersion =
      [ dep
      | dep@(ExeDependency _ _ versionRange) <- internalExeDeps
      , not $ isAnyVersion versionRange
      , packageVersion pkg `withinRange` versionRange
      ]

    depInternalExecutableWithImpossibleVersion =
      [ dep
      | dep@(ExeDependency _ _ versionRange) <- internalExeDeps
      , not $ packageVersion pkg `withinRange` versionRange
      ]

    depMissingInternalExecutable =
      [ dep
      | dep@(ExeDependency _ eName _) <- internalExeDeps
      , not $ eName `elem` internalExecutables
      ]


checkLicense :: PackageDescription -> [PackageCheck]
checkLicense pkg =
  catMaybes [

    check (license pkg == UnspecifiedLicense) $
      PackageDistInexcusable
        "The 'license' field is missing."

  , check (license pkg == AllRightsReserved) $
      PackageDistSuspicious
        "The 'license' is AllRightsReserved. Is that really what you want?"
  , case license pkg of
      UnknownLicense l -> Just $
        PackageBuildWarning $
             quote ("license: " ++ l) ++ " is not a recognised license. The "
          ++ "known licenses are: "
          ++ commaSep (map display knownLicenses)
      _ -> Nothing

  , check (license pkg == BSD4) $
      PackageDistSuspicious $
           "Using 'license: BSD4' is almost always a misunderstanding. 'BSD4' "
        ++ "refers to the old 4-clause BSD license with the advertising "
        ++ "clause. 'BSD3' refers the new 3-clause BSD license."

  , case unknownLicenseVersion (license pkg) of
      Just knownVersions -> Just $
        PackageDistSuspicious $
             "'license: " ++ display (license pkg) ++ "' is not a known "
          ++ "version of that license. The known versions are "
          ++ commaSep (map display knownVersions)
          ++ ". If this is not a mistake and you think it should be a known "
          ++ "version then please file a ticket."
      _ -> Nothing

  , check (license pkg `notElem` [ AllRightsReserved
                                 , UnspecifiedLicense, PublicDomain]
           -- AllRightsReserved and PublicDomain are not strictly
           -- licenses so don't need license files.
        && null (licenseFiles pkg)) $
      PackageDistSuspicious "A 'license-file' is not specified."
  ]
  where
    unknownLicenseVersion (GPL  (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where knownVersions = [ v' | GPL  (Just v') <- knownLicenses ]
    unknownLicenseVersion (LGPL (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where knownVersions = [ v' | LGPL (Just v') <- knownLicenses ]
    unknownLicenseVersion (AGPL (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where knownVersions = [ v' | AGPL (Just v') <- knownLicenses ]
    unknownLicenseVersion (Apache  (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where knownVersions = [ v' | Apache  (Just v') <- knownLicenses ]
    unknownLicenseVersion _ = Nothing

checkSourceRepos :: PackageDescription -> [PackageCheck]
checkSourceRepos pkg =
  catMaybes $ concat [[

    case repoKind repo of
      RepoKindUnknown kind -> Just $ PackageDistInexcusable $
        quote kind ++ " is not a recognised kind of source-repository. "
                   ++ "The repo kind is usually 'head' or 'this'"
      _ -> Nothing

  , check (isNothing (repoType repo)) $
      PackageDistInexcusable
        "The source-repository 'type' is a required field."

  , check (isNothing (repoLocation repo)) $
      PackageDistInexcusable
        "The source-repository 'location' is a required field."

  , check (repoType repo == Just CVS && isNothing (repoModule repo)) $
      PackageDistInexcusable
        "For a CVS source-repository, the 'module' is a required field."

  , check (repoKind repo == RepoThis && isNothing (repoTag repo)) $
      PackageDistInexcusable $
           "For the 'this' kind of source-repository, the 'tag' is a required "
        ++ "field. It should specify the tag corresponding to this version "
        ++ "or release of the package."

  , check (maybe False isAbsoluteOnAnyPlatform (repoSubdir repo)) $
      PackageDistInexcusable
        "The 'subdir' field of a source-repository must be a relative path."
  ]
  | repo <- sourceRepos pkg ]

--TODO: check location looks like a URL for some repo types.

checkGhcOptions :: PackageDescription -> [PackageCheck]
checkGhcOptions pkg =
  catMaybes [

    checkFlags ["-fasm"] $
      PackageDistInexcusable $
           "'ghc-options: -fasm' is unnecessary and will not work on CPU "
        ++ "architectures other than x86, x86-64, ppc or sparc."

  , checkFlags ["-fvia-C"] $
      PackageDistSuspicious $
           "'ghc-options: -fvia-C' is usually unnecessary. If your package "
        ++ "needs -via-C for correctness rather than performance then it "
        ++ "is using the FFI incorrectly and will probably not work with GHC "
        ++ "6.10 or later."

  , checkFlags ["-fhpc"] $
      PackageDistInexcusable $
           "'ghc-options: -fhpc' is not not necessary. Use the configure flag "
        ++ " --enable-coverage instead."

  , checkFlags ["-prof"] $
      PackageBuildWarning $
           "'ghc-options: -prof' is not necessary and will lead to problems "
        ++ "when used on a library. Use the configure flag "
        ++ "--enable-library-profiling and/or --enable-profiling."

  , checkFlags ["-o"] $
      PackageBuildWarning $
           "'ghc-options: -o' is not needed. "
        ++ "The output files are named automatically."

  , checkFlags ["-hide-package"] $
      PackageBuildWarning $
      "'ghc-options: -hide-package' is never needed. "
      ++ "Cabal hides all packages."

  , checkFlags ["--make"] $
      PackageBuildWarning $
      "'ghc-options: --make' is never needed. Cabal uses this automatically."

  , checkFlags ["-main-is"] $
      PackageDistSuspicious $
      "'ghc-options: -main-is' is not portable."

  , checkFlags ["-O0", "-Onot"] $
      PackageDistSuspicious $
      "'ghc-options: -O0' is not needed. "
      ++ "Use the --disable-optimization configure flag."

  , checkFlags [ "-O", "-O1"] $
      PackageDistInexcusable $
      "'ghc-options: -O' is not needed. "
      ++ "Cabal automatically adds the '-O' flag. "
      ++ "Setting it yourself interferes with the --disable-optimization flag."

  , checkFlags ["-O2"] $
      PackageDistSuspiciousWarn $
      "'ghc-options: -O2' is rarely needed. "
      ++ "Check that it is giving a real benefit "
      ++ "and not just imposing longer compile times on your users."

  , checkFlags ["-split-objs"] $
      PackageBuildWarning $
        "'ghc-options: -split-objs' is not needed. "
        ++ "Use the --enable-split-objs configure flag."

  , checkFlags ["-optl-Wl,-s", "-optl-s"] $
      PackageDistInexcusable $
           "'ghc-options: -optl-Wl,-s' is not needed and is not portable to all"
        ++ " operating systems. Cabal 1.4 and later automatically strip"
        ++ " executables. Cabal also has a flag --disable-executable-stripping"
        ++ " which is necessary when building packages for some Linux"
        ++ " distributions and using '-optl-Wl,-s' prevents that from working."

  , checkFlags ["-fglasgow-exts"] $
      PackageDistSuspicious $
        "Instead of 'ghc-options: -fglasgow-exts' it is preferable to use "
        ++ "the 'extensions' field."

  , check ("-threaded" `elem` lib_ghc_options) $
      PackageBuildWarning $
           "'ghc-options: -threaded' has no effect for libraries. It should "
        ++ "only be used for executables."

  , check ("-rtsopts" `elem` lib_ghc_options) $
      PackageBuildWarning $
           "'ghc-options: -rtsopts' has no effect for libraries. It should "
        ++ "only be used for executables."

  , check (any (\opt -> "-with-rtsopts" `isPrefixOf` opt) lib_ghc_options) $
      PackageBuildWarning $
           "'ghc-options: -with-rtsopts' has no effect for libraries. It "
        ++ "should only be used for executables."

  , checkAlternatives "ghc-options" "extensions"
      [ (flag, display extension) | flag <- all_ghc_options
                                  , Just extension <- [ghcExtension flag] ]

  , checkAlternatives "ghc-options" "extensions"
      [ (flag, extension) | flag@('-':'X':extension) <- all_ghc_options ]

  , checkAlternatives "ghc-options" "cpp-options" $
         [ (flag, flag) | flag@('-':'D':_) <- all_ghc_options ]
      ++ [ (flag, flag) | flag@('-':'U':_) <- all_ghc_options ]

  , checkAlternatives "ghc-options" "include-dirs"
      [ (flag, dir) | flag@('-':'I':dir) <- all_ghc_options ]

  , checkAlternatives "ghc-options" "extra-libraries"
      [ (flag, lib) | flag@('-':'l':lib) <- all_ghc_options ]

  , checkAlternatives "ghc-options" "extra-lib-dirs"
      [ (flag, dir) | flag@('-':'L':dir) <- all_ghc_options ]

  , checkAlternatives "ghc-options" "frameworks"
      [ (flag, fmwk) | (flag@"-framework", fmwk) <-
           zip all_ghc_options (safeTail all_ghc_options) ]

  , checkAlternatives "ghc-options" "extra-framework-dirs"
      [ (flag, dir) | (flag@"-framework-path", dir) <-
           zip all_ghc_options (safeTail all_ghc_options) ]
  ]

  where
    all_ghc_options    = concatMap get_ghc_options (allBuildInfo pkg)
    lib_ghc_options    = concatMap (get_ghc_options . libBuildInfo)
                         (allLibraries pkg)
    get_ghc_options bi = hcOptions GHC bi ++ hcProfOptions GHC bi
                         ++ hcSharedOptions GHC bi

    checkFlags :: [String] -> PackageCheck -> Maybe PackageCheck
    checkFlags flags = check (any (`elem` flags) all_ghc_options)

    ghcExtension ('-':'f':name) = case name of
      "allow-overlapping-instances"    -> enable  OverlappingInstances
      "no-allow-overlapping-instances" -> disable OverlappingInstances
      "th"                             -> enable  TemplateHaskell
      "no-th"                          -> disable TemplateHaskell
      "ffi"                            -> enable  ForeignFunctionInterface
      "no-ffi"                         -> disable ForeignFunctionInterface
      "fi"                             -> enable  ForeignFunctionInterface
      "no-fi"                          -> disable ForeignFunctionInterface
      "monomorphism-restriction"       -> enable  MonomorphismRestriction
      "no-monomorphism-restriction"    -> disable MonomorphismRestriction
      "mono-pat-binds"                 -> enable  MonoPatBinds
      "no-mono-pat-binds"              -> disable MonoPatBinds
      "allow-undecidable-instances"    -> enable  UndecidableInstances
      "no-allow-undecidable-instances" -> disable UndecidableInstances
      "allow-incoherent-instances"     -> enable  IncoherentInstances
      "no-allow-incoherent-instances"  -> disable IncoherentInstances
      "arrows"                         -> enable  Arrows
      "no-arrows"                      -> disable Arrows
      "generics"                       -> enable  Generics
      "no-generics"                    -> disable Generics
      "implicit-prelude"               -> enable  ImplicitPrelude
      "no-implicit-prelude"            -> disable ImplicitPrelude
      "implicit-params"                -> enable  ImplicitParams
      "no-implicit-params"             -> disable ImplicitParams
      "bang-patterns"                  -> enable  BangPatterns
      "no-bang-patterns"               -> disable BangPatterns
      "scoped-type-variables"          -> enable  ScopedTypeVariables
      "no-scoped-type-variables"       -> disable ScopedTypeVariables
      "extended-default-rules"         -> enable  ExtendedDefaultRules
      "no-extended-default-rules"      -> disable ExtendedDefaultRules
      _                                -> Nothing
    ghcExtension "-cpp"             = enable CPP
    ghcExtension _                  = Nothing

    enable  e = Just (EnableExtension e)
    disable e = Just (DisableExtension e)

checkCCOptions :: PackageDescription -> [PackageCheck]
checkCCOptions pkg =
  catMaybes [

    checkAlternatives "cc-options" "include-dirs"
      [ (flag, dir) | flag@('-':'I':dir) <- all_ccOptions ]

  , checkAlternatives "cc-options" "extra-libraries"
      [ (flag, lib) | flag@('-':'l':lib) <- all_ccOptions ]

  , checkAlternatives "cc-options" "extra-lib-dirs"
      [ (flag, dir) | flag@('-':'L':dir) <- all_ccOptions ]

  , checkAlternatives "ld-options" "extra-libraries"
      [ (flag, lib) | flag@('-':'l':lib) <- all_ldOptions ]

  , checkAlternatives "ld-options" "extra-lib-dirs"
      [ (flag, dir) | flag@('-':'L':dir) <- all_ldOptions ]

  , checkCCFlags [ "-O", "-Os", "-O0", "-O1", "-O2", "-O3" ] $
      PackageDistSuspicious $
           "'cc-options: -O[n]' is generally not needed. When building with "
        ++ " optimisations Cabal automatically adds '-O2' for C code. "
        ++ "Setting it yourself interferes with the --disable-optimization "
        ++ "flag."
  ]

  where all_ccOptions = [ opts | bi <- allBuildInfo pkg
                              , opts <- ccOptions bi ]
        all_ldOptions = [ opts | bi <- allBuildInfo pkg
                               , opts <- ldOptions bi ]

        checkCCFlags :: [String] -> PackageCheck -> Maybe PackageCheck
        checkCCFlags flags = check (any (`elem` flags) all_ccOptions)

checkCPPOptions :: PackageDescription -> [PackageCheck]
checkCPPOptions pkg =
  catMaybes [
    checkAlternatives "cpp-options" "include-dirs"
      [ (flag, dir) | flag@('-':'I':dir) <- all_cppOptions]
    ]
  where all_cppOptions = [ opts | bi <- allBuildInfo pkg
                                , opts <- cppOptions bi ]

checkAlternatives :: String -> String -> [(String, String)]
                  -> Maybe PackageCheck
checkAlternatives badField goodField flags =
  check (not (null badFlags)) $
    PackageBuildWarning $
         "Instead of " ++ quote (badField ++ ": " ++ unwords badFlags)
      ++ " use " ++ quote (goodField ++ ": " ++ unwords goodFlags)

  where (badFlags, goodFlags) = unzip flags

checkPaths :: PackageDescription -> [PackageCheck]
checkPaths pkg =
  [ PackageBuildWarning $
         quote (kind ++ ": " ++ path)
      ++ " is a relative path outside of the source tree. "
      ++ "This will not work when generating a tarball with 'sdist'."
  | (path, kind) <- relPaths ++ absPaths
  , isOutsideTree path ]
  ++
  [ PackageDistInexcusable $
      quote (kind ++ ": " ++ path) ++ " is an absolute path."
  | (path, kind) <- relPaths
  , isAbsoluteOnAnyPlatform path ]
  ++
  [ PackageDistInexcusable $
         quote (kind ++ ": " ++ path) ++ " points inside the 'dist' "
      ++ "directory. This is not reliable because the location of this "
      ++ "directory is configurable by the user (or package manager). In "
      ++ "addition the layout of the 'dist' directory is subject to change "
      ++ "in future versions of Cabal."
  | (path, kind) <- relPaths ++ absPaths
  , isInsideDist path ]
  ++
  [ PackageDistInexcusable $
         "The 'ghc-options' contains the path '" ++ path ++ "' which points "
      ++ "inside the 'dist' directory. This is not reliable because the "
      ++ "location of this directory is configurable by the user (or package "
      ++ "manager). In addition the layout of the 'dist' directory is subject "
      ++ "to change in future versions of Cabal."
  | bi <- allBuildInfo pkg
  , (GHC, flags) <- options bi
  , path <- flags
  , isInsideDist path ]
  where
    isOutsideTree path = case splitDirectories path of
      "..":_     -> True
      ".":"..":_ -> True
      _          -> False
    isInsideDist path = case map lowercase (splitDirectories path) of
      "dist"    :_ -> True
      ".":"dist":_ -> True
      _            -> False
    -- paths that must be relative
    relPaths =
         [ (path, "extra-src-files") | path <- extraSrcFiles pkg ]
      ++ [ (path, "extra-tmp-files") | path <- extraTmpFiles pkg ]
      ++ [ (path, "extra-doc-files") | path <- extraDocFiles pkg ]
      ++ [ (path, "data-files")      | path <- dataFiles     pkg ]
      ++ [ (path, "data-dir")        | path <- [dataDir      pkg]]
      ++ [ (path, "license-file")    | path <- licenseFiles  pkg ]
      ++ concat
         [    [ (path, "asm-sources")      | path <- asmSources      bi ]
           ++ [ (path, "cmm-sources")      | path <- cmmSources      bi ]
           ++ [ (path, "c-sources")        | path <- cSources        bi ]
           ++ [ (path, "cxx-sources")      | path <- cxxSources      bi ]
           ++ [ (path, "js-sources")       | path <- jsSources       bi ]
           ++ [ (path, "install-includes") | path <- installIncludes bi ]
           ++ [ (path, "hs-source-dirs")   | path <- hsSourceDirs    bi ]
         | bi <- allBuildInfo pkg ]
    -- paths that are allowed to be absolute
    absPaths = concat
      [    [ (path, "includes")         | path <- includes        bi ]
        ++ [ (path, "include-dirs")     | path <- includeDirs     bi ]
        ++ [ (path, "extra-lib-dirs")   | path <- extraLibDirs    bi ]
      | bi <- allBuildInfo pkg ]

--TODO: check sets of paths that would be interpreted differently between Unix
-- and windows, ie case-sensitive or insensitive. Things that might clash, or
-- conversely be distinguished.

--TODO: use the tar path checks on all the above paths

-- | Check that the package declares the version in the @\"cabal-version\"@
-- field correctly.
--
checkCabalVersion :: PackageDescription -> [PackageCheck]
checkCabalVersion pkg =
  catMaybes [

    -- check syntax of cabal-version field
    check (specVersion pkg >= mkVersion [1,10]
           && not simpleSpecVersionRangeSyntax) $
      PackageBuildWarning $
           "Packages relying on Cabal 1.10 or later must only specify a "
        ++ "version range of the form 'cabal-version: >= x.y'. Use "
        ++ "'cabal-version: >= " ++ display (specVersion pkg) ++ "'."

    -- check syntax of cabal-version field
  , check (specVersion pkg < mkVersion [1,9]
           && not simpleSpecVersionRangeSyntax) $
      PackageDistSuspicious $
           "It is recommended that the 'cabal-version' field only specify a "
        ++ "version range of the form '>= x.y'. Use "
        ++ "'cabal-version: >= " ++ display (specVersion pkg) ++ "'. "
        ++ "Tools based on Cabal 1.10 and later will ignore upper bounds."

    -- check syntax of cabal-version field
  , checkVersion [1,12] simpleSpecVersionSyntax $
      PackageBuildWarning $
           "With Cabal 1.10 or earlier, the 'cabal-version' field must use "
        ++ "range syntax rather than a simple version number. Use "
        ++ "'cabal-version: >= " ++ display (specVersion pkg) ++ "'."

    -- check use of test suite sections
  , checkVersion [1,8] (not (null $ testSuites pkg)) $
      PackageDistInexcusable $
           "The 'test-suite' section is new in Cabal 1.10. "
        ++ "Unfortunately it messes up the parser in older Cabal versions "
        ++ "so you must specify at least 'cabal-version: >= 1.8', but note "
        ++ "that only Cabal 1.10 and later can actually run such test suites."

    -- check use of default-language field
    -- note that we do not need to do an equivalent check for the
    -- other-language field since that one does not change behaviour
  , checkVersion [1,10] (any isJust (buildInfoField defaultLanguage)) $
      PackageBuildWarning $
           "To use the 'default-language' field the package needs to specify "
        ++ "at least 'cabal-version: >= 1.10'."

  , check (specVersion pkg >= mkVersion [1,10]
           && (any isNothing (buildInfoField defaultLanguage))) $
      PackageBuildWarning $
           "Packages using 'cabal-version: >= 1.10' must specify the "
        ++ "'default-language' field for each component (e.g. Haskell98 or "
        ++ "Haskell2010). If a component uses different languages in "
        ++ "different modules then list the other ones in the "
        ++ "'other-languages' field."

  , checkVersion [1,18]
    (not . null $ extraDocFiles pkg) $
      PackageDistInexcusable $
           "To use the 'extra-doc-files' field the package needs to specify "
        ++ "at least 'cabal-version: >= 1.18'."

  , checkVersion [1,23]
    (not (null (subLibraries pkg))) $
      PackageDistInexcusable $
           "To use multiple 'library' sections or a named library section "
        ++ "the package needs to specify at least 'cabal-version >= 1.23'."

    -- check use of reexported-modules sections
  , checkVersion [1,21]
    (any (not.null.reexportedModules) (allLibraries pkg)) $
      PackageDistInexcusable $
           "To use the 'reexported-module' field the package needs to specify "
        ++ "at least 'cabal-version: >= 1.21'."

    -- check use of thinning and renaming
  , checkVersion [1,25] usesBackpackIncludes $
      PackageDistInexcusable $
           "To use the 'mixins' field the package needs to specify "
        ++ "at least 'cabal-version: >= 1.25'."

    -- check use of 'extra-framework-dirs' field
  , checkVersion [1,23] (any (not . null) (buildInfoField extraFrameworkDirs)) $
      -- Just a warning, because this won't break on old Cabal versions.
      PackageDistSuspiciousWarn $
           "To use the 'extra-framework-dirs' field the package needs to specify"
        ++ " at least 'cabal-version: >= 1.23'."

    -- check use of default-extensions field
    -- don't need to do the equivalent check for other-extensions
  , checkVersion [1,10] (any (not . null) (buildInfoField defaultExtensions)) $
      PackageBuildWarning $
           "To use the 'default-extensions' field the package needs to specify "
        ++ "at least 'cabal-version: >= 1.10'."

    -- check use of extensions field
  , check (specVersion pkg >= mkVersion [1,10]
           && (any (not . null) (buildInfoField oldExtensions))) $
      PackageBuildWarning $
           "For packages using 'cabal-version: >= 1.10' the 'extensions' "
        ++ "field is deprecated. The new 'default-extensions' field lists "
        ++ "extensions that are used in all modules in the component, while "
        ++ "the 'other-extensions' field lists extensions that are used in "
        ++ "some modules, e.g. via the {-# LANGUAGE #-} pragma."

    -- check use of "foo (>= 1.0 && < 1.4) || >=1.8 " version-range syntax
  , checkVersion [1,8] (not (null versionRangeExpressions)) $
      PackageDistInexcusable $
           "The package uses full version-range expressions "
        ++ "in a 'build-depends' field: "
        ++ commaSep (map displayRawDependency versionRangeExpressions)
        ++ ". To use this new syntax the package needs to specify at least "
        ++ "'cabal-version: >= 1.8'. Alternatively, if broader compatibility "
        ++ "is important, then convert to conjunctive normal form, and use "
        ++ "multiple 'build-depends:' lines, one conjunct per line."

    -- check use of "build-depends: foo == 1.*" syntax
  , checkVersion [1,6] (not (null depsUsingWildcardSyntax)) $
      PackageDistInexcusable $
           "The package uses wildcard syntax in the 'build-depends' field: "
        ++ commaSep (map display depsUsingWildcardSyntax)
        ++ ". To use this new syntax the package need to specify at least "
        ++ "'cabal-version: >= 1.6'. Alternatively, if broader compatibility "
        ++ "is important then use: " ++ commaSep
           [ display (Dependency name (eliminateWildcardSyntax versionRange))
           | Dependency name versionRange <- depsUsingWildcardSyntax ]

    -- check use of "build-depends: foo ^>= 1.2.3" syntax
  , checkVersion [2,0] (not (null depsUsingMajorBoundSyntax)) $
      PackageDistInexcusable $
           "The package uses major bounded version syntax in the "
        ++ "'build-depends' field: "
        ++ commaSep (map display depsUsingMajorBoundSyntax)
        ++ ". To use this new syntax the package need to specify at least "
        ++ "'cabal-version: >= 2.0'. Alternatively, if broader compatibility "
        ++ "is important then use: " ++ commaSep
           [ display (Dependency name (eliminateMajorBoundSyntax versionRange))
           | Dependency name versionRange <- depsUsingMajorBoundSyntax ]

  , checkVersion [2,1] (any (not . null)
                        (concatMap buildInfoField
                         [ asmSources
                         , cmmSources
                         , extraBundledLibs
                         , extraLibFlavours ])) $
      PackageDistInexcusable $
           "The use of 'asm-sources', 'cmm-sources', 'extra-bundled-libraries' "
        ++ " and 'extra-library-flavours' requires the package "
        ++ " to specify at least 'cabal-version: >= 2.1'."

  , checkVersion [2,1] (any (not . null)
                        (buildInfoField virtualModules)) $
      PackageDistInexcusable $
           "The use of 'virtual-modules' requires the package "
        ++ " to specify at least 'cabal-version: >= 2.1'."
 
    -- check use of "tested-with: GHC (>= 1.0 && < 1.4) || >=1.8 " syntax
  , checkVersion [1,8] (not (null testedWithVersionRangeExpressions)) $
      PackageDistInexcusable $
           "The package uses full version-range expressions "
        ++ "in a 'tested-with' field: "
        ++ commaSep (map displayRawDependency testedWithVersionRangeExpressions)
        ++ ". To use this new syntax the package needs to specify at least "
        ++ "'cabal-version: >= 1.8'."

    -- check use of "tested-with: GHC == 6.12.*" syntax
  , checkVersion [1,6] (not (null testedWithUsingWildcardSyntax)) $
      PackageDistInexcusable $
           "The package uses wildcard syntax in the 'tested-with' field: "
        ++ commaSep (map display testedWithUsingWildcardSyntax)
        ++ ". To use this new syntax the package need to specify at least "
        ++ "'cabal-version: >= 1.6'. Alternatively, if broader compatibility "
        ++ "is important then use: " ++ commaSep
           [ display (Dependency name (eliminateWildcardSyntax versionRange))
           | Dependency name versionRange <- testedWithUsingWildcardSyntax ]

    -- check use of "data-files: data/*.txt" syntax
  , checkVersion [1,6] (not (null dataFilesUsingGlobSyntax)) $
      PackageDistInexcusable $
           "Using wildcards like "
        ++ commaSep (map quote $ take 3 dataFilesUsingGlobSyntax)
        ++ " in the 'data-files' field requires 'cabal-version: >= 1.6'. "
        ++ "Alternatively if you require compatibility with earlier Cabal "
        ++ "versions then list all the files explicitly."

    -- check use of "extra-source-files: mk/*.in" syntax
  , checkVersion [1,6] (not (null extraSrcFilesUsingGlobSyntax)) $
      PackageDistInexcusable $
           "Using wildcards like "
        ++ commaSep (map quote $ take 3 extraSrcFilesUsingGlobSyntax)
        ++ " in the 'extra-source-files' field requires "
        ++ "'cabal-version: >= 1.6'. Alternatively if you require "
        ++ "compatibility with earlier Cabal versions then list all the files "
        ++ "explicitly."

    -- check use of "source-repository" section
  , checkVersion [1,6] (not (null (sourceRepos pkg))) $
      PackageDistInexcusable $
           "The 'source-repository' section is new in Cabal 1.6. "
        ++ "Unfortunately it messes up the parser in earlier Cabal versions "
        ++ "so you need to specify 'cabal-version: >= 1.6'."

    -- check for new licenses
  , checkVersion [1,4] (license pkg `notElem` compatLicenses) $
      PackageDistInexcusable $
           "Unfortunately the license " ++ quote (display (license pkg))
        ++ " messes up the parser in earlier Cabal versions so you need to "
        ++ "specify 'cabal-version: >= 1.4'. Alternatively if you require "
        ++ "compatibility with earlier Cabal versions then use 'OtherLicense'."

    -- check for new language extensions
  , checkVersion [1,2,3] (not (null mentionedExtensionsThatNeedCabal12)) $
      PackageDistInexcusable $
           "Unfortunately the language extensions "
        ++ commaSep (map (quote . display) mentionedExtensionsThatNeedCabal12)
        ++ " break the parser in earlier Cabal versions so you need to "
        ++ "specify 'cabal-version: >= 1.2.3'. Alternatively if you require "
        ++ "compatibility with earlier Cabal versions then you may be able to "
        ++ "use an equivalent compiler-specific flag."

  , checkVersion [1,4] (not (null mentionedExtensionsThatNeedCabal14)) $
      PackageDistInexcusable $
           "Unfortunately the language extensions "
        ++ commaSep (map (quote . display) mentionedExtensionsThatNeedCabal14)
        ++ " break the parser in earlier Cabal versions so you need to "
        ++ "specify 'cabal-version: >= 1.4'. Alternatively if you require "
        ++ "compatibility with earlier Cabal versions then you may be able to "
        ++ "use an equivalent compiler-specific flag."

  , check (specVersion pkg >= mkVersion [1,23]
           && isNothing (setupBuildInfo pkg)
           && buildType pkg == Just Custom) $
      PackageBuildWarning $
           "Packages using 'cabal-version: >= 1.23' with 'build-type: Custom' "
        ++ "must use a 'custom-setup' section with a 'setup-depends' field "
        ++ "that specifies the dependencies of the Setup.hs script itself. "
        ++ "The 'setup-depends' field uses the same syntax as 'build-depends', "
        ++ "so a simple example would be 'setup-depends: base, Cabal'."

  , check (specVersion pkg < mkVersion [1,23]
           && isNothing (setupBuildInfo pkg)
           && buildType pkg == Just Custom) $
      PackageDistSuspiciousWarn $
           "From version 1.23 cabal supports specifiying explicit dependencies "
        ++ "for Custom setup scripts. Consider using cabal-version >= 1.23 and "
        ++ "adding a 'custom-setup' section with a 'setup-depends' field "
        ++ "that specifies the dependencies of the Setup.hs script itself. "
        ++ "The 'setup-depends' field uses the same syntax as 'build-depends', "
        ++ "so a simple example would be 'setup-depends: base, Cabal'."

  , check (specVersion pkg >= mkVersion [1,25]
           && elem (autogenPathsModuleName pkg) allModuleNames
           && not (elem (autogenPathsModuleName pkg) allModuleNamesAutogen) ) $
      PackageDistInexcusable $
           "Packages using 'cabal-version: >= 1.25' and the autogenerated "
        ++ "module Paths_* must include it also on the 'autogen-modules' field "
        ++ "besides 'exposed-modules' and 'other-modules'. This specifies that "
        ++ "the module does not come with the package and is generated on "
        ++ "setup. Modules built with a custom Setup.hs script also go here "
        ++ "to ensure that commands like sdist don't fail."

  ]
  where
    -- Perform a check on packages that use a version of the spec less than
    -- the version given. This is for cases where a new Cabal version adds
    -- a new feature and we want to check that it is not used prior to that
    -- version.
    checkVersion :: [Int] -> Bool -> PackageCheck -> Maybe PackageCheck
    checkVersion ver cond pc
      | specVersion pkg >= mkVersion ver       = Nothing
      | otherwise                              = check cond pc

    buildInfoField field         = map field (allBuildInfo pkg)
    dataFilesUsingGlobSyntax     = filter usesGlobSyntax (dataFiles pkg)
    extraSrcFilesUsingGlobSyntax = filter usesGlobSyntax (extraSrcFiles pkg)
    usesGlobSyntax str = case parseFileGlob str of
      Just (FileGlob _ _) -> True
      _                   -> False

    versionRangeExpressions =
        [ dep | dep@(Dependency _ vr) <- buildDepends pkg
              , usesNewVersionRangeSyntax vr ]

    testedWithVersionRangeExpressions =
        [ Dependency (mkPackageName (display compiler)) vr
        | (compiler, vr) <- testedWith pkg
        , usesNewVersionRangeSyntax vr ]

    simpleSpecVersionRangeSyntax =
        either (const True) (cataVersionRange alg) (specVersionRaw pkg)
      where
        alg (OrLaterVersionF _) = True
        alg _                   = False

    -- is the cabal-version field a simple version number, rather than a range
    simpleSpecVersionSyntax =
      either (const True) (const False) (specVersionRaw pkg)

    usesNewVersionRangeSyntax :: VersionRange -> Bool
    usesNewVersionRangeSyntax
        = (> 2) -- uses the new syntax if depth is more than 2
        . cataVersionRange alg
      where
        alg (UnionVersionRangesF a b) = a + b
        alg (IntersectVersionRangesF a b) = a + b
        alg (VersionRangeParensF _) = 3
        alg _ = 1 :: Int

    depsUsingWildcardSyntax = [ dep | dep@(Dependency _ vr) <- buildDepends pkg
                                    , usesWildcardSyntax vr ]

    depsUsingMajorBoundSyntax = [ dep | dep@(Dependency _ vr) <- buildDepends pkg
                                  , usesMajorBoundSyntax vr ]

    usesBackpackIncludes = any (not . null . mixins) (allBuildInfo pkg)

    testedWithUsingWildcardSyntax =
      [ Dependency (mkPackageName (display compiler)) vr
      | (compiler, vr) <- testedWith pkg
      , usesWildcardSyntax vr ]

    usesWildcardSyntax :: VersionRange -> Bool
    usesWildcardSyntax = cataVersionRange alg
      where
        alg (WildcardVersionF _)          = True
        alg (UnionVersionRangesF a b)     = a || b
        alg (IntersectVersionRangesF a b) = a || b
        alg (VersionRangeParensF a)       = a
        alg _                             = False

    -- NB: this eliminates both, WildcardVersion and MajorBoundVersion
    -- because when WildcardVersion is not support, neither is MajorBoundVersion
    eliminateWildcardSyntax = hyloVersionRange embed projectVersionRange
      where
        embed (WildcardVersionF v) = intersectVersionRanges
            (orLaterVersion v) (earlierVersion (wildcardUpperBound v))
        embed (MajorBoundVersionF v) = intersectVersionRanges
            (orLaterVersion v) (earlierVersion (majorUpperBound v))
        embed vr = embedVersionRange vr

    usesMajorBoundSyntax :: VersionRange -> Bool
    usesMajorBoundSyntax = cataVersionRange alg
      where
        alg (MajorBoundVersionF _)        = True
        alg (UnionVersionRangesF a b)     = a || b
        alg (IntersectVersionRangesF a b) = a || b
        alg (VersionRangeParensF a)       = a
        alg _                             = False

    eliminateMajorBoundSyntax = hyloVersionRange embed projectVersionRange
      where
        embed (MajorBoundVersionF v) = intersectVersionRanges
            (orLaterVersion v) (earlierVersion (majorUpperBound v))
        embed vr = embedVersionRange vr

    compatLicenses = [ GPL Nothing, LGPL Nothing, AGPL Nothing, BSD3, BSD4
                     , PublicDomain, AllRightsReserved
                     , UnspecifiedLicense, OtherLicense ]

    mentionedExtensions = [ ext | bi <- allBuildInfo pkg
                                , ext <- allExtensions bi ]
    mentionedExtensionsThatNeedCabal12 =
      nub (filter (`elem` compatExtensionsExtra) mentionedExtensions)

    -- As of Cabal-1.4 we can add new extensions without worrying about
    -- breaking old versions of cabal.
    mentionedExtensionsThatNeedCabal14 =
      nub (filter (`notElem` compatExtensions) mentionedExtensions)

    -- The known extensions in Cabal-1.2.3
    compatExtensions =
      map EnableExtension
      [ OverlappingInstances, UndecidableInstances, IncoherentInstances
      , RecursiveDo, ParallelListComp, MultiParamTypeClasses
      , FunctionalDependencies, Rank2Types
      , RankNTypes, PolymorphicComponents, ExistentialQuantification
      , ScopedTypeVariables, ImplicitParams, FlexibleContexts
      , FlexibleInstances, EmptyDataDecls, CPP, BangPatterns
      , TypeSynonymInstances, TemplateHaskell, ForeignFunctionInterface
      , Arrows, Generics, NamedFieldPuns, PatternGuards
      , GeneralizedNewtypeDeriving, ExtensibleRecords, RestrictedTypeSynonyms
      , HereDocuments] ++
      map DisableExtension
      [MonomorphismRestriction, ImplicitPrelude] ++
      compatExtensionsExtra

    -- The extra known extensions in Cabal-1.2.3 vs Cabal-1.1.6
    -- (Cabal-1.1.6 came with ghc-6.6. Cabal-1.2 came with ghc-6.8)
    compatExtensionsExtra =
      map EnableExtension
      [ KindSignatures, MagicHash, TypeFamilies, StandaloneDeriving
      , UnicodeSyntax, PatternSignatures, UnliftedFFITypes, LiberalTypeSynonyms
      , TypeOperators, RecordWildCards, RecordPuns, DisambiguateRecordFields
      , OverloadedStrings, GADTs, RelaxedPolyRec
      , ExtendedDefaultRules, UnboxedTuples, DeriveDataTypeable
      , ConstrainedClassMethods
      ] ++
      map DisableExtension
      [MonoPatBinds]

    allModuleNames =
         (case library pkg of
           Nothing -> []
           (Just lib) -> explicitLibModules lib
         )
      ++ concatMap otherModules (allBuildInfo pkg)

    allModuleNamesAutogen = concatMap autogenModules (allBuildInfo pkg)

displayRawDependency :: Dependency -> String
displayRawDependency (Dependency pkg vr) =
  display pkg ++ " " ++ display vr


-- ------------------------------------------------------------
-- * Checks on the GenericPackageDescription
-- ------------------------------------------------------------

-- | Check the build-depends fields for any weirdness or bad practise.
--
checkPackageVersions :: GenericPackageDescription -> [PackageCheck]
checkPackageVersions pkg =
  catMaybes [

    -- Check that the version of base is bounded above.
    -- For example this bans "build-depends: base >= 3".
    -- It should probably be "build-depends: base >= 3 && < 4"
    -- which is the same as  "build-depends: base == 3.*"
    check (not (boundedAbove baseDependency)) $
      PackageDistInexcusable $
           "The dependency 'build-depends: base' does not specify an upper "
        ++ "bound on the version number. Each major release of the 'base' "
        ++ "package changes the API in various ways and most packages will "
        ++ "need some changes to compile with it. The recommended practise "
        ++ "is to specify an upper bound on the version of the 'base' "
        ++ "package. This ensures your package will continue to build when a "
        ++ "new major version of the 'base' package is released. If you are "
        ++ "not sure what upper bound to use then use the next  major "
        ++ "version. For example if you have tested your package with 'base' "
        ++ "version 4.5 and 4.6 then use 'build-depends: base >= 4.5 && < 4.7'."

  ]
  where
    -- TODO: What we really want to do is test if there exists any
    -- configuration in which the base version is unbounded above.
    -- However that's a bit tricky because there are many possible
    -- configurations. As a cheap easy and safe approximation we will
    -- pick a single "typical" configuration and check if that has an
    -- open upper bound. To get a typical configuration we finalise
    -- using no package index and the current platform.
    finalised = finalizePD
                              mempty defaultComponentRequestedSpec (const True)
                              buildPlatform
                              (unknownCompilerInfo
                                (CompilerId buildCompilerFlavor nullVersion)
                                NoAbiTag)
                              [] pkg
    baseDependency = case finalised of
      Right (pkg', _) | not (null baseDeps) ->
          foldr intersectVersionRanges anyVersion baseDeps
        where
          baseDeps =
            [ vr | Dependency pname vr <- buildDepends pkg'
                 , pname == mkPackageName "base" ]

      -- Just in case finalizePD fails for any reason,
      -- or if the package doesn't depend on the base package at all,
      -- then we will just skip the check, since boundedAbove noVersion = True
      _          -> noVersion

    boundedAbove :: VersionRange -> Bool
    boundedAbove vr = case asVersionIntervals vr of
      []        -> True -- this is the inconsistent version range.
      intervals -> case last intervals of
        (_,   UpperBound _ _) -> True
        (_, NoUpperBound    ) -> False


checkConditionals :: GenericPackageDescription -> [PackageCheck]
checkConditionals pkg =
  catMaybes [

    check (not $ null unknownOSs) $
      PackageDistInexcusable $
           "Unknown operating system name "
        ++ commaSep (map quote unknownOSs)

  , check (not $ null unknownArches) $
      PackageDistInexcusable $
           "Unknown architecture name "
        ++ commaSep (map quote unknownArches)

  , check (not $ null unknownImpls) $
      PackageDistInexcusable $
           "Unknown compiler name "
        ++ commaSep (map quote unknownImpls)
  ]
  where
    unknownOSs    = [ os   | OS   (OtherOS os)           <- conditions ]
    unknownArches = [ arch | Arch (OtherArch arch)       <- conditions ]
    unknownImpls  = [ impl | Impl (OtherCompiler impl) _ <- conditions ]
    conditions = concatMap fvs (maybeToList (condLibrary pkg))
              ++ concatMap (fvs . snd) (condSubLibraries pkg)
              ++ concatMap (fvs . snd) (condForeignLibs pkg)
              ++ concatMap (fvs . snd) (condExecutables pkg)
              ++ concatMap (fvs . snd) (condTestSuites pkg)
              ++ concatMap (fvs . snd) (condBenchmarks pkg)
    fvs (CondNode _ _ ifs) = concatMap compfv ifs -- free variables
    compfv (CondBranch c ct mct) = condfv c ++ fvs ct ++ maybe [] fvs mct
    condfv c = case c of
      Var v      -> [v]
      Lit _      -> []
      CNot c1    -> condfv c1
      COr  c1 c2 -> condfv c1 ++ condfv c2
      CAnd c1 c2 -> condfv c1 ++ condfv c2

checkFlagNames :: GenericPackageDescription -> [PackageCheck]
checkFlagNames gpd
    | null invalidFlagNames = []
    | otherwise             = [ PackageDistInexcusable
        $ "Suspicious flag names: " ++ unwords invalidFlagNames ++ ". "
        ++ "To avoid ambiguity in command line interfaces, flag shouldn't "
        ++ "start with a dash. Also for better compatibility, flag names "
        ++ "shouldn't contain non-ascii characters."
        ]
  where
    invalidFlagNames =
        [ fn
        | flag <- genPackageFlags gpd
        , let fn = unFlagName (flagName flag)
        , invalidFlagName fn
        ]
    -- starts with dash
    invalidFlagName ('-':_) = True
    -- mon ascii letter
    invalidFlagName cs = any (not . isAscii) cs

checkUnusedFlags :: GenericPackageDescription -> [PackageCheck]
checkUnusedFlags gpd
    | declared == used = []
    | otherwise        = [ PackageDistSuspicious
        $ "Declared and used flag sets differ: "
        ++ s declared ++ " /= " ++ s used ++ ". "
        ]
  where
    s :: Set.Set FlagName -> String
    s = commaSep . map unFlagName . Set.toList

    declared :: Set.Set FlagName
    declared = toSetOf (L.genPackageFlags . traverse . L.flagName) gpd

    used :: Set.Set FlagName
    used = mconcat
        [ toSetOf (L.condLibrary      . traverse      . traverseCondTreeV . L._Flag) gpd
        , toSetOf (L.condSubLibraries . traverse . _2 . traverseCondTreeV . L._Flag) gpd
        , toSetOf (L.condForeignLibs  . traverse . _2 . traverseCondTreeV . L._Flag) gpd
        , toSetOf (L.condExecutables  . traverse . _2 . traverseCondTreeV . L._Flag) gpd
        , toSetOf (L.condTestSuites   . traverse . _2 . traverseCondTreeV . L._Flag) gpd
        , toSetOf (L.condBenchmarks   . traverse . _2 . traverseCondTreeV . L._Flag) gpd
        ]

checkUnicodeXFields :: GenericPackageDescription -> [PackageCheck]
checkUnicodeXFields gpd
    | null nonAsciiXFields = []
    | otherwise            = [ PackageDistInexcusable
        $ "Non ascii custom fields: " ++ unwords nonAsciiXFields ++ ". "
        ++ "For better compatibility, custom field names "
        ++ "shouldn't contain non-ascii characters."
        ]
  where
    nonAsciiXFields :: [String]
    nonAsciiXFields = [ n | (n, _) <- xfields, any (not . isAscii) n ]

    xfields :: [(String,String)]
    xfields = DList.runDList $ mconcat
        [ toDListOf (L.packageDescription . L.customFieldsPD . traverse) gpd
        , toDListOf (L.buildInfos         . L.customFieldsBI . traverse) gpd
        ]

checkDevelopmentOnlyFlagsBuildInfo :: BuildInfo -> [PackageCheck]
checkDevelopmentOnlyFlagsBuildInfo bi =
  catMaybes [

    check has_WerrorWall $
      PackageDistInexcusable $
           "'ghc-options: -Wall -Werror' makes the package very easy to "
        ++ "break with future GHC versions because new GHC versions often "
        ++ "add new warnings. Use just 'ghc-options: -Wall' instead."
        ++ extraExplanation

  , check (not has_WerrorWall && has_Werror) $
      PackageDistInexcusable $
           "'ghc-options: -Werror' makes the package easy to "
        ++ "break with future GHC versions because new GHC versions often "
        ++ "add new warnings. "
        ++ extraExplanation

  , checkFlags ["-fdefer-type-errors"] $
      PackageDistInexcusable $
           "'ghc-options: -fdefer-type-errors' is fine during development but "
        ++ "is not appropriate for a distributed package. "
        ++ extraExplanation

    -- -dynamic is not a debug flag
  , check (any (\opt -> "-d" `isPrefixOf` opt && opt /= "-dynamic")
           ghc_options) $
      PackageDistInexcusable $
           "'ghc-options: -d*' debug flags are not appropriate "
        ++ "for a distributed package. "
        ++ extraExplanation

  , checkFlags ["-fprof-auto", "-fprof-auto-top", "-fprof-auto-calls",
               "-fprof-cafs", "-fno-prof-count-entries",
               "-auto-all", "-auto", "-caf-all"] $
      PackageDistSuspicious $
           "'ghc-options/ghc-prof-options: -fprof*' profiling flags are typically not "
        ++ "appropriate for a distributed library package. These flags are "
        ++ "useful to profile this package, but when profiling other packages "
        ++ "that use this one these flags clutter the profile output with "
        ++ "excessive detail. If you think other packages really want to see "
        ++ "cost centres from this package then use '-fprof-auto-exported' "
        ++ "which puts cost centres only on exported functions. "
        ++ extraExplanation
  ]
  where
    extraExplanation =
         " Alternatively, if you want to use this, make it conditional based "
      ++ "on a Cabal configuration flag (with 'manual: True' and 'default: "
      ++ "False') and enable that flag during development."

    has_WerrorWall   = has_Werror && ( has_Wall || has_W )
    has_Werror       = "-Werror" `elem` ghc_options
    has_Wall         = "-Wall"   `elem` ghc_options
    has_W            = "-W"      `elem` ghc_options
    ghc_options      = hcOptions GHC bi ++ hcProfOptions GHC bi
                       ++ hcSharedOptions GHC bi

    checkFlags :: [String] -> PackageCheck -> Maybe PackageCheck
    checkFlags flags = check (any (`elem` flags) ghc_options)

checkDevelopmentOnlyFlags :: GenericPackageDescription -> [PackageCheck]
checkDevelopmentOnlyFlags pkg =
    concatMap checkDevelopmentOnlyFlagsBuildInfo
              [ bi
              | (conditions, bi) <- allConditionalBuildInfo
              , not (any guardedByManualFlag conditions) ]
  where
    guardedByManualFlag = definitelyFalse

    -- We've basically got three-values logic here: True, False or unknown
    -- hence this pattern to propagate the unknown cases properly.
    definitelyFalse (Var (Flag n)) = maybe False not (Map.lookup n manualFlags)
    definitelyFalse (Var _)        = False
    definitelyFalse (Lit  b)       = not b
    definitelyFalse (CNot c)       = definitelyTrue c
    definitelyFalse (COr  c1 c2)   = definitelyFalse c1 && definitelyFalse c2
    definitelyFalse (CAnd c1 c2)   = definitelyFalse c1 || definitelyFalse c2

    definitelyTrue (Var (Flag n)) = fromMaybe False (Map.lookup n manualFlags)
    definitelyTrue (Var _)        = False
    definitelyTrue (Lit  b)       = b
    definitelyTrue (CNot c)       = definitelyFalse c
    definitelyTrue (COr  c1 c2)   = definitelyTrue c1 || definitelyTrue c2
    definitelyTrue (CAnd c1 c2)   = definitelyTrue c1 && definitelyTrue c2

    manualFlags = Map.fromList
                    [ (flagName flag, flagDefault flag)
                    | flag <- genPackageFlags pkg
                    , flagManual flag ]

    allConditionalBuildInfo :: [([Condition ConfVar], BuildInfo)]
    allConditionalBuildInfo =
        concatMap (collectCondTreePaths libBuildInfo)
                  (maybeToList (condLibrary pkg))

     ++ concatMap (collectCondTreePaths libBuildInfo . snd)
                  (condSubLibraries pkg)

     ++ concatMap (collectCondTreePaths buildInfo . snd)
                  (condExecutables pkg)

     ++ concatMap (collectCondTreePaths testBuildInfo . snd)
                  (condTestSuites pkg)

     ++ concatMap (collectCondTreePaths benchmarkBuildInfo . snd)
                  (condBenchmarks pkg)

    -- get all the leaf BuildInfo, paired up with the path (in the tree sense)
    -- of if-conditions that guard it
    collectCondTreePaths :: (a -> b)
                         -> CondTree v c a
                         -> [([Condition v], b)]
    collectCondTreePaths mapData = go []
      where
        go conditions condNode =
            -- the data at this level in the tree:
            (reverse conditions, mapData (condTreeData condNode))

          : concat
            [ go (condition:conditions) ifThen
            | (CondBranch condition ifThen _) <- condTreeComponents condNode ]

         ++ concat
            [ go (condition:conditions) elseThen
            | (CondBranch condition _ (Just elseThen)) <- condTreeComponents condNode ]


-- ------------------------------------------------------------
-- * Checks involving files in the package
-- ------------------------------------------------------------

-- | Sanity check things that requires IO. It looks at the files in the
-- package and expects to find the package unpacked in at the given file path.
--
checkPackageFiles :: PackageDescription -> FilePath -> NoCallStackIO [PackageCheck]
checkPackageFiles pkg root = checkPackageContent checkFilesIO pkg
  where
    checkFilesIO = CheckPackageContentOps {
      doesFileExist        = System.doesFileExist                  . relative,
      doesDirectoryExist   = System.doesDirectoryExist             . relative,
      getDirectoryContents = System.Directory.getDirectoryContents . relative,
      getFileContents      = BS.readFile
    }
    relative path = root </> path

-- | A record of operations needed to check the contents of packages.
-- Used by 'checkPackageContent'.
--
data CheckPackageContentOps m = CheckPackageContentOps {
    doesFileExist        :: FilePath -> m Bool,
    doesDirectoryExist   :: FilePath -> m Bool,
    getDirectoryContents :: FilePath -> m [FilePath],
    getFileContents      :: FilePath -> m BS.ByteString
  }

-- | Sanity check things that requires looking at files in the package.
-- This is a generalised version of 'checkPackageFiles' that can work in any
-- monad for which you can provide 'CheckPackageContentOps' operations.
--
-- The point of this extra generality is to allow doing checks in some virtual
-- file system, for example a tarball in memory.
--
checkPackageContent :: Monad m => CheckPackageContentOps m
                    -> PackageDescription
                    -> m [PackageCheck]
checkPackageContent ops pkg = do
  cabalBomError   <- checkCabalFileBOM    ops
  cabalNameError  <- checkCabalFileName   ops pkg
  licenseErrors   <- checkLicensesExist   ops pkg
  setupError      <- checkSetupExists     ops pkg
  configureError  <- checkConfigureExists ops pkg
  localPathErrors <- checkLocalPathsExist ops pkg
  vcsLocation     <- checkMissingVcsInfo  ops pkg

  return $ licenseErrors
        ++ catMaybes [cabalBomError, cabalNameError, setupError, configureError]
        ++ localPathErrors
        ++ vcsLocation

checkCabalFileBOM :: Monad m => CheckPackageContentOps m
                  -> m (Maybe PackageCheck)
checkCabalFileBOM ops = do
  epdfile <- findPackageDesc ops
  case epdfile of
    -- MASSIVE HACK.  If the Cabal file doesn't exist, that is
    -- a very strange situation to be in, because the driver code
    -- in 'Distribution.Setup' ought to have noticed already!
    -- But this can be an issue, see #3552 and also when
    -- --cabal-file is specified.  So if you can't find the file,
    -- just don't bother with this check.
    Left _       -> return $ Nothing
    Right pdfile -> (flip check pc . BS.isPrefixOf bomUtf8)
                    `liftM` (getFileContents ops pdfile)
      where pc = PackageDistInexcusable $
                 pdfile ++ " starts with an Unicode byte order mark (BOM)."
                 ++ " This may cause problems with older cabal versions."

  where
    bomUtf8 :: BS.ByteString
    bomUtf8 = BS.pack [0xef,0xbb,0xbf] -- U+FEFF encoded as UTF8

checkCabalFileName :: Monad m => CheckPackageContentOps m
                 -> PackageDescription
                 -> m (Maybe PackageCheck)
checkCabalFileName ops pkg = do
  -- findPackageDesc already takes care to detect missing/multiple
  -- .cabal files; we don't include this check in 'findPackageDesc' in
  -- order not to short-cut other checks which call 'findPackageDesc'
  epdfile <- findPackageDesc ops
  case epdfile of
    -- see "MASSIVE HACK" note in 'checkCabalFileBOM'
    Left _       -> return Nothing
    Right pdfile
      | takeFileName pdfile == expectedCabalname -> return Nothing
      | otherwise -> return $ Just $ PackageDistInexcusable $
                 "The filename " ++ pdfile ++ " does not match package name " ++
                 "(expected: " ++ expectedCabalname ++ ")"
  where
    pkgname = unPackageName . packageName $ pkg
    expectedCabalname = pkgname <.> "cabal"


-- |Find a package description file in the given directory.  Looks for
-- @.cabal@ files.  Like 'Distribution.Simple.Utils.findPackageDesc',
-- but generalized over monads.
findPackageDesc :: Monad m => CheckPackageContentOps m
                 -> m (Either PackageCheck FilePath) -- ^<pkgname>.cabal
findPackageDesc ops
 = do let dir = "."
      files <- getDirectoryContents ops dir
      -- to make sure we do not mistake a ~/.cabal/ dir for a <pkgname>.cabal
      -- file we filter to exclude dirs and null base file names:
      cabalFiles <- filterM (doesFileExist ops)
                       [ dir </> file
                       | file <- files
                       , let (name, ext) = splitExtension file
                       , not (null name) && ext == ".cabal" ]
      case cabalFiles of
        []          -> return (Left $ PackageBuildImpossible noDesc)
        [cabalFile] -> return (Right cabalFile)
        multiple    -> return (Left $ PackageBuildImpossible
                               $ multiDesc multiple)

  where
    noDesc :: String
    noDesc = "No cabal file found.\n"
             ++ "Please create a package description file <pkgname>.cabal"

    multiDesc :: [String] -> String
    multiDesc l = "Multiple cabal files found while checking.\n"
                  ++ "Please use only one of: "
                  ++ intercalate ", " l

checkLicensesExist :: Monad m => CheckPackageContentOps m
                   -> PackageDescription
                   -> m [PackageCheck]
checkLicensesExist ops pkg = do
    exists <- mapM (doesFileExist ops) (licenseFiles pkg)
    return
      [ PackageBuildWarning $
           "The '" ++ fieldname ++ "' field refers to the file "
        ++ quote file ++ " which does not exist."
      | (file, False) <- zip (licenseFiles pkg) exists ]
  where
    fieldname | length (licenseFiles pkg) == 1 = "license-file"
              | otherwise                      = "license-files"

checkSetupExists :: Monad m => CheckPackageContentOps m
                 -> PackageDescription
                 -> m (Maybe PackageCheck)
checkSetupExists ops pkg = do
  let simpleBuild = buildType pkg == Just Simple
  hsexists  <- doesFileExist ops "Setup.hs"
  lhsexists <- doesFileExist ops "Setup.lhs"
  return $ check (not simpleBuild && not hsexists && not lhsexists) $
    PackageDistInexcusable $
      "The package is missing a Setup.hs or Setup.lhs script."

checkConfigureExists :: Monad m => CheckPackageContentOps m
                     -> PackageDescription
                     -> m (Maybe PackageCheck)
checkConfigureExists ops PackageDescription { buildType = Just Configure } = do
  exists <- doesFileExist ops "configure"
  return $ check (not exists) $
    PackageBuildWarning $
      "The 'build-type' is 'Configure' but there is no 'configure' script. "
      ++ "You probably need to run 'autoreconf -i' to generate it."
checkConfigureExists _ _ = return Nothing

checkLocalPathsExist :: Monad m => CheckPackageContentOps m
                     -> PackageDescription
                     -> m [PackageCheck]
checkLocalPathsExist ops pkg = do
  let dirs = [ (dir, kind)
             | bi <- allBuildInfo pkg
             , (dir, kind) <-
                  [ (dir, "extra-lib-dirs") | dir <- extraLibDirs bi ]
               ++ [ (dir, "extra-framework-dirs")
                  | dir <- extraFrameworkDirs  bi ]
               ++ [ (dir, "include-dirs")   | dir <- includeDirs  bi ]
               ++ [ (dir, "hs-source-dirs") | dir <- hsSourceDirs bi ]
             , isRelativeOnAnyPlatform dir ]
  missing <- filterM (liftM not . doesDirectoryExist ops . fst) dirs
  return [ PackageBuildWarning {
             explanation = quote (kind ++ ": " ++ dir)
                        ++ " directory does not exist."
           }
         | (dir, kind) <- missing ]

checkMissingVcsInfo :: Monad m => CheckPackageContentOps m
                    -> PackageDescription
                    -> m [PackageCheck]
checkMissingVcsInfo ops pkg | null (sourceRepos pkg) = do
    vcsInUse <- liftM or $ mapM (doesDirectoryExist ops) repoDirnames
    if vcsInUse
      then return [ PackageDistSuspicious message ]
      else return []
  where
    repoDirnames = [ dirname | repo    <- knownRepoTypes
                             , dirname <- repoTypeDirname repo ]
    message  = "When distributing packages it is encouraged to specify source "
            ++ "control information in the .cabal file using one or more "
            ++ "'source-repository' sections. See the Cabal user guide for "
            ++ "details."

checkMissingVcsInfo _ _ = return []

repoTypeDirname :: RepoType -> [FilePath]
repoTypeDirname Darcs      = ["_darcs"]
repoTypeDirname Git        = [".git"]
repoTypeDirname SVN        = [".svn"]
repoTypeDirname CVS        = ["CVS"]
repoTypeDirname Mercurial  = [".hg"]
repoTypeDirname GnuArch    = [".arch-params"]
repoTypeDirname Bazaar     = [".bzr"]
repoTypeDirname Monotone   = ["_MTN"]
repoTypeDirname _          = []


-- ------------------------------------------------------------
-- * Checks involving files in the package
-- ------------------------------------------------------------

-- | Check the names of all files in a package for portability problems. This
-- should be done for example when creating or validating a package tarball.
--
checkPackageFileNames :: [FilePath] -> [PackageCheck]
checkPackageFileNames files =
     (take 1 . mapMaybe checkWindowsPath $ files)
  ++ (take 1 . mapMaybe checkTarPath     $ files)
      -- If we get any of these checks triggering then we're likely to get
      -- many, and that's probably not helpful, so return at most one.

checkWindowsPath :: FilePath -> Maybe PackageCheck
checkWindowsPath path =
  check (not $ FilePath.Windows.isValid path') $
    PackageDistInexcusable $
         "Unfortunately, the file " ++ quote path ++ " is not a valid file "
      ++ "name on Windows which would cause portability problems for this "
      ++ "package. Windows file names cannot contain any of the characters "
      ++ "\":*?<>|\" and there are a few reserved names including \"aux\", "
      ++ "\"nul\", \"con\", \"prn\", \"com1-9\", \"lpt1-9\" and \"clock$\"."
  where
    path' = ".\\" ++ path
    -- force a relative name to catch invalid file names like "f:oo" which
    -- otherwise parse as file "oo" in the current directory on the 'f' drive.

-- | Check a file name is valid for the portable POSIX tar format.
--
-- The POSIX tar format has a restriction on the length of file names. It is
-- unfortunately not a simple restriction like a maximum length. The exact
-- restriction is that either the whole path be 100 characters or less, or it
-- be possible to split the path on a directory separator such that the first
-- part is 155 characters or less and the second part 100 characters or less.
--
checkTarPath :: FilePath -> Maybe PackageCheck
checkTarPath path
  | length path > 255   = Just longPath
  | otherwise = case pack nameMax (reverse (splitPath path)) of
    Left err           -> Just err
    Right []           -> Nothing
    Right (h:rest) -> case pack prefixMax remainder of
      Left err         -> Just err
      Right []         -> Nothing
      Right (_:_)      -> Just noSplit
     where
        -- drop the '/' between the name and prefix:
        remainder = init h : rest

  where
    nameMax, prefixMax :: Int
    nameMax   = 100
    prefixMax = 155

    pack _   []     = Left emptyName
    pack maxLen (c:cs)
      | n > maxLen  = Left longName
      | otherwise   = Right (pack' maxLen n cs)
      where n = length c

    pack' maxLen n (c:cs)
      | n' <= maxLen = pack' maxLen n' cs
      where n' = n + length c
    pack' _     _ cs = cs

    longPath = PackageDistInexcusable $
         "The following file name is too long to store in a portable POSIX "
      ++ "format tar archive. The maximum length is 255 ASCII characters.\n"
      ++ "The file in question is:\n  " ++ path
    longName = PackageDistInexcusable $
         "The following file name is too long to store in a portable POSIX "
      ++ "format tar archive. The maximum length for the name part (including "
      ++ "extension) is 100 ASCII characters. The maximum length for any "
      ++ "individual directory component is 155.\n"
      ++ "The file in question is:\n  " ++ path
    noSplit = PackageDistInexcusable $
         "The following file name is too long to store in a portable POSIX "
      ++ "format tar archive. While the total length is less than 255 ASCII "
      ++ "characters, there are unfortunately further restrictions. It has to "
      ++ "be possible to split the file path on a directory separator into "
      ++ "two parts such that the first part fits in 155 characters or less "
      ++ "and the second part fits in 100 characters or less. Basically you "
      ++ "have to make the file name or directory names shorter, or you could "
      ++ "split a long directory name into nested subdirectories with shorter "
      ++ "names.\nThe file in question is:\n  " ++ path
    emptyName = PackageDistInexcusable $
         "Encountered a file with an empty name, something is very wrong! "
      ++ "Files with an empty name cannot be stored in a tar archive or in "
      ++ "standard file systems."

-- ------------------------------------------------------------
-- * Utils
-- ------------------------------------------------------------

quote :: String -> String
quote s = "'" ++ s ++ "'"

commaSep :: [String] -> String
commaSep = intercalate ", "

dups :: Ord a => [a] -> [a]
dups xs = [ x | (x:_:_) <- group (sort xs) ]

fileExtensionSupportedLanguage :: FilePath -> Bool
fileExtensionSupportedLanguage path =
    isHaskell || isC
  where
    extension = takeExtension path
    isHaskell = extension `elem` [".hs", ".lhs"]
    isC       = isJust (filenameCDialect extension)
