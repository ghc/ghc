{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Backwards compatibility reexport of everything you need to know
-- about @.cabal@ files.

module Distribution.PackageDescription (
        -- * Package descriptions
        PackageDescription(..),
        emptyPackageDescription,
        specVersion,
        descCabalVersion,
        BuildType(..),
        knownBuildTypes,
        allLibraries,

        -- ** Renaming (syntactic)
        ModuleRenaming(..),
        defaultRenaming,

        -- ** Libraries
        Library(..),
        ModuleReexport(..),
        emptyLibrary,
        withLib,
        hasPublicLib,
        hasLibs,
        explicitLibModules,
        libModulesAutogen,
        libModules,

        -- ** Executables
        Executable(..),
        emptyExecutable,
        withExe,
        hasExes,
        exeModules,
        exeModulesAutogen,

        -- * Tests
        TestSuite(..),
        TestSuiteInterface(..),
        TestType(..),
        testType,
        knownTestTypes,
        emptyTestSuite,
        hasTests,
        withTest,
        testModules,
        testModulesAutogen,

        -- * Benchmarks
        Benchmark(..),
        BenchmarkInterface(..),
        BenchmarkType(..),
        benchmarkType,
        knownBenchmarkTypes,
        emptyBenchmark,
        hasBenchmarks,
        withBenchmark,
        benchmarkModules,
        benchmarkModulesAutogen,

        -- * Build information
        BuildInfo(..),
        emptyBuildInfo,
        allBuildInfo,
        allLanguages,
        allExtensions,
        usedExtensions,
        usesTemplateHaskellOrQQ,
        hcOptions,
        hcProfOptions,
        hcSharedOptions,
        hcStaticOptions,

        -- ** Supplementary build information
        ComponentName(..),
        defaultLibName,
        HookedBuildInfo,
        emptyHookedBuildInfo,
        updatePackageDescription,

        -- * package configuration
        GenericPackageDescription(..),
        Flag(..), emptyFlag,
        FlagName, mkFlagName, unFlagName,
        FlagAssignment, mkFlagAssignment, unFlagAssignment,
        nullFlagAssignment, showFlagValue,
        diffFlagAssignment, lookupFlagAssignment, insertFlagAssignment,
        dispFlagAssignment, parseFlagAssignment, parsecFlagAssignment,
        CondTree(..), ConfVar(..), Condition(..),
        cNot, cAnd, cOr,

        -- * Source repositories
        SourceRepo(..),
        RepoKind(..),
        RepoType(..),
        knownRepoTypes,
        emptySourceRepo,

        -- * Custom setup build information
        SetupBuildInfo(..),
  ) where

import Prelude ()
--import Distribution.Compat.Prelude

import Distribution.Types.Library
import Distribution.Types.TestSuite
import Distribution.Types.Executable
import Distribution.Types.Benchmark
import Distribution.Types.TestType
import Distribution.Types.TestSuiteInterface
import Distribution.Types.BenchmarkType
import Distribution.Types.BenchmarkInterface
import Distribution.Types.ModuleRenaming
import Distribution.Types.ModuleReexport
import Distribution.Types.BuildInfo
import Distribution.Types.SetupBuildInfo
import Distribution.Types.BuildType
import Distribution.Types.GenericPackageDescription
import Distribution.Types.CondTree
import Distribution.Types.Condition
import Distribution.Types.PackageDescription
import Distribution.Types.ComponentName
import Distribution.Types.HookedBuildInfo
import Distribution.Types.SourceRepo
