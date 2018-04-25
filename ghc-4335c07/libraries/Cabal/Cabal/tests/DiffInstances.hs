{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -freduction-depth=0 -fno-warn-orphans #-}
module DiffInstances where

import           Generics.SOP.TH
import           StructDiff

-------------------------------------------------------------------------------

import           Distribution.Compiler           (CompilerFlavor)
import           Distribution.License            (License)
import           Distribution.ModuleName         (ModuleName)
import           Distribution.Package
                 (Dependency, PackageIdentifier, PackageName)
import           Distribution.Types.ForeignLib
import           Distribution.Types.ForeignLibOption
import           Distribution.Types.ForeignLibType
import           Distribution.Types.IncludeRenaming (IncludeRenaming)
import           Distribution.PackageDescription
import           Distribution.Types.CondTree
import           Distribution.Types.ExeDependency
import           Distribution.Types.ExecutableScope
import           Distribution.Types.LegacyExeDependency
import           Distribution.Types.Mixin
import           Distribution.Types.PkgconfigDependency
import           Distribution.Types.UnqualComponentName
import           Distribution.Version            (Version, VersionRange)
import           Language.Haskell.Extension
                 (Extension, KnownExtension, Language)

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

deriveGeneric ''Benchmark
deriveGeneric ''BenchmarkInterface
deriveGeneric ''BenchmarkType
deriveGeneric ''BuildInfo
deriveGeneric ''BuildType
deriveGeneric ''CompilerFlavor
deriveGeneric ''CondBranch
deriveGeneric ''CondTree
deriveGeneric ''Dependency
deriveGeneric ''Executable
deriveGeneric ''Extension
deriveGeneric ''Flag
deriveGeneric ''ForeignLib
deriveGeneric ''ForeignLibOption
deriveGeneric ''ForeignLibType
deriveGeneric ''GenericPackageDescription
deriveGeneric ''IncludeRenaming
deriveGeneric ''KnownExtension
deriveGeneric ''Language
deriveGeneric ''Library
deriveGeneric ''License
deriveGeneric ''ModuleReexport
deriveGeneric ''ModuleRenaming
deriveGeneric ''PackageDescription
deriveGeneric ''PackageIdentifier
deriveGeneric ''PackageName
deriveGeneric ''RepoKind
deriveGeneric ''RepoType
deriveGeneric ''SetupBuildInfo
deriveGeneric ''SourceRepo
deriveGeneric ''TestSuite
deriveGeneric ''TestSuiteInterface
deriveGeneric ''TestType
deriveGeneric ''VersionRange

instance (Eq a, Show a) => Diff (Condition a) where diff = eqDiff
instance (Show a, Diff b, Diff c, Show b, Show c, Eq a, Eq c, Eq b) => Diff (CondTree a b c)
instance (Show a, Diff b, Diff c, Show b, Show c, Eq a, Eq c, Eq b) => Diff (CondBranch a b c)

instance Diff Benchmark
instance Diff BenchmarkInterface
instance Diff BenchmarkType
instance Diff BuildInfo
instance Diff BuildType
instance Diff CompilerFlavor
instance Diff Dependency
instance Diff ExeDependency where diff = eqDiff
instance Diff Executable
instance Diff ExecutableScope where diff = eqDiff
instance Diff Extension
instance Diff Flag
instance Diff FlagName where diff = eqDiff
instance Diff ForeignLib
instance Diff ForeignLibOption
instance Diff ForeignLibType
instance Diff GenericPackageDescription
instance Diff IncludeRenaming
instance Diff KnownExtension
instance Diff Language
instance Diff LegacyExeDependency where diff = eqDiff
instance Diff LibVersionInfo where diff = eqDiff
instance Diff Library
instance Diff License
instance Diff Mixin where diff = eqDiff
instance Diff ModuleName where diff = eqDiff
instance Diff ModuleReexport
instance Diff ModuleRenaming
instance Diff PackageDescription
instance Diff PackageIdentifier
instance Diff PackageName where diff = eqDiff
instance Diff PkgconfigDependency where diff = eqDiff
instance Diff RepoKind
instance Diff RepoType
instance Diff SetupBuildInfo
instance Diff SourceRepo
instance Diff TestSuite
instance Diff TestSuiteInterface
instance Diff TestType
instance Diff UnqualComponentName where diff = eqDiff
instance Diff Version where diff = eqDiff
instance Diff VersionRange
