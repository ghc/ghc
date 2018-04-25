module Distribution.Types.GenericPackageDescription.Lens (
    GenericPackageDescription,
    Flag,
    FlagName,
    ConfVar (..),
    module Distribution.Types.GenericPackageDescription.Lens,
    ) where

import Prelude()
import Distribution.Compat.Prelude
import Distribution.Compat.Lens

import Distribution.Types.GenericPackageDescription (GenericPackageDescription(GenericPackageDescription), Flag(MkFlag), FlagName, ConfVar (..))

-- lens
import Distribution.Types.BuildInfo.Lens

-- We import types from their packages, so we can remove unused imports
-- and have wider inter-module dependency graph
import Distribution.Types.CondTree (CondTree)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Executable (Executable)
import Distribution.Types.PackageDescription (PackageDescription)
import Distribution.Types.Benchmark (Benchmark)
import Distribution.Types.ForeignLib (ForeignLib)
import Distribution.Types.Library (Library)
import Distribution.Types.TestSuite (TestSuite)
import Distribution.Types.UnqualComponentName (UnqualComponentName)
import Distribution.System (Arch, OS)
import Distribution.Compiler (CompilerFlavor)
import Distribution.Version (VersionRange)

-------------------------------------------------------------------------------
-- GenericPackageDescription
-------------------------------------------------------------------------------

condBenchmarks :: Lens' GenericPackageDescription [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)]
condBenchmarks f (GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 x8) = fmap (\y1 -> GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 y1) (f x8)
{-# INLINE condBenchmarks #-}

condExecutables :: Lens' GenericPackageDescription [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)]
condExecutables f (GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 x8) = fmap (\y1 -> GenericPackageDescription x1 x2 x3 x4 x5 y1 x7 x8) (f x6)
{-# INLINE condExecutables #-}

condForeignLibs :: Lens' GenericPackageDescription [(UnqualComponentName, CondTree ConfVar [Dependency] Distribution.Types.ForeignLib.ForeignLib)]
condForeignLibs f (GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 x8) = fmap (\y1 -> GenericPackageDescription x1 x2 x3 x4 y1 x6 x7 x8) (f x5)
{-# INLINE condForeignLibs #-}

condLibrary :: Lens' GenericPackageDescription (Maybe (CondTree ConfVar [Dependency] Library))
condLibrary f (GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 x8) = fmap (\y1 -> GenericPackageDescription x1 x2 y1 x4 x5 x6 x7 x8) (f x3)
{-# INLINE condLibrary #-}

condSubLibraries :: Lens' GenericPackageDescription [(UnqualComponentName, CondTree ConfVar [Dependency] Library)]
condSubLibraries f (GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 x8) = fmap (\y1 -> GenericPackageDescription x1 x2 x3 y1 x5 x6 x7 x8) (f x4)
{-# INLINE condSubLibraries #-}

condTestSuites :: Lens' GenericPackageDescription [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)]
condTestSuites f (GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 x8) = fmap (\y1 -> GenericPackageDescription x1 x2 x3 x4 x5 x6 y1 x8) (f x7)
{-# INLINE condTestSuites #-}

genPackageFlags :: Lens' GenericPackageDescription [Flag]
genPackageFlags f (GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 x8) = fmap (\y1 -> GenericPackageDescription x1 y1 x3 x4 x5 x6 x7 x8) (f x2)
{-# INLINE genPackageFlags #-}

packageDescription :: Lens' GenericPackageDescription PackageDescription
packageDescription f (GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 x8) = fmap (\y1 -> GenericPackageDescription y1 x2 x3 x4 x5 x6 x7 x8) (f x1)
{-# INLINE packageDescription #-}

-------------------------------------------------------------------------------
-- BuildInfos
-------------------------------------------------------------------------------

buildInfos :: Traversal' GenericPackageDescription BuildInfo
buildInfos f (GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 x8) =
    GenericPackageDescription x1 x2
        <$> (traverse . traverse . buildInfo) f x3
        <*> (traverse . _2 . traverse . buildInfo) f x4
        <*> (traverse . _2 . traverse . buildInfo) f x5
        <*> (traverse . _2 . traverse . buildInfo) f x6
        <*> (traverse . _2 . traverse . buildInfo) f x7
        <*> (traverse . _2 . traverse . buildInfo) f x8

-------------------------------------------------------------------------------
-- Flag
-------------------------------------------------------------------------------

flagName :: Lens' Flag FlagName
flagName f (MkFlag x1 x2 x3 x4) = fmap (\y1 -> MkFlag y1 x2 x3 x4) (f x1)
{-# INLINE flagName #-}

flagDescription :: Lens' Flag String
flagDescription f (MkFlag x1 x2 x3 x4) = fmap (\y1 -> MkFlag x1 y1 x3 x4) (f x2)
{-# INLINE flagDescription #-}

flagDefault :: Lens' Flag Bool
flagDefault f (MkFlag x1 x2 x3 x4) = fmap (\y1 -> MkFlag x1 x2 y1 x4) (f x3)
{-# INLINE flagDefault #-}

flagManual :: Lens' Flag Bool
flagManual f (MkFlag x1 x2 x3 x4) = fmap (\y1 -> MkFlag x1 x2 x3 y1) (f x4)
{-# INLINE flagManual #-}

-------------------------------------------------------------------------------
-- ConfVar
-------------------------------------------------------------------------------

_OS :: Traversal' ConfVar OS
_OS f (OS os) = OS <$> f os
_OS _ x       = pure x

_Arch :: Traversal' ConfVar Arch
_Arch f (Arch arch) = Arch <$> f arch
_Arch _ x           = pure x

_Flag :: Traversal' ConfVar FlagName
_Flag f (Flag flag) = Flag <$> f flag
_Flag _ x           = pure x

_Impl :: Traversal' ConfVar (CompilerFlavor, VersionRange)
_Impl f (Impl cf vr) = uncurry Impl <$> f (cf, vr)
_Impl _ x            = pure x
