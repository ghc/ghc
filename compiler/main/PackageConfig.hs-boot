module PackageConfig where
import FastString
import {-# SOURCE #-} Module
import GHC.PackageDb
newtype PackageName = PackageName FastString
newtype SourcePackageId = SourcePackageId FastString
type PackageConfig = InstalledPackageInfo ComponentId SourcePackageId PackageName UnitId ModuleName Module
