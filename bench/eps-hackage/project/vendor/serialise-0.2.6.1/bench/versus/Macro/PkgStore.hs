{-# OPTIONS_GHC -fno-warn-orphans -fsimpl-tick-factor=500 #-}
module Macro.PkgStore where

import Macro.Types
import Data.Store as Store
import Data.ByteString as BS

serialise :: [GenericPackageDescription] -> BS.ByteString
serialise pkgs = Store.encode pkgs

deserialise :: ByteString -> [GenericPackageDescription]
deserialise = (\(Right x) -> x) . Store.decode

instance Store Version
instance Store PackageName
instance Store PackageId
instance Store VersionRange
instance Store Dependency
instance Store CompilerFlavor
instance Store License
instance Store SourceRepo
instance Store RepoKind
instance Store RepoType
instance Store BuildType
instance Store Library
instance Store Executable
instance Store TestSuite
instance Store TestSuiteInterface
instance Store TestType
instance Store Benchmark
instance Store BenchmarkInterface
instance Store BenchmarkType
instance Store BuildInfo
instance Store ModuleName
instance Store Language
instance Store Extension
instance Store KnownExtension
instance Store OS
instance Store Arch
instance Store Flag
instance Store FlagName
instance (Store a, Store b, Store c) => Store (CondTree a b c)
instance Store ConfVar
instance Store a => Store (Condition a)
instance Store PackageDescription
instance Store GenericPackageDescription
