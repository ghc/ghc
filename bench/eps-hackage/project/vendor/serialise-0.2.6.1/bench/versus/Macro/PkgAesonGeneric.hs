{-# OPTIONS_GHC -fno-warn-orphans #-}
module Macro.PkgAesonGeneric where

import Macro.Types
import Data.Aeson as Aeson
import Data.ByteString.Lazy as BS
import Data.Maybe

serialise :: [GenericPackageDescription] -> BS.ByteString
serialise pkgs = Aeson.encode pkgs

deserialise :: BS.ByteString -> [GenericPackageDescription]
deserialise = fromJust . Aeson.decode'

instance ToJSON Version
instance ToJSON PackageName
instance ToJSON PackageId
instance ToJSON VersionRange
instance ToJSON Dependency
instance ToJSON CompilerFlavor
instance ToJSON License
instance ToJSON SourceRepo
instance ToJSON RepoKind
instance ToJSON RepoType
instance ToJSON BuildType
instance ToJSON Library
instance ToJSON Executable
instance ToJSON TestSuite
instance ToJSON TestSuiteInterface
instance ToJSON TestType
instance ToJSON Benchmark
instance ToJSON BenchmarkInterface
instance ToJSON BenchmarkType
instance ToJSON BuildInfo
instance ToJSON ModuleName
instance ToJSON Language
instance ToJSON Extension
instance ToJSON KnownExtension
instance ToJSON PackageDescription
instance ToJSON OS
instance ToJSON Arch
instance ToJSON Flag
instance ToJSON FlagName
instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (CondTree a b c)
instance ToJSON ConfVar
instance ToJSON a => ToJSON (Condition a)
instance ToJSON GenericPackageDescription

instance FromJSON Version
instance FromJSON PackageName
instance FromJSON PackageId
instance FromJSON VersionRange
instance FromJSON Dependency
instance FromJSON CompilerFlavor
instance FromJSON License
instance FromJSON SourceRepo
instance FromJSON RepoKind
instance FromJSON RepoType
instance FromJSON BuildType
instance FromJSON Library
instance FromJSON Executable
instance FromJSON TestSuite
instance FromJSON TestSuiteInterface
instance FromJSON TestType
instance FromJSON Benchmark
instance FromJSON BenchmarkInterface
instance FromJSON BenchmarkType
instance FromJSON BuildInfo
instance FromJSON ModuleName
instance FromJSON Language
instance FromJSON Extension
instance FromJSON KnownExtension
instance FromJSON PackageDescription
instance FromJSON OS
instance FromJSON Arch
instance FromJSON Flag
instance FromJSON FlagName
instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (CondTree a b c)
instance FromJSON ConfVar
instance FromJSON a => FromJSON (Condition a)
instance FromJSON GenericPackageDescription
