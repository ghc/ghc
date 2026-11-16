{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Macro.ReadShow where

import Macro.Types
import Data.ByteString.Lazy.Char8 as BS

serialise :: [GenericPackageDescription] -> BS.ByteString
serialise = BS.pack . show

deserialise :: BS.ByteString -> [GenericPackageDescription]
deserialise = read . BS.unpack

deriving instance Show Version
deriving instance Show PackageName
deriving instance Show PackageId
deriving instance Show VersionRange
deriving instance Show Dependency
deriving instance Show CompilerFlavor
deriving instance Show License
deriving instance Show SourceRepo
deriving instance Show RepoKind
deriving instance Show RepoType
deriving instance Show BuildType
deriving instance Show Library
deriving instance Show Executable
deriving instance Show TestSuite
deriving instance Show TestSuiteInterface
deriving instance Show TestType
deriving instance Show Benchmark
deriving instance Show BenchmarkInterface
deriving instance Show BenchmarkType
deriving instance Show BuildInfo
deriving instance Show ModuleName
deriving instance Show Language
deriving instance Show Extension
deriving instance Show KnownExtension
deriving instance Show PackageDescription
deriving instance Show OS
deriving instance Show Arch
deriving instance Show Flag
deriving instance Show FlagName
deriving instance (Show a, Show b, Show c) => Show (CondTree a b c)
deriving instance Show ConfVar
deriving instance Show a => Show (Condition a)
deriving instance Show GenericPackageDescription

deriving instance Read Version
deriving instance Read PackageName
deriving instance Read PackageId
deriving instance Read VersionRange
deriving instance Read Dependency
deriving instance Read CompilerFlavor
deriving instance Read License
deriving instance Read SourceRepo
deriving instance Read RepoKind
deriving instance Read RepoType
deriving instance Read BuildType
deriving instance Read Library
deriving instance Read Executable
deriving instance Read TestSuite
deriving instance Read TestSuiteInterface
deriving instance Read TestType
deriving instance Read Benchmark
deriving instance Read BenchmarkInterface
deriving instance Read BenchmarkType
deriving instance Read BuildInfo
deriving instance Read ModuleName
deriving instance Read Language
deriving instance Read Extension
deriving instance Read KnownExtension
deriving instance Read PackageDescription
deriving instance Read OS
deriving instance Read Arch
deriving instance Read Flag
deriving instance Read FlagName
deriving instance (Read a, Read b, Read c) => Read (CondTree a b c)
deriving instance Read ConfVar
deriving instance Read a => Read (Condition a)
deriving instance Read GenericPackageDescription
