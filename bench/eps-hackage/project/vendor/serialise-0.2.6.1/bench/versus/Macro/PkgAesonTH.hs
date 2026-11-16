{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Macro.PkgAesonTH where

import Macro.Types
import Data.Aeson as Aeson
import Data.Aeson.TH as Aeson
import Data.ByteString.Lazy as BS
import Data.Maybe

deriveJSON defaultOptions ''Version
deriveJSON defaultOptions ''PackageName
deriveJSON defaultOptions ''PackageId
deriveJSON defaultOptions ''VersionRange
deriveJSON defaultOptions ''Dependency
deriveJSON defaultOptions ''CompilerFlavor
deriveJSON defaultOptions ''License
deriveJSON defaultOptions ''RepoKind
deriveJSON defaultOptions ''RepoType
deriveJSON defaultOptions ''SourceRepo
deriveJSON defaultOptions ''BuildType
deriveJSON defaultOptions ''ModuleName
deriveJSON defaultOptions ''Language
deriveJSON defaultOptions ''KnownExtension
deriveJSON defaultOptions ''Extension
deriveJSON defaultOptions ''BuildInfo
deriveJSON defaultOptions ''Library
deriveJSON defaultOptions ''Executable
deriveJSON defaultOptions ''TestType
deriveJSON defaultOptions ''TestSuiteInterface
deriveJSON defaultOptions ''TestSuite
deriveJSON defaultOptions ''BenchmarkType
deriveJSON defaultOptions ''BenchmarkInterface
deriveJSON defaultOptions ''Benchmark
deriveJSON defaultOptions ''PackageDescription
deriveJSON defaultOptions ''OS
deriveJSON defaultOptions ''Arch
deriveJSON defaultOptions ''FlagName
deriveJSON defaultOptions ''Flag
deriveJSON defaultOptions ''Condition
deriveJSON defaultOptions ''CondTree
deriveJSON defaultOptions ''ConfVar
deriveJSON defaultOptions ''GenericPackageDescription

serialise :: [GenericPackageDescription] -> BS.ByteString
serialise pkgs = Aeson.encode pkgs

deserialise :: BS.ByteString -> [GenericPackageDescription]
deserialise = fromJust . Aeson.decode'
