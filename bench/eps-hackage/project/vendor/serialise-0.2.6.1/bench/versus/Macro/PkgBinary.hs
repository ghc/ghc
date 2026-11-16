{-# OPTIONS_GHC -fno-warn-orphans -fsimpl-tick-factor=500 #-}
module Macro.PkgBinary where

import Macro.Types
import Data.Binary as Binary
import Data.Binary.Get as Binary
import Data.ByteString.Lazy as BS

serialise :: [GenericPackageDescription] -> BS.ByteString
serialise pkgs = Binary.encode pkgs

deserialise :: BS.ByteString -> [GenericPackageDescription]
deserialise = Binary.decode

deserialiseNull :: BS.ByteString -> ()
deserialiseNull =
    Binary.runGet $ do
      n <- get :: Get Int
      go n
  where
    go 0 = return ()
    go i = do x <- get :: Get GenericPackageDescription
              x `seq` go (i-1)

instance Binary Version
instance Binary PackageName
instance Binary PackageId
instance Binary VersionRange
instance Binary Dependency
instance Binary CompilerFlavor
instance Binary License
instance Binary SourceRepo
instance Binary RepoKind
instance Binary RepoType
instance Binary BuildType
instance Binary Library
instance Binary Executable
instance Binary TestSuite
instance Binary TestSuiteInterface
instance Binary TestType
instance Binary Benchmark
instance Binary BenchmarkInterface
instance Binary BenchmarkType
instance Binary BuildInfo
instance Binary ModuleName
instance Binary Language
instance Binary Extension
instance Binary KnownExtension
instance Binary PackageDescription
instance Binary OS
instance Binary Arch
instance Binary Flag
instance Binary FlagName
instance (Binary a, Binary b, Binary c) => Binary (CondTree a b c)
instance Binary ConfVar
instance Binary a => Binary (Condition a)
instance Binary GenericPackageDescription
