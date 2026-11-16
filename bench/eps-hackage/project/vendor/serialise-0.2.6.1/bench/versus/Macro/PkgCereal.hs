{-# OPTIONS_GHC -fno-warn-orphans -fsimpl-tick-factor=500 #-}
module Macro.PkgCereal where

import Macro.Types
import Data.Serialize as Cereal
import Data.ByteString.Lazy as BS

serialise :: [GenericPackageDescription] -> BS.ByteString
serialise pkgs = Cereal.encodeLazy pkgs

deserialise :: BS.ByteString -> [GenericPackageDescription]
deserialise = (\(Right x) -> x) . Cereal.decodeLazy

deserialiseNull :: BS.ByteString -> ()
deserialiseNull bs =
    case Cereal.runGetLazy decodeListNull bs of
      Right () -> ()
      Left e   -> error $ "PkgCereal.deserialiseNull: decoding failed: " ++ e
  where
    decodeListNull = do
      n <- get :: Get Int
      go n
    go 0 = return ()
    go i = do x <- get :: Get GenericPackageDescription
              x `seq` go (i-1)

instance Serialize Version
instance Serialize PackageName
instance Serialize PackageId
instance Serialize VersionRange
instance Serialize Dependency
instance Serialize CompilerFlavor
instance Serialize License
instance Serialize SourceRepo
instance Serialize RepoKind
instance Serialize RepoType
instance Serialize BuildType
instance Serialize Library
instance Serialize Executable
instance Serialize TestSuite
instance Serialize TestSuiteInterface
instance Serialize TestType
instance Serialize Benchmark
instance Serialize BenchmarkInterface
instance Serialize BenchmarkType
instance Serialize BuildInfo
instance Serialize ModuleName
instance Serialize Language
instance Serialize Extension
instance Serialize KnownExtension
instance Serialize PackageDescription
instance Serialize OS
instance Serialize Arch
instance Serialize Flag
instance Serialize FlagName
instance (Serialize a, Serialize b, Serialize c) => Serialize (CondTree a b c)
instance Serialize ConfVar
instance Serialize a => Serialize (Condition a)
instance Serialize GenericPackageDescription
