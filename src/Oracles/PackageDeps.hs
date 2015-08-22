{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.PackageDeps (
    packageDeps,
    packageDepsOracle
    ) where

import Base
import Util
import Package
import qualified Data.HashMap.Strict as Map

newtype PackageDepsKey = PackageDepsKey PackageName
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- packageDeps name is an action that given a package looks up its dependencies
-- in Base.packageDependencies file. The dependencies need to be computed by
-- scanning package cabal files (see Rules.Cabal).
packageDeps :: Package -> Action [PackageName]
packageDeps pkg = do
    res <- askOracle . PackageDepsKey . pkgName $ pkg
    return . fromMaybe [] $ res

-- Oracle for the package dependencies file
packageDepsOracle :: Rules ()
packageDepsOracle = do
    deps <- newCache $ \_ -> do
        putOracle $ "Reading package dependencies..."
        contents <- readFileLines packageDependencies
        return . Map.fromList
               $ [ (head ps, tail ps) | line <- contents, let ps = words line ]
    _ <- addOracle $ \(PackageDepsKey pkg) -> Map.lookup pkg <$> deps ()
    return ()
