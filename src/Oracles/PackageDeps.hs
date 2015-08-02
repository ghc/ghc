{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.PackageDeps (
    packageDeps,
    packageDepsOracle
    ) where

import Base
import Oracles.Base
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import Control.Applicative

newtype PackageDepsKey = PackageDepsKey String
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- packageDeps depFile objFile is an action that looks up dependencies of an
-- object file (objFile) in a generated dependecy file (depFile).
packageDeps :: String -> Action [String]
packageDeps pkg = do
    res <- askOracle $ PackageDepsKey pkg
    return . fromMaybe [] $ res

-- Oracle for 'path/dist/*.deps' files
packageDepsOracle :: Rules ()
packageDepsOracle = do
    deps <- newCache $ \_ -> do
        putOracle $ "Reading package dependencies..."
        contents <- readFileLines packageDependencies
        return . Map.fromList
               $ [ (head ps, tail ps) | line <- contents, let ps = words line ]
    addOracle $ \(PackageDepsKey pkg) -> Map.lookup pkg <$> deps ()
    return ()
