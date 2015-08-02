{-# LANGUAGE DeriveGeneric #-}

module Package (
    Package (..), PackageName, pkgCabalPath,
    library, topLevel, setPath
    ) where

import Base
import Util
import Data.Function
import GHC.Generics

-- It is helpful to distinguish package names from strings.
type PackageName = String

-- pkgPath is the path to the source code relative to the root
data Package = Package
     {
         pkgName :: PackageName, -- Examples: "ghc", "Cabal"
         pkgPath :: FilePath     -- "compiler", "libraries/Cabal/Cabal"
     }
     deriving Generic

-- Relative path to cabal file, e.g.: "libraries/Cabal/Cabal/Cabal.cabal"
pkgCabalPath :: Package -> FilePath
pkgCabalPath pkg = pkgPath pkg -/- pkgName pkg <.> "cabal"

instance Show Package where
    show = pkgName

instance Eq Package where
    (==) = (==) `on` pkgName

instance Ord Package where
    compare = compare `on` pkgName

library :: PackageName -> Package
library name = Package name ("libraries" -/- name)

topLevel :: PackageName -> Package
topLevel name = Package name name

setPath :: Package -> FilePath -> Package
setPath pkg path = pkg { pkgPath = path }

-- Instances for storing in the Shake database
instance Binary Package
instance Hashable Package where
    hashWithSalt salt = hashWithSalt salt . show
