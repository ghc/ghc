{-# LANGUAGE DeriveGeneric #-}
module Package (
    Package (..), PackageName, pkgCabalFile, setPath, topLevel, library, utility,
    matchPackageNames
    ) where

import Base
import GHC.Generics (Generic)

-- It is helpful to distinguish package names from strings.
type PackageName = String

-- type PackageType = Program | Library

-- pkgPath is the path to the source code relative to the root
data Package = Package
     {
         pkgName :: PackageName, -- Examples: "ghc", "Cabal"
         pkgPath :: FilePath     -- "compiler", "libraries/Cabal/Cabal"
         -- pkgType :: PackageType  -- TopLevel, Library
     }
     deriving Generic

-- Relative path to cabal file, e.g.: "libraries/Cabal/Cabal/Cabal.cabal"
pkgCabalFile :: Package -> FilePath
pkgCabalFile pkg = pkgPath pkg -/- pkgName pkg <.> "cabal"

topLevel :: PackageName -> Package
topLevel name = Package name name

library :: PackageName -> Package
library name = Package name ("libraries" -/- name)

utility :: PackageName -> Package
utility name = Package name ("utils" -/- name)

setPath :: Package -> FilePath -> Package
setPath pkg path = pkg { pkgPath = path }

instance Show Package where
    show = pkgName

instance Eq Package where
    (==) = (==) `on` pkgName

instance Ord Package where
    compare = compare `on` pkgName

-- Given a sorted list of packages and a sorted list of package names, returns
-- packages whose names appear in the list of names
matchPackageNames :: [Package] -> [PackageName] -> [Package]
matchPackageNames = intersectOrd (\pkg name -> compare (pkgName pkg) name)

-- Instances for storing in the Shake database
instance Binary Package
instance Hashable Package where
    hashWithSalt salt = hashWithSalt salt . show
instance NFData Package
