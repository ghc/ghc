{-# LANGUAGE DeriveGeneric #-}
module Package (
    Package (..), PackageName, PackageType (..),
    -- * Queries
    pkgCabalFile,
    matchPackageNames,
    -- * Helpers for constructing 'Package's
    setPath, topLevel, library, utility, setPkgType
    ) where

import Base
import GHC.Generics (Generic)

-- | It is helpful to distinguish package names from strings.
type PackageName = String

-- | We regard packages as either being libraries or programs. This is
-- bit of a convenient lie as Cabal packages can be both, but it works
-- for now.
data PackageType = Program | Library
                 deriving Generic

data Package = Package
     {
         pkgName :: PackageName, -- ^ Examples: "ghc", "Cabal"
         pkgPath :: FilePath,    -- ^ pkgPath is the path to the source code relative to the root.
                                 -- e.g. "compiler", "libraries/Cabal/Cabal"
         pkgType :: PackageType
     }
     deriving Generic

-- Relative path to cabal file, e.g.: "libraries/Cabal/Cabal/Cabal.cabal"
pkgCabalFile :: Package -> FilePath
pkgCabalFile pkg = pkgPath pkg -/- pkgName pkg <.> "cabal"

topLevel :: PackageName -> Package
topLevel name = Package name name Library

library :: PackageName -> Package
library name = Package name ("libraries" -/- name) Library

utility :: PackageName -> Package
utility name = Package name ("utils" -/- name) Program

setPath :: Package -> FilePath -> Package
setPath pkg path = pkg { pkgPath = path }

setPkgType :: Package -> PackageType -> Package
setPkgType pkg ty = pkg { pkgType = ty }

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

instance Binary PackageType
instance Hashable PackageType
instance NFData PackageType
