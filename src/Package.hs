{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Package (
    Package (..), PackageName (..), PackageType (..),
    -- * Queries
    pkgNameString, pkgCabalFile,
    -- * Helpers for constructing and using 'Package's
    setPath, topLevel, library, utility, setType, isLibrary, isProgram
    ) where

import Data.String
import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Generics
import Hadrian.Utilities

-- | The name of a Cabal package.
newtype PackageName = PackageName { fromPackageName :: String }
    deriving (Binary, Eq, Generic, Hashable, IsString, NFData, Ord, Typeable)

-- TODO: Make PackageType more precise, #12.
-- | We regard packages as either being libraries or programs. This is bit of a
-- convenient lie as Cabal packages can be both, but it works for now.
data PackageType = Library | Program deriving Generic

data Package = Package
    { pkgName :: PackageName -- ^ Examples: "ghc", "Cabal".
    , pkgPath :: FilePath    -- ^ pkgPath is the path to the source code relative
                             -- to the root, e.g. "compiler", "libraries/Cabal/Cabal".
    , pkgType :: PackageType -- ^ A library or a program.
    } deriving Generic

-- TODO: Get rid of non-derived Show instances.
instance Show Package where
    show = pkgNameString

instance Eq Package where
    p == q = pkgName p == pkgName q

instance Ord Package where
    compare p q = compare (pkgName p) (pkgName q)

instance Binary   Package
instance Hashable Package
instance NFData   Package

instance Binary   PackageType
instance Hashable PackageType
instance NFData   PackageType

-- | Prettyprint 'Package' name.
pkgNameString :: Package -> String
pkgNameString = fromPackageName . pkgName

-- | Relative path to cabal file, e.g.: "libraries/Cabal/Cabal/Cabal.cabal"
pkgCabalFile :: Package -> FilePath
pkgCabalFile pkg = pkgPath pkg -/- pkgNameString pkg <.> "cabal"

-- | Smart constructor for a top-level package, e.g. 'compiler'.
topLevel :: PackageName -> Package
topLevel name = Package name (fromPackageName name) Library

-- | Smart constructor for a library package, e.g. 'array'.
library :: PackageName -> Package
library name = Package name ("libraries" -/- fromPackageName name) Library

-- | Smart constructor for a utility package, e.g. 'haddock'.
utility :: PackageName -> Package
utility name = Package name ("utils" -/- fromPackageName name) Program

-- | Amend package path. Useful when a package name doesn't match its path.
setPath :: Package -> FilePath -> Package
setPath pkg path = pkg { pkgPath = path }

-- | Amend package type.
setType :: Package -> PackageType -> Package
setType pkg ty = pkg { pkgType = ty }

-- | Check whether a package is a library.
isLibrary :: Package -> Bool
isLibrary (Package _ _ Library) = True
isLibrary _ = False

-- | Check whether a package is a program.
isProgram :: Package -> Bool
isProgram (Package _ _ Program) = True
isProgram _ = False
