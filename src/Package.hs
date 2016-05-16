{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Package (
    Package (..), PackageName (..), PackageType (..),
    -- * Queries
    pkgNameString,
    pkgCabalFile,
    matchPackageNames,
    -- * Helpers for constructing and using 'Package's
    setPath, topLevel, library, utility, setType, isLibrary, isProgram
    ) where

import Data.String
import GHC.Generics (Generic)

import Base

-- | The name of a Cabal package
newtype PackageName = PackageName { fromPackageName :: String }
    deriving (Eq, Ord, IsString, Generic, Binary, Hashable, Typeable, NFData)

-- TODO: Make PackageType more precise, #12
-- TODO: Turn Program to Program FilePath thereby getting rid of programPath
-- | We regard packages as either being libraries or programs. This is
-- bit of a convenient lie as Cabal packages can be both, but it works
-- for now.
data PackageType = Program | Library deriving Generic

data Package = Package
    { pkgName :: PackageName -- ^ Examples: "ghc", "Cabal"
    , pkgPath :: FilePath    -- ^ pkgPath is the path to the source code relative to the root.
                             -- e.g. "compiler", "libraries/Cabal/Cabal"
    , pkgType :: PackageType
    } deriving Generic

-- | Prettyprint Package name.
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

-- TODO: Get rid of non-derived Show instances.
instance Show Package where
    show = pkgNameString

instance Eq Package where
    (==) = (==) `on` pkgName

instance Ord Package where
    compare = compare `on` pkgName

-- | Given a sorted list of packages and a sorted list of package names, returns
-- packages whose names appear in the list of names.
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
