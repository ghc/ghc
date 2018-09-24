module Hadrian.Package.Type where

import GHC.Generics
import Development.Shake.Classes

data PackageLanguage = C | Haskell deriving (Generic, Show)

-- TODO: Make PackageType more precise.
-- See https://github.com/snowleopard/hadrian/issues/12.
data PackageType = Library | Program deriving (Generic, Show)

type PackageName = String

-- TODO: Consider turning Package into a GADT indexed with language and type.
data Package = Package {
    -- | The package language. 'C' and 'Haskell' packages are supported.
    pkgLanguage :: PackageLanguage,
    -- | The package type. 'Library' and 'Program' packages are supported.
    pkgType :: PackageType,
    -- | The package name. We assume that all packages have different names,
    -- hence two packages with the same name are considered equal.
    pkgName :: PackageName,
    -- | The path to the package source code relative to the root of the build
    -- system. For example, @libraries/Cabal/Cabal@ and @ghc@ are paths to the
    -- @Cabal@ and @ghc-bin@ packages in GHC.
    pkgPath :: FilePath
    } deriving (Generic, Show)

instance Eq Package where
    p == q = pkgName p == pkgName q

instance Ord Package where
    compare p q = compare (pkgName p) (pkgName q)

instance Binary   PackageLanguage
instance Hashable PackageLanguage
instance NFData   PackageLanguage

instance Binary   PackageType
instance Hashable PackageType
instance NFData   PackageType

instance Binary   Package
instance Hashable Package
instance NFData   Package
