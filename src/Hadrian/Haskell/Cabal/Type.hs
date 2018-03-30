module Hadrian.Haskell.Cabal.Type where

import Development.Shake.Classes
import Distribution.PackageDescription (GenericPackageDescription, PackageDescription)
import GHC.Generics
import Hadrian.Package.Type

-- | Haskell package metadata extracted from a Cabal file.
data Cabal = Cabal
    { name                      :: PackageName
    , version                   :: String
    , synopsis                  :: String
    , genericPackageDescription :: GenericPackageDescription
    , packageDescription        :: PackageDescription
    , packageDependencies       :: [Package]
    } deriving (Eq, Show, Typeable, Generic)

instance Binary Cabal

instance Hashable Cabal where
    hashWithSalt salt = hashWithSalt salt . show

instance NFData Cabal
