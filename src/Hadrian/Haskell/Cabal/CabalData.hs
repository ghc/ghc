module Hadrian.Haskell.Cabal.CabalData where

import Development.Shake.Classes
import Distribution.PackageDescription
import GHC.Generics
import Hadrian.Package

-- | Haskell package metadata extracted from a Cabal file, without performing
-- the resolution of package configuration flags and associated conditionals.
-- One consequence is that 'packageDependencies' is an overappoximation of
-- actual package dependencies; for example, both @unix@ and @win32@ packages
-- may be included even if only one of them is required on the target OS.
data CabalData = CabalData
    { name                      :: PackageName
    , version                   :: String
    , synopsis                  :: String
    , genericPackageDescription :: GenericPackageDescription
    , packageDescription        :: PackageDescription
    , packageDependencies       :: [Package]
    } deriving (Eq, Show, Typeable, Generic)

instance Binary   CabalData
instance Hashable CabalData where hashWithSalt salt = hashWithSalt salt . show
instance NFData   CabalData
