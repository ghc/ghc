{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.PackageId
  ( PackageIdentifier(..)
  , PackageId
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Version
         ( Version, nullVersion )

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Distribution.Compat.ReadP
import Distribution.Text
import Distribution.Pretty
import Distribution.Types.PackageName

-- | Type alias so we can use the shorter name PackageId.
type PackageId = PackageIdentifier

-- | The name and version of a package.
data PackageIdentifier
    = PackageIdentifier {
        pkgName    :: PackageName, -- ^The name of this package, eg. foo
        pkgVersion :: Version -- ^the version of this package, eg 1.2
     }
     deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary PackageIdentifier

instance Pretty PackageIdentifier where
  pretty (PackageIdentifier n v)
    | v == nullVersion = pretty n -- if no version, don't show version.
    | otherwise        = pretty n <<>> Disp.char '-' <<>> pretty v

instance Text PackageIdentifier where
  parse = do
    n <- parse
    v <- (Parse.char '-' >> parse) <++ return nullVersion
    return (PackageIdentifier n v)

instance NFData PackageIdentifier where
    rnf (PackageIdentifier name version) = rnf name `seq` rnf version
