{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.Dependency
  ( Dependency(..)
  , depPkgName
  , depVerRange
  , thisPackageVersion
  , notThisPackageVersion
  , simplifyDependency
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Version ( VersionRange, thisVersion
                            , notThisVersion, anyVersion
                            , simplifyVersionRange )

import qualified Distribution.Compat.ReadP as Parse

import Distribution.Text
import Distribution.Pretty
import Distribution.Parsec.Class
import Distribution.Types.PackageId
import Distribution.Types.PackageName

import Text.PrettyPrint ((<+>))

-- | Describes a dependency on a source package (API)
--
data Dependency = Dependency PackageName VersionRange
                  deriving (Generic, Read, Show, Eq, Typeable, Data)

depPkgName :: Dependency -> PackageName
depPkgName (Dependency pn _) = pn

depVerRange :: Dependency -> VersionRange
depVerRange (Dependency _ vr) = vr

instance Binary Dependency
instance NFData Dependency where rnf = genericRnf

instance Pretty Dependency where
    pretty (Dependency name ver) = pretty name <+> pretty ver

instance Parsec Dependency where
    parsec = do
        name <- lexemeParsec
        ver  <- parsec <|> pure anyVersion
        return (Dependency name ver)

instance Text Dependency where
  parse = do name <- parse
             Parse.skipSpaces
             ver <- parse Parse.<++ return anyVersion
             Parse.skipSpaces
             return (Dependency name ver)

thisPackageVersion :: PackageIdentifier -> Dependency
thisPackageVersion (PackageIdentifier n v) =
  Dependency n (thisVersion v)

notThisPackageVersion :: PackageIdentifier -> Dependency
notThisPackageVersion (PackageIdentifier n v) =
  Dependency n (notThisVersion v)

-- | Simplify the 'VersionRange' expression in a 'Dependency'.
-- See 'simplifyVersionRange'.
--
simplifyDependency :: Dependency -> Dependency
simplifyDependency (Dependency name range) =
  Dependency name (simplifyVersionRange range)
