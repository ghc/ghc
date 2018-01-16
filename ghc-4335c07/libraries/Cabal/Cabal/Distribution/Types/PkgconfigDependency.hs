{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.Types.PkgconfigDependency
  ( PkgconfigDependency(..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Version (VersionRange, anyVersion)

import Distribution.Types.PkgconfigName

import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Text

import qualified Distribution.Compat.Parsec as P
import           Distribution.Compat.ReadP  ((<++))
import qualified Distribution.Compat.ReadP  as Parse
import           Text.PrettyPrint           ((<+>))

-- | Describes a dependency on a pkg-config library
--
-- @since 2.0.0.2
data PkgconfigDependency = PkgconfigDependency
                           PkgconfigName
                           VersionRange
                         deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary PkgconfigDependency
instance NFData PkgconfigDependency where rnf = genericRnf

instance Pretty PkgconfigDependency where
  pretty (PkgconfigDependency name ver) =
    pretty name <+> pretty ver

instance Parsec PkgconfigDependency where
    parsec = do
        name <- parsec
        P.spaces
        verRange <- parsec <|> pure anyVersion
        pure $ PkgconfigDependency name verRange

instance Text PkgconfigDependency where
  parse = do name <- parse
             Parse.skipSpaces
             ver <- parse <++ return anyVersion
             Parse.skipSpaces
             return $ PkgconfigDependency name ver
