{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Distribution.Types.Mixin (
    Mixin(..),
) where

import Distribution.Compat.Prelude
import Prelude ()

import Text.PrettyPrint ((<+>))

import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Text
import Distribution.Types.IncludeRenaming
import Distribution.Types.PackageName

import qualified Distribution.Compat.Parsec as P
import qualified Distribution.Compat.ReadP  as Parse

data Mixin = Mixin { mixinPackageName :: PackageName
                   , mixinIncludeRenaming :: IncludeRenaming }
    deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance Binary Mixin

instance Pretty Mixin where
    pretty (Mixin pkg_name incl) = pretty pkg_name <+> pretty incl

instance Parsec Mixin where
    parsec = do
        mod_name <- parsec
        P.spaces
        incl <- parsec
        return (Mixin mod_name incl)

instance Text Mixin where
    parse = do
        pkg_name <- parse
        Parse.skipSpaces
        incl <- parse
        return (Mixin pkg_name incl)
