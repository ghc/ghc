{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.Types.LegacyExeDependency
  ( LegacyExeDependency(..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec.Class
import Distribution.ParseUtils   (parseMaybeQuoted)
import Distribution.Pretty
import Distribution.Text
import Distribution.Version      (VersionRange, anyVersion)

import qualified Distribution.Compat.Parsec as P
import           Distribution.Compat.ReadP  ((<++))
import qualified Distribution.Compat.ReadP  as Parse
import           Text.PrettyPrint           (text, (<+>))

-- | Describes a legacy `build-tools`-style dependency on an executable
--
-- It is "legacy" because we do not know what the build-tool referred to. It
-- could refer to a pkg-config executable (PkgconfigName), or an internal
-- executable (UnqualComponentName). Thus the name is stringly typed.
--
-- @since 2.0.0.2
data LegacyExeDependency = LegacyExeDependency
                           String
                           VersionRange
                         deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary LegacyExeDependency
instance NFData LegacyExeDependency where rnf = genericRnf

instance Pretty LegacyExeDependency where
  pretty (LegacyExeDependency name ver) =
    text name <+> pretty ver

instance Parsec LegacyExeDependency where
    parsec = do
        name <- parsecMaybeQuoted nameP
        P.spaces
        verRange <- parsecMaybeQuoted parsec <|> pure anyVersion
        pure $ LegacyExeDependency name verRange
      where
        nameP = intercalate "-" <$> P.sepBy1 component (P.char '-')
        component = do
            cs <- P.munch1 (\c -> isAlphaNum c || c == '+' || c == '_')
            if all isDigit cs then fail "invalid component" else return cs

instance Text LegacyExeDependency where
  parse = do name <- parseMaybeQuoted parseBuildToolName
             Parse.skipSpaces
             ver <- parse <++ return anyVersion
             Parse.skipSpaces
             return $ LegacyExeDependency name ver
    where
      -- like parsePackageName but accepts symbols in components
      parseBuildToolName :: Parse.ReadP r String
      parseBuildToolName = do ns <- Parse.sepBy1 component (Parse.char '-')
                              return (intercalate "-" ns)
        where component = do
                cs <- Parse.munch1 (\c -> isAlphaNum c || c == '+' || c == '_')
                if all isDigit cs then Parse.pfail else return cs
