{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.Types.UnqualComponentName
  ( UnqualComponentName, unUnqualComponentName, mkUnqualComponentName
  , packageNameToUnqualComponentName, unqualComponentNameToPackageName
  ) where

import Distribution.Compat.Prelude
import Distribution.Utils.ShortText
import Prelude ()

import Distribution.Parsec.Class
import Distribution.ParseUtils        (parsePackageName)
import Distribution.Pretty
import Distribution.Text
import Distribution.Types.PackageName

-- | An unqualified component name, for any kind of component.
--
-- This is distinguished from a 'ComponentName' and 'ComponentId'. The former
-- also states which of a library, executable, etc the name refers too. The
-- later uniquely identifiers a component and its closure.
--
-- @since 2.0.0.2
newtype UnqualComponentName = UnqualComponentName ShortText
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data,
            Semigroup, Monoid) -- TODO: bad enabler of bad monoids

-- | Convert 'UnqualComponentName' to 'String'
--
-- @since 2.0.0.2
unUnqualComponentName :: UnqualComponentName -> String
unUnqualComponentName (UnqualComponentName s) = fromShortText s

-- | Construct a 'UnqualComponentName' from a 'String'
--
-- 'mkUnqualComponentName' is the inverse to 'unUnqualComponentName'
--
-- Note: No validations are performed to ensure that the resulting
-- 'UnqualComponentName' is valid
--
-- @since 2.0.0.2
mkUnqualComponentName :: String -> UnqualComponentName
mkUnqualComponentName = UnqualComponentName . toShortText

-- | 'mkUnqualComponentName'
--
-- @since 2.0.0.2
instance IsString UnqualComponentName where
  fromString = mkUnqualComponentName

instance Binary UnqualComponentName

instance Pretty UnqualComponentName where
  pretty = showToken . unUnqualComponentName

instance Parsec UnqualComponentName where
  parsec = mkUnqualComponentName <$> parsecUnqualComponentName

instance Text UnqualComponentName where
  parse = mkUnqualComponentName <$> parsePackageName

instance NFData UnqualComponentName where
  rnf (UnqualComponentName pkg) = rnf pkg

-- TODO avoid String round trip with these PackageName <->
-- UnqualComponentName converters.

-- | Converts a package name to an unqualified component name
--
-- Useful in legacy situations where a package name may refer to an internal
-- component, if one is defined with that name.
--
-- @since 2.0.0.2
packageNameToUnqualComponentName :: PackageName -> UnqualComponentName
packageNameToUnqualComponentName = mkUnqualComponentName . unPackageName

-- | Converts an unqualified component name to a package name
--
-- `packageNameToUnqualComponentName` is the inverse of
-- `unqualComponentNameToPackageName`.
--
-- Useful in legacy situations where a package name may refer to an internal
-- component, if one is defined with that name.
--
-- @since 2.0.0.2
unqualComponentNameToPackageName :: UnqualComponentName -> PackageName
unqualComponentNameToPackageName = mkPackageName . unUnqualComponentName
