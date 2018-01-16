{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Distribution.Types.UnitId
  ( UnitId, unUnitId, mkUnitId
  , DefUnitId
  , unsafeMkDefUnitId
  , unDefUnitId
  , newSimpleUnitId
  , mkLegacyUnitId
  , getHSLibraryName
  , InstalledPackageId -- backwards compat
  ) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Utils.ShortText

import qualified Distribution.Compat.ReadP as Parse
import qualified Distribution.Compat.Parsec as P
import Distribution.Pretty
import Distribution.Parsec.Class
import Distribution.Text
import Distribution.Types.ComponentId
import Distribution.Types.PackageId

import Text.PrettyPrint (text)

-- | A unit identifier identifies a (possibly instantiated)
-- package/component that can be installed the installed package
-- database.  There are several types of components that can be
-- installed:
--
--  * A traditional library with no holes, so that 'unitIdHash'
--    is @Nothing@.  In the absence of Backpack, 'UnitId'
--    is the same as a 'ComponentId'.
--
--  * An indefinite, Backpack library with holes.  In this case,
--    'unitIdHash' is still @Nothing@, but in the install,
--    there are only interfaces, no compiled objects.
--
--  * An instantiated Backpack library with all the holes
--    filled in.  'unitIdHash' is a @Just@ a hash of the
--    instantiating mapping.
--
-- A unit is a component plus the additional information on how the
-- holes are filled in. Thus there is a one to many relationship: for a
-- particular component there are many different ways of filling in the
-- holes, and each different combination is a unit (and has a separate
-- 'UnitId').
--
-- 'UnitId' is distinct from 'OpenUnitId', in that it is always
-- installed, whereas 'OpenUnitId' are intermediate unit identities
-- that arise during mixin linking, and don't necessarily correspond
-- to any actually installed unit.  Since the mapping is not actually
-- recorded in a 'UnitId', you can't actually substitute over them
-- (but you can substitute over 'OpenUnitId').  See also
-- "Distribution.Backpack.FullUnitId" for a mechanism for expanding an
-- instantiated 'UnitId' to retrieve its mapping.
--
-- Backwards compatibility note: if you need to get the string
-- representation of a UnitId to pass, e.g., as a @-package-id@
-- flag, use the 'display' function, which will work on all
-- versions of Cabal.
--
newtype UnitId = UnitId ShortText
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data, NFData)

{-# DEPRECATED InstalledPackageId "Use UnitId instead" #-}
type InstalledPackageId = UnitId

instance Binary UnitId

-- | The textual format for 'UnitId' coincides with the format
-- GHC accepts for @-package-id@.
--
instance Pretty UnitId where
    pretty = text . unUnitId

-- | The textual format for 'UnitId' coincides with the format
-- GHC accepts for @-package-id@.
--
instance Parsec UnitId where
    parsec = mkUnitId <$> P.munch1 (\c -> isAlphaNum c || c `elem` "-_.+")

instance Text UnitId where
    parse = mkUnitId <$> Parse.munch1 (\c -> isAlphaNum c || c `elem` "-_.+")

-- | If you need backwards compatibility, consider using 'display'
-- instead, which is supported by all versions of Cabal.
--
unUnitId :: UnitId -> String
unUnitId (UnitId s) = fromShortText s

mkUnitId :: String -> UnitId
mkUnitId = UnitId . toShortText

-- | 'mkUnitId'
--
-- @since 2.0.0.2
instance IsString UnitId where
    fromString = mkUnitId

-- | Create a unit identity with no associated hash directly
-- from a 'ComponentId'.
newSimpleUnitId :: ComponentId -> UnitId
newSimpleUnitId = mkUnitId . unComponentId

-- | Make an old-style UnitId from a package identifier.
-- Assumed to be for the public library
mkLegacyUnitId :: PackageId -> UnitId
mkLegacyUnitId = newSimpleUnitId . mkComponentId . display

-- | Returns library name prefixed with HS, suitable for filenames
getHSLibraryName :: UnitId -> String
getHSLibraryName uid = "HS" ++ display uid

-- | A 'UnitId' for a definite package.  The 'DefUnitId' invariant says
-- that a 'UnitId' identified this way is definite; i.e., it has no
-- unfilled holes.
newtype DefUnitId = DefUnitId { unDefUnitId :: UnitId }
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data, Binary, NFData, Pretty, Text)

-- Workaround for a GHC 8.0.1 bug, see
-- https://github.com/haskell/cabal/issues/4793#issuecomment-334258288
instance Parsec DefUnitId where
  parsec = DefUnitId <$> parsec

-- | Unsafely create a 'DefUnitId' from a 'UnitId'.  Your responsibility
-- is to ensure that the 'DefUnitId' invariant holds.
unsafeMkDefUnitId :: UnitId -> DefUnitId
unsafeMkDefUnitId = DefUnitId
