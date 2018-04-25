{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.PackageName
  ( PackageName, unPackageName, mkPackageName
  ) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Utils.ShortText

import qualified Text.PrettyPrint as Disp
import Distribution.ParseUtils
import Distribution.Text
import Distribution.Pretty
import Distribution.Parsec.Class

-- | A package name.
--
-- Use 'mkPackageName' and 'unPackageName' to convert from/to a
-- 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0.0.2
newtype PackageName = PackageName ShortText
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

-- | Convert 'PackageName' to 'String'
unPackageName :: PackageName -> String
unPackageName (PackageName s) = fromShortText s

-- | Construct a 'PackageName' from a 'String'
--
-- 'mkPackageName' is the inverse to 'unPackageName'
--
-- Note: No validations are performed to ensure that the resulting
-- 'PackageName' is valid
--
-- @since 2.0.0.2
mkPackageName :: String -> PackageName
mkPackageName = PackageName . toShortText

-- | 'mkPackageName'
--
-- @since 2.0.0.2
instance IsString PackageName where
  fromString = mkPackageName

instance Binary PackageName

instance Pretty PackageName where
  pretty = Disp.text . unPackageName

instance Parsec PackageName where
  parsec = mkPackageName <$> parsecUnqualComponentName

instance Text PackageName where
  parse = mkPackageName <$> parsePackageName

instance NFData PackageName where
    rnf (PackageName pkg) = rnf pkg
