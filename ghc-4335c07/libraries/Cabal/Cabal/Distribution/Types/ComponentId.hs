{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distribution.Types.ComponentId
  ( ComponentId, unComponentId, mkComponentId
  ) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Utils.ShortText

import qualified Distribution.Compat.ReadP as Parse
import qualified Distribution.Compat.Parsec as  P
import Distribution.Text
import Distribution.Pretty
import Distribution.Parsec.Class

import Text.PrettyPrint (text)

-- | A 'ComponentId' uniquely identifies the transitive source
-- code closure of a component (i.e. libraries, executables).
--
-- For non-Backpack components, this corresponds one to one with
-- the 'UnitId', which serves as the basis for install paths,
-- linker symbols, etc.
--
-- Use 'mkComponentId' and 'unComponentId' to convert from/to a
-- 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0.0.2
newtype ComponentId = ComponentId ShortText
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

-- | Construct a 'ComponentId' from a 'String'
--
-- 'mkComponentId' is the inverse to 'unComponentId'
--
-- Note: No validations are performed to ensure that the resulting
-- 'ComponentId' is valid
--
-- @since 2.0.0.2
mkComponentId :: String -> ComponentId
mkComponentId = ComponentId . toShortText

-- | Convert 'ComponentId' to 'String'
--
-- @since 2.0.0.2
unComponentId :: ComponentId -> String
unComponentId (ComponentId s) = fromShortText s

-- | 'mkComponentId'
--
-- @since 2.0.0.2
instance IsString ComponentId where
    fromString = mkComponentId

instance Binary ComponentId

instance Pretty ComponentId where
  pretty = text . unComponentId

instance Parsec ComponentId where
  parsec = mkComponentId `fmap` P.munch1 abi_char
   where abi_char c = isAlphaNum c || c `elem` "-_."

instance Text ComponentId where
  parse = mkComponentId `fmap` Parse.munch1 abi_char
   where abi_char c = isAlphaNum c || c `elem` "-_."

instance NFData ComponentId where
    rnf = rnf . unComponentId
