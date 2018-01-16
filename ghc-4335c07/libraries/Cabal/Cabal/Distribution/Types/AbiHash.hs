{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distribution.Types.AbiHash
  ( AbiHash, unAbiHash, mkAbiHash
  ) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Utils.ShortText

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Distribution.Text

-- | ABI Hashes
--
-- Use 'mkAbiHash' and 'unAbiHash' to convert from/to a
-- 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0.0.2
newtype AbiHash = AbiHash ShortText
    deriving (Eq, Show, Read, Generic)

-- | Construct a 'AbiHash' from a 'String'
--
-- 'mkAbiHash' is the inverse to 'unAbiHash'
--
-- Note: No validations are performed to ensure that the resulting
-- 'AbiHash' is valid
--
-- @since 2.0.0.2
unAbiHash :: AbiHash -> String
unAbiHash (AbiHash h) = fromShortText h

-- | Convert 'AbiHash' to 'String'
--
-- @since 2.0.0.2
mkAbiHash :: String -> AbiHash
mkAbiHash = AbiHash . toShortText

-- | 'mkAbiHash'
--
-- @since 2.0.0.2
instance IsString AbiHash where
    fromString = mkAbiHash

instance Binary AbiHash

instance Text AbiHash where
    disp = Disp.text . unAbiHash
    parse = fmap mkAbiHash (Parse.munch isAlphaNum)
