-- |
-- Module      : Foundation.Primitives.Types.AsciiString
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
-- Stability   : experimental
-- Portability : portable
--
-- A AsciiString type backed by a `ASCII` encoded byte array and all the necessary
-- functions to manipulate the string.
--
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
module Basement.Types.AsciiString
    ( AsciiString(..)
    , MutableAsciiString(..)
    -- * Binary conversion
    , fromBytesUnsafe
    , fromBytes
    ) where

import           Basement.Compat.Base
import           Basement.Compat.Semigroup
import           Basement.Types.Char7
import           Basement.UArray.Base
import qualified Basement.Types.Char7 as Char7
import qualified Basement.UArray as A (all, unsafeRecast)

-- | Opaque packed array of characters in the ASCII encoding
newtype AsciiString = AsciiString { toBytes :: UArray Char7 }
    deriving (Typeable, Semigroup, Monoid, Eq, Ord)

newtype MutableAsciiString st = MutableAsciiString (MUArray Char7 st)
    deriving (Typeable)

instance Show AsciiString where
    show = fmap Char7.toChar . toList
instance IsString AsciiString where
    fromString = fromList . fmap Char7.fromCharMask
instance IsList AsciiString where
    type Item AsciiString = Char7
    fromList = AsciiString . fromList
    toList (AsciiString chars) = toList chars

-- | Convert a Byte Array representing ASCII data directly to an AsciiString without checking for ASCII validity
--
-- If the input contains invalid Char7 value (anything above 0x7f),
-- it will trigger runtime async errors when processing data.
--
-- In doubt, use 'fromBytes'
fromBytesUnsafe :: UArray Word8 -> AsciiString
fromBytesUnsafe = AsciiString . A.unsafeRecast

-- | Convert a Byte Array representing ASCII checking validity.
--
-- If the byte array is not valid, then Nothing is returned
fromBytes :: UArray Word8 -> Maybe AsciiString
fromBytes arr
    | A.all (\x -> x < 0x80) arr = Just $ AsciiString $ A.unsafeRecast arr
    | otherwise                  = Nothing
