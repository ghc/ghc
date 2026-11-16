-- |
-- Module      : Codec.Serialise.Encoding
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- High level API for encoding values, for later serialization into
-- CBOR binary format, using a @'Monoid'@ based interface.
--
module Codec.Serialise.Encoding
  ( -- * Encoding implementation
    Encoding(..)             -- :: *
  , Tokens(..)               -- :: *

    -- * @'Encoding'@ API for serialisation
  , encodeWord               -- :: Word -> Encoding
  , encodeWord8              -- :: Word8 -> Encoding
  , encodeWord16             -- :: Word16 -> Encoding
  , encodeWord32             -- :: Word32 -> Encoding
  , encodeWord64             -- :: Word64 -> Encoding
  , encodeInt                -- :: Int -> Encoding
  , encodeInt8               -- :: Int8 -> Encoding
  , encodeInt16              -- :: Int16 -> Encoding
  , encodeInt32              -- :: Int32 -> Encoding
  , encodeInt64              -- :: Int64 -> Encoding
  , encodeInteger            -- :: Integer -> Encoding
  , encodeBytes              -- :: B.ByteString -> Encoding
  , encodeBytesIndef         -- :: Encoding
  , encodeByteArray          -- :: ByteArray -> Encoding
  , encodeString             -- :: T.Text -> Encoding
  , encodeStringIndef        -- :: Encoding
  , encodeUtf8ByteArray      -- :: ByteArray -> Encoding
  , encodeListLen            -- :: Word -> Encoding
  , encodeListLenIndef       -- :: Encoding
  , encodeMapLen             -- :: Word -> Encoding
  , encodeMapLenIndef        -- :: Encoding
  , encodeBreak              -- :: Encoding
  , encodeTag                -- :: Word -> Encoding
  , encodeTag64              -- :: Word64 -> Encoding
  , encodeBool               -- :: Bool -> Encoding
  , encodeUndef              -- :: Encoding
  , encodeNull               -- :: Encoding
  , encodeSimple             -- :: Word8 -> Encoding
  , encodeFloat16            -- :: Float -> Encoding
  , encodeFloat              -- :: Float -> Encoding
  , encodeDouble             -- :: Double -> Encoding
  ) where

import           Codec.CBOR.Encoding

import           Prelude         hiding (encodeFloat)
