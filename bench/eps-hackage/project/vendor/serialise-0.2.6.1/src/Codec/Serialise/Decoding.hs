-- |
-- Module      : Codec.Serialise.Decoding
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- High level API for decoding values that were encoded with the
-- "Codec.Serialise.Encoding" module, using a @'Monad'@
-- based interface.
--
module Codec.Serialise.Decoding
  ( -- * Decode primitive operations
    Decoder
  , DecodeAction(..)
  , getDecodeAction

  -- ** Read input tokens
  , decodeWord          -- :: Decoder s Word
  , decodeWord8         -- :: Decoder s Word8
  , decodeWord16        -- :: Decoder s Word16
  , decodeWord32        -- :: Decoder s Word32
  , decodeWord64        -- :: Decoder s Word64
  , decodeNegWord       -- :: Decoder s Word
  , decodeNegWord64     -- :: Decoder s Word64
  , decodeInt           -- :: Decoder s Int
  , decodeInt8          -- :: Decoder s Int8
  , decodeInt16         -- :: Decoder s Int16
  , decodeInt32         -- :: Decoder s Int32
  , decodeInt64         -- :: Decoder s Int64
  , decodeInteger       -- :: Decoder s Integer
  , decodeFloat         -- :: Decoder s Float
  , decodeDouble        -- :: Decoder s Double
  , decodeBytes         -- :: Decoder s ByteString
  , decodeBytesIndef    -- :: Decoder s ()
  , decodeByteArray     -- :: Decoder s ByteArray
  , decodeString        -- :: Decoder s Text
  , decodeStringIndef   -- :: Decoder s ()
  , decodeUtf8ByteArray -- :: Decoder s ByteArray
  , decodeListLen       -- :: Decoder s Int
  , decodeListLenIndef  -- :: Decoder s ()
  , decodeMapLen        -- :: Decoder s Int
  , decodeMapLenIndef   -- :: Decoder s ()
  , decodeTag           -- :: Decoder s Word
  , decodeTag64         -- :: Decoder s Word64
  , decodeBool          -- :: Decoder s Bool
  , decodeNull          -- :: Decoder s ()
  , decodeSimple        -- :: Decoder s Word8

  -- ** Specialised Read input token operations
  , decodeWordOf        -- :: Word -> Decoder s ()
  , decodeListLenOf     -- :: Int  -> Decoder s ()

  -- ** Branching operations
--, decodeBytesOrIndef
--, decodeStringOrIndef
  , decodeListLenOrIndef -- :: Decoder s (Maybe Int)
  , decodeMapLenOrIndef  -- :: Decoder s (Maybe Int)
  , decodeBreakOr        -- :: Decoder s Bool

  -- ** Inspecting the token type
  , peekTokenType        -- :: Decoder s TokenType
  , peekAvailable        -- :: Decoder s Int
  , TokenType(..)

  -- ** Special operations
--, ignoreTerms
--, decodeTrace

  -- * Sequence operations
  , decodeSequenceLenIndef -- :: ...
  , decodeSequenceLenN     -- :: ...
  ) where

import           Codec.CBOR.Decoding

import           Prelude         hiding (decodeFloat)
