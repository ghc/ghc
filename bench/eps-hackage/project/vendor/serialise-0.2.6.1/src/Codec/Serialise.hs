{-# LANGUAGE CPP                #-}
{-# LANGUAGE RankNTypes         #-}

-- |
-- Module      : Codec.Serialise
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides functions to serialise and deserialise Haskell
-- values for storage or transmission, to and from lazy
-- @'Data.ByteString.Lazy.ByteString'@s. It also provides a type class
-- and utilities to help you make your types serialisable.
--
-- For a full tutorial on using this module, see
-- "Codec.Serialise.Tutorial".
--
module Codec.Serialise
  ( -- * High level, one-shot API
    -- $highlevel
    serialise
  , deserialise
  , deserialiseOrFail

    -- * Deserialisation exceptions
  , CBOR.Read.DeserialiseFailure(..)

    -- * Incremental encoding interface
    -- $primitives
  , serialiseIncremental
  , deserialiseIncremental
  , CBOR.Read.IDecode(..)

    -- * The @'Serialise'@ class
  , Serialise(..)

    -- * IO operations
    -- | Convenient utilities for basic @'IO'@ operations.

    -- ** @'FilePath'@ API
  , writeFileSerialise
  , readFileDeserialise
    -- ** @'Handle'@ API
  , hPutSerialise
  ) where

import           Control.Monad.ST
import           System.IO                        (Handle, IOMode (..), withFile)
import           Control.Exception                (throw, throwIO)

import qualified Data.ByteString.Builder          as BS
import qualified Data.ByteString.Lazy             as BS
import qualified Data.ByteString.Lazy.Internal    as BS

import           Codec.Serialise.Class
import qualified Codec.CBOR.Read  as CBOR.Read
import qualified Codec.CBOR.Write as CBOR.Write


--------------------------------------------------------------------------------

-- $primitives
-- The following API allows you to encode or decode CBOR values incrementally,
-- which is useful for large structures that require you to stream values in
-- over time.
--

-- | Serialise a Haskell value to an external binary representation.
--
-- The output is represented as a 'BS.Builder' and is constructed incrementally.
-- The representation as a 'BS.Builder' allows efficient concatenation with
-- other data.
--
-- @since 0.2.0.0
serialiseIncremental :: Serialise a => a -> BS.Builder
serialiseIncremental = CBOR.Write.toBuilder . encode

-- | Deserialise a Haskell value from the external binary representation.
--
-- This allows /input/ data to be provided incrementally, rather than all in one
-- go. It also gives an explicit representation of deserialisation errors.
--
-- Note that the incremental behaviour is only for the input data, not the
-- output value: the final deserialised value is constructed and returned as a
-- whole, not incrementally.
--
-- @since 0.2.0.0
deserialiseIncremental :: Serialise a => ST s (CBOR.Read.IDecode s a)
deserialiseIncremental = CBOR.Read.deserialiseIncremental decode

--------------------------------------------------------------------------------

-- $highlevel
-- The following API exposes a high level interface allowing you to quickly
-- convert between arbitrary Haskell values (which are an instance of
-- @'Serialise'@) and lazy @'BS.ByteString'@s.
--

-- | Serialise a Haskell value to an external binary representation.
--
-- The output is represented as a lazy 'BS.ByteString' and is constructed
-- incrementally.
--
-- @since 0.2.0.0
serialise :: Serialise a => a -> BS.ByteString
serialise = CBOR.Write.toLazyByteString . encode

-- | Deserialise a Haskell value from the external binary representation
-- (which must have been made using 'serialise' or related function).
--
-- /Throws/: @'CBOR.Read.DeserialiseFailure'@ if the given external
-- representation is invalid or does not correspond to a value of the
-- expected type.
--
-- @since 0.2.0.0
deserialise :: Serialise a => BS.ByteString -> a
deserialise bs0 =
    runST (supplyAllInput bs0 =<< deserialiseIncremental)
  where
    supplyAllInput _bs (CBOR.Read.Done _ _ x) = return x
    supplyAllInput  bs (CBOR.Read.Partial k)  =
      case bs of
        BS.Chunk chunk bs' -> k (Just chunk) >>= supplyAllInput bs'
        BS.Empty           -> k Nothing      >>= supplyAllInput BS.Empty
    supplyAllInput _ (CBOR.Read.Fail _ _ exn) = throw exn

-- | Deserialise a Haskell value from the external binary representation,
-- or get back a @'DeserialiseFailure'@.
--
-- @since 0.2.0.0
deserialiseOrFail :: Serialise a => BS.ByteString -> Either CBOR.Read.DeserialiseFailure a
deserialiseOrFail bs0 =
    runST (supplyAllInput bs0 =<< deserialiseIncremental)
  where
    supplyAllInput _bs (CBOR.Read.Done _ _ x) = return (Right x)
    supplyAllInput  bs (CBOR.Read.Partial k)  =
      case bs of
        BS.Chunk chunk bs' -> k (Just chunk) >>= supplyAllInput bs'
        BS.Empty           -> k Nothing      >>= supplyAllInput BS.Empty
    supplyAllInput _ (CBOR.Read.Fail _ _ exn) = return (Left exn)

--------------------------------------------------------------------------------
-- File-based API

-- | Serialise a @'BS.ByteString'@ (via @'serialise'@) and write it directly
-- to the specified @'Handle'@.
--
-- @since 0.2.0.0
hPutSerialise :: Serialise a
              => Handle       -- ^ The @'Handle'@ to write to.
              -> a            -- ^ The value to be serialised and written.
              -> IO ()
hPutSerialise hnd x = BS.hPut hnd (serialise x)

-- | Serialise a @'BS.ByteString'@ and write it directly to the
-- specified file.
--
-- @since 0.2.0.0
writeFileSerialise :: Serialise a
                   => FilePath     -- ^ The file to write to.
                   -> a            -- ^ The value to be serialised and written.
                   -> IO ()
writeFileSerialise fname x =
    withFile fname WriteMode $ \hnd -> hPutSerialise hnd x

-- | Read the specified file (internally, by reading a @'BS.ByteString'@)
-- and attempt to decode it into a Haskell value using @'deserialise'@
-- (the type of which is determined by the choice of the result type).
--
-- /Throws/: @'CBOR.Read.DeserialiseFailure'@ if the file fails to
-- deserialise properly.
--
-- @since 0.2.0.0
readFileDeserialise :: Serialise a
                    => FilePath     -- ^ The file to read from.
                    -> IO a         -- ^ The deserialised value.
readFileDeserialise fname =
    withFile fname ReadMode $ \hnd -> do
      input <- BS.hGetContents hnd
      case deserialiseOrFail input of
        Left  err -> throwIO err
        Right x   -> return x
