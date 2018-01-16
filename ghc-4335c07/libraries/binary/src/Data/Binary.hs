{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   : unstable
-- Portability : portable to Hugs and GHC. Requires the FFI and some flexible instances.
--
-- Binary serialisation of Haskell values to and from lazy 'ByteString's.
-- The Binary library provides methods for encoding Haskell values as
-- streams of bytes directly in memory. The resulting 'ByteString' can
-- then be written to disk, sent over the network, or further processed
-- (for example, compressed with gzip).
--
-- The @binary@ package is notable in that it provides both pure, and
-- high performance serialisation.
--
-- Values encoded using the 'Binary' class are always encoded in network order
-- (big endian) form, and encoded data should be portable across
-- machine endianness, word size, or compiler version. For example,
-- data encoded using the 'Binary' class could be written on any machine,
-- and read back on any another.
--
-- If the specifics of the data format is not important to you, for example,
-- you are more interested in serializing and deserializing values than
-- in which format will be used, it is possible to derive 'Binary'
-- instances using the generic support. See 'GBinaryGet' and
-- 'GBinaryPut'.
--
-- If you have specific requirements about the encoding format, you can use
-- the encoding and decoding primitives directly, see the modules
-- "Data.Binary.Get" and "Data.Binary.Put".
--
-----------------------------------------------------------------------------

module Data.Binary (

    -- * The Binary class
      Binary(..)
    -- ** Example
    -- $example

    -- * Generic support
    -- $generics
    , GBinaryGet(..)
    , GBinaryPut(..)

    -- * The Get and Put monads
    , Get
    , Put

    -- * Useful helpers for writing instances
    , putWord8
    , getWord8

    -- * Binary serialisation
    , encode                    -- :: Binary a => a -> ByteString
    , decode                    -- :: Binary a => ByteString -> a
    , decodeOrFail

    -- * IO functions for serialisation
    , encodeFile                -- :: Binary a => FilePath -> a -> IO ()
    , decodeFile                -- :: Binary a => FilePath -> IO a
    , decodeFileOrFail

    , module Data.Word -- useful

    ) where

import Data.Word

import Data.Binary.Class
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.Generic ()

import qualified Data.ByteString as B ( hGet, length )
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L ( defaultChunkSize )
import System.IO ( withBinaryFile, IOMode(ReadMode) )

------------------------------------------------------------------------

-- $example
-- To serialise a custom type, an instance of Binary for that type is
-- required. For example, suppose we have a data structure:
--
-- > data Exp = IntE Int
-- >          | OpE  String Exp Exp
-- >    deriving Show
--
-- We can encode values of this type into bytestrings using the
-- following instance, which proceeds by recursively breaking down the
-- structure to serialise:
--
-- > instance Binary Exp where
-- >       put (IntE i)      = do put (0 :: Word8)
-- >                              put i
-- >       put (OpE s e1 e2) = do put (1 :: Word8)
-- >                              put s
-- >                              put e1
-- >                              put e2
-- >
-- >       get = do t <- get :: Get Word8
-- >                case t of
-- >                     0 -> do i <- get
-- >                             return (IntE i)
-- >                     1 -> do s  <- get
-- >                             e1 <- get
-- >                             e2 <- get
-- >                             return (OpE s e1 e2)
--
-- Note how we write an initial tag byte to indicate each variant of the
-- data type.
--
-- We can simplify the writing of 'get' instances using monadic
-- combinators:
--
-- >       get = do tag <- getWord8
-- >                case tag of
-- >                    0 -> liftM  IntE get
-- >                    1 -> liftM3 OpE  get get get
--
-- To serialise this to a bytestring, we use 'encode', which packs the
-- data structure into a binary format, in a lazy bytestring
--
-- > > let e = OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
-- > > let v = encode e
--
-- Where 'v' is a binary encoded data structure. To reconstruct the
-- original data, we use 'decode'
--
-- > > decode v :: Exp
-- > OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
--
-- The lazy ByteString that results from 'encode' can be written to
-- disk, and read from disk using Data.ByteString.Lazy IO functions,
-- such as hPutStr or writeFile:
--
-- > > writeFile "/tmp/exp.txt" (encode e)
--
-- And read back with:
--
-- > > readFile "/tmp/exp.txt" >>= return . decode :: IO Exp
-- > OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
--
-- We can also directly serialise a value to and from a Handle, or a file:
--
-- > > v <- decodeFile  "/tmp/exp.txt" :: IO Exp
-- > OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
--
-- And write a value to disk
--
-- > > encodeFile "/tmp/a.txt" v
--

------------------------------------------------------------------------
-- Wrappers to run the underlying monad

-- | Encode a value using binary serialisation to a lazy ByteString.
--
encode :: Binary a => a -> ByteString
encode = runPut . put
{-# INLINE encode #-}

-- | Decode a value from a lazy ByteString, reconstructing the original structure.
decode :: Binary a => ByteString -> a
decode = runGet get

-- | Decode a value from a lazy ByteString. Returning 'Left' on failure and
-- 'Right' on success. In both cases the unconsumed input and the number of
-- consumed bytes is returned. In case of failure, a human-readable error
-- message will be returned as well.
--
-- /Since: 0.7.0.0/
decodeOrFail :: Binary a => L.ByteString
             -> Either (L.ByteString, ByteOffset, String)
                       (L.ByteString, ByteOffset, a)
decodeOrFail = runGetOrFail get


------------------------------------------------------------------------
-- Convenience IO operations

-- | Lazily serialise a value to a file.
--
-- This is just a convenience function, it's defined simply as:
--
-- > encodeFile f = B.writeFile f . encode
--
-- So for example if you wanted to compress as well, you could use:
--
-- > B.writeFile f . compress . encode
--
encodeFile :: Binary a => FilePath -> a -> IO ()
encodeFile f v = L.writeFile f (encode v)

-- | Decode a value from a file. In case of errors, 'error' will
-- be called with the error message.
--
-- /Since: 0.7.0.0/
decodeFile :: Binary a => FilePath -> IO a
decodeFile f = do
  result <- decodeFileOrFail f
  case result of
    Right x -> return x
    Left (_,str) -> error str

-- | Decode a value from a file. In case of success, the value will be returned
-- in 'Right'. In case of decoder errors, the error message together with
-- the byte offset will be returned.
decodeFileOrFail :: Binary a => FilePath -> IO (Either (ByteOffset, String) a)
decodeFileOrFail f =
  withBinaryFile f ReadMode $ \h -> do
    feed (runGetIncremental get) h
  where -- TODO: put in Data.Binary.Get and name pushFromHandle?
    feed (Done _ _ x) _ = return (Right x)
    feed (Fail _ pos str) _ = return (Left (pos, str))
    feed (Partial k) h = do
      chunk <- B.hGet h L.defaultChunkSize
      case B.length chunk of
        0 -> feed (k Nothing) h
        _ -> feed (k (Just chunk)) h

------------------------------------------------------------------------
-- $generics
--
-- Beginning with GHC 7.2, it is possible to use binary serialization
-- without writing any instance boilerplate code.
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import Data.Binary
-- > import GHC.Generics (Generic)
-- >
-- > data Foo = Foo
-- >          deriving (Generic)
-- >
-- > -- GHC will automatically fill out the instance
-- > instance Binary Foo
--
-- This mechanism makes use of GHC's efficient built-in generics
-- support.
