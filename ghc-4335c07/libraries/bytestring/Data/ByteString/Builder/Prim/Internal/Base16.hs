{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Hexadecimal encoding of nibbles (4-bit) and octets (8-bit) as ASCII
-- characters.
--
-- The current implementation is based on a table based encoding inspired by
-- the code in the 'base64-bytestring' library by Bryan O'Sullivan. In our
-- benchmarks on a 32-bit machine it turned out to be the fastest
-- implementation option.
--
module Data.ByteString.Builder.Prim.Internal.Base16 (
    EncodingTable
  , lowerTable
  , encode8_as_16h
  ) where

import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S

#if MIN_VERSION_base(4,4,0)
#if MIN_VERSION_base(4,7,0)
import           Foreign
#else
import           Foreign hiding (unsafePerformIO, unsafeForeignPtrToPtr)
#endif
import           Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import           System.IO.Unsafe (unsafePerformIO)
#else
import           Foreign
#endif

-- Creating the encoding table
------------------------------

-- TODO: Use table from C implementation.

-- | An encoding table for Base16 encoding.
newtype EncodingTable = EncodingTable (ForeignPtr Word8)

tableFromList :: [Word8] -> EncodingTable
tableFromList xs = case S.pack xs of S.PS fp _ _ -> EncodingTable fp

unsafeIndex :: EncodingTable -> Int -> IO Word8
unsafeIndex (EncodingTable table) = peekElemOff (unsafeForeignPtrToPtr table)

base16EncodingTable :: EncodingTable -> IO EncodingTable
base16EncodingTable alphabet = do
    xs <- sequence $ concat $ [ [ix j, ix k] | j <- [0..15], k <- [0..15] ]
    return $ tableFromList xs
  where
    ix = unsafeIndex alphabet

{-# NOINLINE lowerAlphabet #-}
lowerAlphabet :: EncodingTable
lowerAlphabet =
    tableFromList $ map (fromIntegral . fromEnum) $ ['0'..'9'] ++ ['a'..'f']

-- | The encoding table for hexadecimal values with lower-case characters;
-- e.g., deadbeef.
{-# NOINLINE lowerTable #-}
lowerTable :: EncodingTable
lowerTable = unsafePerformIO $ base16EncodingTable lowerAlphabet

-- | Encode an octet as 16bit word comprising both encoded nibbles ordered
-- according to the host endianness. Writing these 16bit to memory will write
-- the nibbles in the correct order (i.e. big-endian).
{-# INLINE encode8_as_16h #-}
encode8_as_16h :: EncodingTable -> Word8 -> IO Word16
encode8_as_16h (EncodingTable table) =
    peekElemOff (castPtr $ unsafeForeignPtrToPtr table) . fromIntegral
