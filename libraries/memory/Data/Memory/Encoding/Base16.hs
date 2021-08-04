-- |
-- Module      : Data.Memory.Encoding.Base16
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Low-level Base16 encoding and decoding.
--
-- If you just want to encode or decode some bytes, you probably want to use
-- the "Data.ByteArray.Encoding" module.
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
module Data.Memory.Encoding.Base16
    ( showHexadecimal
    , toHexadecimal
    , fromHexadecimal
    ) where

import           Data.Memory.Internal.Compat
import           Data.Word
import           Data.Bits ((.|.))
import           GHC.Prim
import           GHC.Types
import           GHC.Word
import           Control.Monad
import           Foreign.Storable
import           Foreign.Ptr (Ptr)

-- | Transform a raw memory to an hexadecimal 'String'
-- 
-- user beware, no checks are made
showHexadecimal :: (forall a . (Ptr Word8 -> IO a) -> IO a) -- ^ a 'with' type of function to hold reference to the object
                -> Int    -- ^ length in bytes
                -> String
showHexadecimal withPtr = doChunks 0
  where
        doChunks ofs len
            | len < 4   = doUnique ofs len
            | otherwise = do
                let !(W8# a, W8# b, W8# c, W8# d) = unsafeDoIO $ withPtr (read4 ofs)
                    !(# w1, w2 #) = convertByte a
                    !(# w3, w4 #) = convertByte b
                    !(# w5, w6 #) = convertByte c
                    !(# w7, w8 #) = convertByte d
                 in wToChar w1 : wToChar w2 : wToChar w3 : wToChar w4
                  : wToChar w5 : wToChar w6 : wToChar w7 : wToChar w8
                  : doChunks (ofs + 4) (len - 4)

        doUnique ofs len
            | len == 0  = []
            | otherwise =
                let !(W8# b)      = unsafeDoIO $ withPtr (byteIndex ofs)
                    !(# w1, w2 #) = convertByte b
                 in wToChar w1 : wToChar w2 : doUnique (ofs + 1) (len - 1)

        read4 :: Int -> Ptr Word8 -> IO (Word8, Word8, Word8, Word8)
        read4 ofs p =
            liftM4 (,,,) (byteIndex ofs     p) (byteIndex (ofs+1) p)
                         (byteIndex (ofs+2) p) (byteIndex (ofs+3) p)

        wToChar :: Word# -> Char
        wToChar w = toEnum (I# (word2Int# w))

        byteIndex :: Int -> Ptr Word8 -> IO Word8
        byteIndex i p = peekByteOff p i

-- | Transform a number of bytes pointed by.@src in the hexadecimal binary representation in @dst
--
-- destination memory need to be of correct size, otherwise it will lead
-- to really bad things.
toHexadecimal :: Ptr Word8 -- ^ destination memory
              -> Ptr Word8 -- ^ source memory
              -> Int       -- ^ number of bytes
              -> IO ()
toHexadecimal bout bin n = loop 0
  where loop i
            | i == n  = return ()
            | otherwise = do
                (W8# w) <- peekByteOff bin i
                let !(# w1, w2 #) = convertByte w
                pokeByteOff bout (i * 2)     (W8# w1)
                pokeByteOff bout (i * 2 + 1) (W8# w2)
                loop (i+1)

-- | Convert a value Word# to two Word#s containing
-- the hexadecimal representation of the Word#
convertByte :: Word# -> (# Word#, Word# #)
convertByte b = (# r tableHi b, r tableLo b #)
  where
        r :: Addr# -> Word# -> Word#
        r table index = indexWord8OffAddr# table (word2Int# index)

        !tableLo =
            "0123456789abcdef0123456789abcdef\
            \0123456789abcdef0123456789abcdef\
            \0123456789abcdef0123456789abcdef\
            \0123456789abcdef0123456789abcdef\
            \0123456789abcdef0123456789abcdef\
            \0123456789abcdef0123456789abcdef\
            \0123456789abcdef0123456789abcdef\
            \0123456789abcdef0123456789abcdef"#
        !tableHi =
            "00000000000000001111111111111111\
            \22222222222222223333333333333333\
            \44444444444444445555555555555555\
            \66666666666666667777777777777777\
            \88888888888888889999999999999999\
            \aaaaaaaaaaaaaaaabbbbbbbbbbbbbbbb\
            \ccccccccccccccccdddddddddddddddd\
            \eeeeeeeeeeeeeeeeffffffffffffffff"#
{-# INLINE convertByte #-}

-- | convert a base16 @src in @dst.
--
-- n need to even
fromHexadecimal :: Ptr Word8 -> Ptr Word8 -> Int -> IO (Maybe Int)
fromHexadecimal dst src n
    | odd n     = error "fromHexadecimal: invalid odd length."
    | otherwise = loop 0 0
  where loop di i
            | i == n    = return Nothing
            | otherwise = do
                a <- rHi `fmap` peekByteOff src i
                b <- rLo `fmap` peekByteOff src (i+1)
                if a == 0xff || b == 0xff
                    then return $ Just i
                    else pokeByteOff dst di (a .|. b) >> loop (di+1) (i+2)

        rLo (W8# index) = W8# (indexWord8OffAddr# tableLo (word2Int# index))
        rHi (W8# index) = W8# (indexWord8OffAddr# tableHi (word2Int# index))
        
        !tableLo =
                "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\xff\xff\xff\xff\xff\xff\
                 \\xff\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#
        !tableHi =
                "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\x00\x10\x20\x30\x40\x50\x60\x70\x80\x90\xff\xff\xff\xff\xff\xff\
                 \\xff\xa0\xb0\xc0\xd0\xe0\xf0\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xa0\xb0\xc0\xd0\xe0\xf0\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                 \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

