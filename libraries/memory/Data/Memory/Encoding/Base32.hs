-- |
-- Module      : Data.Memory.Encoding.Base32
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
-- Low-level Base32 encoding and decoding.
--
-- If you just want to encode or decode some bytes, you probably want to use
-- the "Data.ByteArray.Encoding" module.
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
module Data.Memory.Encoding.Base32
    ( toBase32
    , unBase32Length
    , fromBase32
    ) where

import           Data.Memory.Internal.Compat
import           Data.Memory.Internal.CompatPrim
import           Data.Word
import           Data.Bits ((.|.))
import           GHC.Prim
import           GHC.Word
import           Control.Monad
import           Foreign.Storable
import           Foreign.Ptr (Ptr)

-- | Transform a number of bytes pointed by.@src in the base32 binary representation in @dst
--
-- destination memory need to be of correct size, otherwise it will lead
-- to really bad things.
toBase32 :: Ptr Word8 -- ^ input
         -> Ptr Word8 -- ^ output
         -> Int       -- ^ input len
         -> IO ()
toBase32 dst src len = loop 0 0
  where
    eqChar :: Word8
    eqChar = 0x3d

    peekOrZero :: Int -> IO Word8
    peekOrZero i
        | i >= len  = return 0
        | otherwise = peekByteOff src i

    pokeOrPadding :: Int -- for the test
                  -> Int -- src index
                  -> Word8 -- the value
                  -> IO ()
    pokeOrPadding i di v
        | i <  len  = pokeByteOff dst di v
        | otherwise = pokeByteOff dst di eqChar

    loop :: Int -- index input
         -> Int -- index output
         -> IO ()
    loop i di
        | i >= len  = return ()
        | otherwise = do
            i1 <- peekByteOff src i
            i2 <- peekOrZero (i + 1)
            i3 <- peekOrZero (i + 2)
            i4 <- peekOrZero (i + 3)
            i5 <- peekOrZero (i + 4)

            let (o1,o2,o3,o4,o5,o6,o7,o8) = toBase32Per5Bytes (i1, i2, i3, i4, i5)

            pokeByteOff dst di o1
            pokeByteOff dst (di + 1) o2
            pokeOrPadding (i + 1) (di + 2) o3
            pokeOrPadding (i + 1) (di + 3) o4
            pokeOrPadding (i + 2) (di + 4) o5
            pokeOrPadding (i + 3) (di + 5) o6
            pokeOrPadding (i + 3) (di + 6) o7
            pokeOrPadding (i + 4) (di + 7) o8

            loop (i+5) (di+8)

toBase32Per5Bytes :: (Word8, Word8, Word8, Word8, Word8)
                  -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
toBase32Per5Bytes (W8# i1, W8# i2, W8# i3, W8# i4, W8# i5) =
    (index o1, index o2, index o3, index o4, index o5, index o6, index o7, index o8)
  where
    -- 1111 1000 >> 3
    !o1 =     (uncheckedShiftRL# (and# i1 0xF8##) 3#)
    -- 0000 0111 << 2 | 1100 0000 >> 6
    !o2 = or# (uncheckedShiftL#  (and# i1 0x07##) 2#) (uncheckedShiftRL# (and# i2 0xC0##) 6#)
    -- 0011 1110 >> 1
    !o3 =     (uncheckedShiftRL# (and# i2 0x3E##) 1#)
    -- 0000 0001 << 4 | 1111 0000 >> 4
    !o4 = or# (uncheckedShiftL#  (and# i2 0x01##) 4#) (uncheckedShiftRL# (and# i3 0xF0##) 4#)
    -- 0000 1111 << 1 | 1000 0000 >> 7
    !o5 = or# (uncheckedShiftL#  (and# i3 0x0F##) 1#) (uncheckedShiftRL# (and# i4 0x80##) 7#)
    -- 0111 1100 >> 2
    !o6 =     (uncheckedShiftRL# (and# i4 0x7C##) 2#)
    -- 0000 0011 << 3 | 1110 0000 >> 5
    !o7 = or# (uncheckedShiftL#  (and# i4 0x03##) 3#) (uncheckedShiftRL# (and# i5 0xE0##) 5#)
    -- 0001 1111
    !o8 =     ((and# i5 0x1F##))

    !set = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"#

    index :: Word# -> Word8
    index idx = W8# (indexWord8OffAddr# set (word2Int# idx))

-- | Get the length needed for the destination buffer for a base32 decoding.
--
-- if the length is not a multiple of 8, Nothing is returned
unBase32Length :: Ptr Word8 -> Int -> IO (Maybe Int)
unBase32Length src len
    | len < 1            = return $ Just 0
    | (len `mod` 8) /= 0 = return Nothing
    | otherwise          = do
        last1Byte <- peekByteOff src (len - 1)
        last2Byte <- peekByteOff src (len - 2)
        last3Byte <- peekByteOff src (len - 3)
        last4Byte <- peekByteOff src (len - 4)
        last5Byte <- peekByteOff src (len - 5)
        last6Byte <- peekByteOff src (len - 6)

        let dstLen = caseByte last1Byte last2Byte last3Byte last4Byte last5Byte last6Byte
        return $ Just $ (len `div` 8) * 5 - dstLen
  where
    caseByte :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Int
    caseByte last1 last2 last3 last4 last5 last6
        | last6 == eqAscii = 4
        | last5 == eqAscii = 3 -- error this padding is not expected (error will be detected in fromBase32)
        | last4 == eqAscii = 3
        | last3 == eqAscii = 2
        | last2 == eqAscii = 1 -- error this padding is not expected (error will be detected in fromBase32)
        | last1 == eqAscii = 1
        | otherwise        = 0

    eqAscii :: Word8
    eqAscii = 0x3D

-- | convert from base32 in @src to binary in @dst, using the number of bytes specified
--
-- the user should use unBase32Length to compute the correct length, or check that
-- the length specification is proper. no check is done here.
fromBase32 :: Ptr Word8 -> Ptr Word8 -> Int -> IO (Maybe Int)
fromBase32 dst src len
    | len == 0  = return Nothing
    | otherwise = loop 0 0
  where
    loop :: Int -- the index dst
         -> Int -- the index src
         -> IO (Maybe Int)
    loop di i
        | i == (len - 8) = do
            i1 <- peekByteOff src i
            i2 <- peekByteOff src (i + 1)
            i3 <- peekByteOff src (i + 2)
            i4 <- peekByteOff src (i + 3)
            i5 <- peekByteOff src (i + 4)
            i6 <- peekByteOff src (i + 5)
            i7 <- peekByteOff src (i + 6)
            i8 <- peekByteOff src (i + 7)

            let (nbBytes, i3', i4', i5', i6', i7', i8') =
                    case (i3, i4, i5, i6, i7, i8) of
                        (0x3D, 0x3D, 0x3D, 0x3D, 0x3D, 0x3D) -> (6, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41)
                        (0x3D, _   , _   , _   , _   , _   ) -> (0, i3, i4, i5, i6, i7, i8) -- invalid
                        (_   , 0x3D, 0x3D, 0x3D, 0x3D, 0x3D) -> (5, i3  , 0x41, 0x41, 0x41, 0x41, 0x41)
                        (_   , 0x3D, _   , _   , _   , _   ) -> (0, i3, i4, i5, i6, i7, i8) -- invalid
                        (_   , _   , 0x3D, 0x3D, 0x3D, 0x3D) -> (4, i3  , i4  , 0x41, 0x41, 0x41, 0x41)
                        (_   , _   , 0x3D, _   , _   , _   ) -> (0, i3, i4, i5, i6, i7, i8) -- invalid
                        (_   , _   , _   , 0x3D, 0x3D, 0x3D) -> (3, i3  , i4  , i5  , 0x41, 0x41, 0x41)
                        (_   , _   , _   , 0x3D, _   , _   ) -> (0, i3, i4, i5, i6, i7, i8) -- invalid
                        (_   , _   , _   , _   , 0x3D, 0x3D) -> (2, i3  , i4  , i5  , i6  , 0x41, 0x41)
                        (_   , _   , _   , _   , 0x3D, _   ) -> (0, i3, i4, i5, i6, i7, i8) -- invalid
                        (_   , _   , _   , _   , _   , 0x3D) -> (1, i3  , i4  , i5  , i6  , i7  , 0x41)
                        (_   , _   , _   , _   , _   , _   ) -> (0 :: Int, i3, i4, i5, i6, i7, i8)

            case fromBase32Per8Bytes (i1, i2, i3', i4', i5', i6', i7', i8') of
                Left  ofs                  -> return $ Just (i + ofs)
                Right (o1, o2, o3, o4, o5) -> do
                    pokeByteOff dst  di    o1
                    pokeByteOff dst (di+1) o2
                    when (nbBytes < 5) $ pokeByteOff dst (di+2) o3
                    when (nbBytes < 4) $ pokeByteOff dst (di+3) o4
                    when (nbBytes < 2) $ pokeByteOff dst (di+4) o5
                    return Nothing

        | otherwise = do
            i1 <- peekByteOff src i
            i2 <- peekByteOff src (i + 1)
            i3 <- peekByteOff src (i + 2)
            i4 <- peekByteOff src (i + 3)
            i5 <- peekByteOff src (i + 4)
            i6 <- peekByteOff src (i + 5)
            i7 <- peekByteOff src (i + 6)
            i8 <- peekByteOff src (i + 7)

            case fromBase32Per8Bytes (i1, i2, i3, i4, i5, i6, i7, i8) of
                Left  ofs                  -> return $ Just (i + ofs)
                Right (o1, o2, o3, o4, o5) -> do
                    pokeByteOff dst  di    o1
                    pokeByteOff dst (di+1) o2
                    pokeByteOff dst (di+2) o3
                    pokeByteOff dst (di+3) o4
                    pokeByteOff dst (di+4) o5
                    loop (di+5) (i+8)

fromBase32Per8Bytes :: (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
                    -> Either Int (Word8, Word8, Word8, Word8, Word8)
fromBase32Per8Bytes (i1, i2, i3, i4, i5, i6, i7, i8) =
    case (rset i1, rset i2, rset i3, rset i4, rset i5, rset i6, rset i7, rset i8) of
        (0xFF, _   , _   , _   , _   , _   , _   , _   ) -> Left 0
        (_   , 0xFF, _   , _   , _   , _   , _   , _   ) -> Left 1
        (_   , _   , 0xFF, _   , _   , _   , _   , _   ) -> Left 2
        (_   , _   , _   , 0xFF, _   , _   , _   , _   ) -> Left 3
        (_   , _   , _   , _   , 0xFF, _   , _   , _   ) -> Left 4
        (_   , _   , _   , _   , _   , 0xFF, _   , _   ) -> Left 5
        (_   , _   , _   , _   , _   , _   , 0xFF, _   ) -> Left 6
        (_   , _   , _   , _   , _   , _   , _   , 0xFF) -> Left 7
        (ri1 , ri2 , ri3 , ri4 , ri5 , ri6 , ri7 , ri8 ) ->
                -- 0001 1111 << 3 | 0001 11xx >> 2
            let o1 = (ri1 `unsafeShiftL` 3) .|. (ri2 `unsafeShiftR` 2)
                -- 000x xx11 << 6 | 0001 1111 << 1 | 0001 xxxx >> 4
                o2 = (ri2 `unsafeShiftL` 6) .|. (ri3 `unsafeShiftL` 1) .|. (ri4 `unsafeShiftR` 4)
                -- 000x 1111 << 4 | 0001 111x >> 1
                o3 = (ri4 `unsafeShiftL` 4) .|. (ri5 `unsafeShiftR` 1)
                -- 000x xxx1 << 7 | 0001 1111 << 2 | 0001 1xxx >> 3
                o4 = (ri5 `unsafeShiftL` 7) .|. (ri6 `unsafeShiftL` 2) .|. (ri7 `unsafeShiftR` 3)
                -- 000x x111 << 5 | 0001 1111
                o5 = (ri7 `unsafeShiftL` 5) .|. ri8
             in Right (o1, o2, o3, o4, o5)
  where
    rset :: Word8 -> Word8
    rset (W8# w)
        | booleanPrim (w `leWord#` 0xff##) = W8# (indexWord8OffAddr# rsetTable (word2Int# w))
        | otherwise                        = 0xff

    !rsetTable = "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
                 \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
                 \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
                 \\xFF\xFF\x1A\x1B\x1C\x1D\x1E\x1F\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
                 \\xFF\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\
                 \\x0F\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\xFF\xFF\xFF\xFF\xFF\
                 \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
                 \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
                 \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
                 \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
                 \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
                 \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
                 \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
                 \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
                 \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
                 \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"#
