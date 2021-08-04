-- |
-- Module      : Data.Memory.Encoding.Base64
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Low-level Base64 encoding and decoding.
--
-- If you just want to encode or decode some bytes, you probably want to use
-- the "Data.ByteArray.Encoding" module.
--
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE UnboxedTuples     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE Rank2Types        #-}
module Data.Memory.Encoding.Base64
    ( toBase64
    , toBase64URL
    , toBase64OpenBSD
    , unBase64Length
    , unBase64LengthUnpadded
    , fromBase64
    , fromBase64URLUnpadded
    , fromBase64OpenBSD
    ) where

import           Data.Memory.Internal.Compat
import           Data.Memory.Internal.CompatPrim
import           Data.Memory.Internal.Imports
import           Data.Bits ((.|.))
import           GHC.Prim
import           GHC.Word
import           Foreign.Storable
import           Foreign.Ptr (Ptr)

-- | Transform a number of bytes pointed by @src@ to base64 binary representation in @dst@
--
-- The destination memory need to be of correct size, otherwise it will lead
-- to really bad things.
toBase64 :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
toBase64 dst src len = toBase64Internal set dst src len True
  where
        !set = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"#

-- | Transform a number of bytes pointed by @src@ to, URL-safe base64 binary
-- representation in @dst@. The result will be either padded or unpadded,
-- depending on the boolean @padded@ argument.
--
-- The destination memory need to be of correct size, otherwise it will lead
-- to really bad things.
toBase64URL :: Bool -> Ptr Word8 -> Ptr Word8 -> Int -> IO ()
toBase64URL padded dst src len = toBase64Internal set dst src len padded
  where
        !set = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"#

toBase64OpenBSD :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
toBase64OpenBSD dst src len = toBase64Internal set dst src len False
  where
        !set = "./ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"#

toBase64Internal :: Addr# -> Ptr Word8 -> Ptr Word8 -> Int -> Bool -> IO ()
toBase64Internal table dst src len padded = loop 0 0
  where
        eqChar = 0x3d :: Word8

        loop i di
            | i >= len  = return ()
            | otherwise = do
                a <- peekByteOff src i
                b <- if i + 1 >= len then return 0 else peekByteOff src (i+1)
                c <- if i + 2 >= len then return 0 else peekByteOff src (i+2)

                let (w,x,y,z) = convert3 table a b c

                pokeByteOff dst di     w
                pokeByteOff dst (di+1) x

                if i + 1 < len
                    then
                        pokeByteOff dst (di+2) y
                    else
                        when padded (pokeByteOff dst (di+2) eqChar)
                if i + 2 < len
                    then
                        pokeByteOff dst (di+3) z
                    else
                        when padded (pokeByteOff dst (di+3) eqChar)

                loop (i+3) (di+4)

convert3 :: Addr# -> Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8, Word8)
convert3 table (W8# a) (W8# b) (W8# c) =
    let !w = narrow8Word# (uncheckedShiftRL# a 2#)
        !x = or# (and# (uncheckedShiftL# a 4#) 0x30##) (uncheckedShiftRL# b 4#)
        !y = or# (and# (uncheckedShiftL# b 2#) 0x3c##) (uncheckedShiftRL# c 6#)
        !z = and# c 0x3f##
     in (index w, index x, index y, index z)
  where
        index :: Word# -> Word8
        index idx = W8# (indexWord8OffAddr# table (word2Int# idx))

-- | Get the length needed for the destination buffer for a base64 decoding.
--
-- if the length is not a multiple of 4, Nothing is returned
unBase64Length :: Ptr Word8 -> Int -> IO (Maybe Int)
unBase64Length src len
    | len < 1            = return $ Just 0
    | (len `mod` 4) /= 0 = return Nothing
    | otherwise          = do
        last1Byte <- peekByteOff src (len - 1)
        last2Byte <- peekByteOff src (len - 2)
        let dstLen = if last1Byte == eqAscii
                        then if last2Byte == eqAscii then 2 else 1
                        else 0
        return $ Just $ (len `div` 4) * 3 - dstLen
  where
        eqAscii :: Word8
        eqAscii = fromIntegral (fromEnum '=')

-- | Get the length needed for the destination buffer for an
-- <http://tools.ietf.org/html/rfc4648#section-3.2 unpadded> base64 decoding.
--
-- If the length of the encoded string is a multiple of 4, plus one, Nothing is
-- returned. Any other value can be valid without padding.
unBase64LengthUnpadded :: Int -> Maybe Int
unBase64LengthUnpadded len = case r of
    0 -> Just (3*q)
    2 -> Just (3*q + 1)
    3 -> Just (3*q + 2)
    _ -> Nothing
  where (q, r) = len `divMod` 4

fromBase64OpenBSD :: Ptr Word8 -> Ptr Word8 -> Int -> IO (Maybe Int)
fromBase64OpenBSD dst src len = fromBase64Unpadded rsetOpenBSD dst src len

fromBase64URLUnpadded :: Ptr Word8 -> Ptr Word8 -> Int -> IO (Maybe Int)
fromBase64URLUnpadded dst src len = fromBase64Unpadded rsetURL dst src len

fromBase64Unpadded :: (Word8 -> Word8) -> Ptr Word8 -> Ptr Word8 -> Int -> IO (Maybe Int)
fromBase64Unpadded rset dst src len = loop 0 0
  where loop di i
            | i == len       = return Nothing
            | i == len - 1   = return Nothing -- Shouldn't happen if len is valid
            | i == len - 2   = do
                a <- peekByteOff src i
                b <- peekByteOff src (i+1)

                case decode2 a b of
                    Left ofs -> return $ Just (i + ofs)
                    Right x  -> do
                        pokeByteOff dst di x
                        return Nothing
            | i == len - 3   = do
                a <- peekByteOff src i
                b <- peekByteOff src (i+1)
                c <- peekByteOff src (i+2)

                case decode3 a b c of
                    Left ofs    -> return $ Just (i + ofs)
                    Right (x,y) -> do
                        pokeByteOff dst di     x
                        pokeByteOff dst (di+1) y
                        return Nothing
            | otherwise      = do
                a <- peekByteOff src i
                b <- peekByteOff src (i+1)
                c <- peekByteOff src (i+2)
                d <- peekByteOff src (i+3)

                case decode4 a b c d of
                    Left ofs      -> return $ Just (i + ofs)
                    Right (x,y,z) -> do
                        pokeByteOff dst di     x
                        pokeByteOff dst (di+1) y
                        pokeByteOff dst (di+2) z
                        loop (di + 3) (i + 4)

        decode2 :: Word8 -> Word8 -> Either Int Word8
        decode2 a b =
            case (rset a, rset b) of
                (0xff, _   ) -> Left 0
                (_   , 0xff) -> Left 1
                (ra  , rb  ) -> Right ((ra `unsafeShiftL` 2) .|. (rb `unsafeShiftR` 4))

        decode3 :: Word8 -> Word8 -> Word8 -> Either Int (Word8, Word8)
        decode3 a b c =
            case (rset a, rset b, rset c) of
                (0xff, _   , _   ) -> Left 0
                (_   , 0xff, _   ) -> Left 1
                (_   , _   , 0xff) -> Left 2
                (ra  , rb  , rc  ) ->
                    let x = (ra `unsafeShiftL` 2) .|. (rb `unsafeShiftR` 4)
                        y = (rb `unsafeShiftL` 4) .|. (rc `unsafeShiftR` 2)
                     in Right (x,y)


        decode4 :: Word8 -> Word8 -> Word8 -> Word8 -> Either Int (Word8, Word8, Word8)
        decode4 a b c d =
            case (rset a, rset b, rset c, rset d) of
                (0xff, _   , _   , _   ) -> Left 0
                (_   , 0xff, _   , _   ) -> Left 1
                (_   , _   , 0xff, _   ) -> Left 2
                (_   , _   , _   , 0xff) -> Left 3
                (ra  , rb  , rc  , rd  ) ->
                    let x = (ra `unsafeShiftL` 2) .|. (rb `unsafeShiftR` 4)
                        y = (rb `unsafeShiftL` 4) .|. (rc `unsafeShiftR` 2)
                        z = (rc `unsafeShiftL` 6) .|. rd
                     in Right (x,y,z)

rsetURL :: Word8 -> Word8
rsetURL (W8# w)
    | booleanPrim (w `leWord#` 0xff##) = W8# (indexWord8OffAddr# rsetTable (word2Int# w))
    | otherwise                        = 0xff
  where !rsetTable = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x3e\xff\xff\
                     \\x34\x35\x36\x37\x38\x39\x3a\x3b\x3c\x3d\xff\xff\xff\xff\xff\xff\
                     \\xff\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\
                     \\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\xff\xff\xff\xff\x3f\
                     \\xff\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\
                     \\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

rsetOpenBSD :: Word8 -> Word8
rsetOpenBSD (W8# w)
    | booleanPrim (w `leWord#` 0xff##) = W8# (indexWord8OffAddr# rsetTable (word2Int# w))
    | otherwise                        = 0xff
  where !rsetTable = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x01\
                     \\x36\x37\x38\x39\x3a\x3b\x3c\x3d\x3e\x3f\xff\xff\xff\xff\xff\xff\
                     \\xff\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\
                     \\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\xff\xff\xff\xff\xff\
                     \\xff\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\
                     \\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#


-- | convert from base64 in @src@ to binary in @dst@, using the number of bytes specified
--
-- the user should use unBase64Length to compute the correct length, or check that
-- the length specification is proper. no check is done here.
fromBase64 :: Ptr Word8 -> Ptr Word8 -> Int -> IO (Maybe Int)
fromBase64 dst src len
    | len == 0  = return Nothing
    | otherwise = loop 0 0
  where loop di i
            | i == (len-4) = do
                a <- peekByteOff src i
                b <- peekByteOff src (i+1)
                c <- peekByteOff src (i+2)
                d <- peekByteOff src (i+3)

                let (nbBytes, c',d') =
                        case (c,d) of
                            (0x3d, 0x3d) -> (2, 0x30, 0x30)
                            (0x3d, _   ) -> (0, c, d) -- invalid: automatically 'c' will make it error out
                            (_   , 0x3d) -> (1, c, 0x30)
                            (_   , _   ) -> (0 :: Int, c, d)
                case decode4 a b c' d' of
                    Left ofs -> return $ Just (i + ofs)
                    Right (x,y,z) -> do
                        pokeByteOff dst di x
                        when (nbBytes < 2) $ pokeByteOff dst (di+1) y
                        when (nbBytes < 1) $ pokeByteOff dst (di+2) z
                        return Nothing
            | otherwise    = do
                a <- peekByteOff src i
                b <- peekByteOff src (i+1)
                c <- peekByteOff src (i+2)
                d <- peekByteOff src (i+3)

                case decode4 a b c d of
                    Left ofs      -> return $ Just (i + ofs)
                    Right (x,y,z) -> do
                        pokeByteOff dst di     x
                        pokeByteOff dst (di+1) y
                        pokeByteOff dst (di+2) z
                        loop (di + 3) (i + 4)

        decode4 :: Word8 -> Word8 -> Word8 -> Word8 -> Either Int (Word8, Word8, Word8)
        decode4 a b c d =
            case (rset a, rset b, rset c, rset d) of
                (0xff, _   , _   , _   ) -> Left 0
                (_   , 0xff, _   , _   ) -> Left 1
                (_   , _   , 0xff, _   ) -> Left 2
                (_   , _   , _   , 0xff) -> Left 3
                (ra  , rb  , rc  , rd  ) ->
                    let x = (ra `unsafeShiftL` 2) .|. (rb `unsafeShiftR` 4)
                        y = (rb `unsafeShiftL` 4) .|. (rc `unsafeShiftR` 2)
                        z = (rc `unsafeShiftL` 6) .|. rd
                     in Right (x,y,z)

        rset :: Word8 -> Word8
        rset (W8# w)
            | booleanPrim (w `leWord#` 0xff##) = W8# (indexWord8OffAddr# rsetTable (word2Int# w))
            | otherwise                        = 0xff

        !rsetTable = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x3e\xff\xff\xff\x3f\
                     \\x34\x35\x36\x37\x38\x39\x3a\x3b\x3c\x3d\xff\xff\xff\xff\xff\xff\
                     \\xff\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\
                     \\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\xff\xff\xff\xff\xff\
                     \\xff\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\
                     \\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#
