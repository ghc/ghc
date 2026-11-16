{-# LANGUAGE BangPatterns #-}

module Codec.Serialise.Internal.GeneralisedUTF8
    ( encodeGenUTF8
    , UTF8Encoding(..)
    , decodeGenUTF8
      -- * Utilities
    , isSurrogate
    , isValid
    ) where

import Control.Monad.ST
import Data.Bits
import Data.Char
import Data.Word
import qualified Codec.CBOR.ByteArray.Sliced as BAS
import Data.Primitive.ByteArray

data UTF8Encoding = ConformantUTF8 | GeneralisedUTF8
                  deriving (Show, Eq)

-- | Is a 'Char' a UTF-16 surrogate?
isSurrogate :: Char -> Bool
isSurrogate c = c >= '\xd800' && c <= '\xdfff'

-- | Encode a string as (generalized) UTF-8. In addition to the encoding, we
-- return a flag indicating whether the encoded string contained any surrogate
-- characters, in which case the output is generalized UTF-8.
encodeGenUTF8 :: String -> (BAS.SlicedByteArray, UTF8Encoding)
encodeGenUTF8 st = runST $ do
    -- We slightly over-allocate such that we won't need to copy in the
    -- ASCII-only case.
    ba <- newByteArray (length st + 4)
    go ba ConformantUTF8 0 st
  where
    go :: MutableByteArray s -> UTF8Encoding
       -> Int -> [Char]
       -> ST s (BAS.SlicedByteArray, UTF8Encoding)
    go ba !enc !off  [] = do
        ba' <- unsafeFreezeByteArray ba
        return (BAS.SBA ba' 0 off, enc)
    go ba enc off  (c:cs)
      | off + 4 >= cap = do
        -- We ran out of room; reallocate and copy
        ba' <- newByteArray (cap + cap `div` 2 + 1)
        copyMutableByteArray ba' 0 ba 0 off
        go ba' enc off (c:cs)

      | c >= '\x10000' = do
        writeByteArray ba (off+0) (0xf0 .|. (0x07 .&. shiftedByte 18))
        writeByteArray ba (off+1) (0x80 .|. (0x3f .&. shiftedByte 12))
        writeByteArray ba (off+2) (0x80 .|. (0x3f .&. shiftedByte  6))
        writeByteArray ba (off+3) (0x80 .|. (0x3f .&. shiftedByte  0))
        go ba enc (off+4) cs

      | c >= '\x0800'  = do
        writeByteArray ba (off+0) (0xe0 .|. (0x0f .&. shiftedByte 12))
        writeByteArray ba (off+1) (0x80 .|. (0x3f .&. shiftedByte  6))
        writeByteArray ba (off+2) (0x80 .|. (0x3f .&. shiftedByte  0))

        -- Is this a surrogate character?
        let enc'
              | isSurrogate c = GeneralisedUTF8
              | otherwise     = enc
        go ba enc' (off+3) cs

      | c >= '\x0080'  = do
        writeByteArray ba (off+0) (0xc0 .|. (0x1f .&. shiftedByte  6))
        writeByteArray ba (off+1) (0x80 .|. (0x3f .&. shiftedByte  0))
        go ba enc (off+2) cs

      | c <= '\x007f'  = do
        writeByteArray ba off (fromIntegral n :: Word8)
        go ba enc (off+1) cs

      | otherwise      = error "encodeGenUTF8: Impossible"
      where
        cap = sizeofMutableByteArray ba
        n = ord c
        shiftedByte :: Int -> Word8
        shiftedByte shft = fromIntegral $ n `shiftR` shft

decodeGenUTF8 :: ByteArray -> String
decodeGenUTF8 ba = go 0
  where
    !len = sizeofByteArray ba

    index :: Int -> Int
    index i = fromIntegral (ba `indexByteArray` i :: Word8)

    go !off
      | off == len = []

      | n0 .&. 0xf8 == 0xf0 =
        let n1 = index (off + 1)
            n2 = index (off + 2)
            n3 = index (off + 3)
            c  = chr $  (n0 .&. 0x07) `shiftL` 18
                    .|. (n1 .&. 0x3f) `shiftL` 12
                    .|. (n2 .&. 0x3f) `shiftL`  6
                    .|. (n3 .&. 0x3f)
        in c : go (off + 4)

      | n0 .&. 0xf0 == 0xe0 =
        let n1 = index (off + 1)
            n2 = index (off + 2)
            c  = chr $  (n0 .&. 0x0f) `shiftL` 12
                    .|. (n1 .&. 0x3f) `shiftL`  6
                    .|. (n2 .&. 0x3f)
        in c : go (off + 3)

      | n0 .&. 0xe0 == 0xc0 =
        let n1 = index (off + 1)
            c  = chr $  (n0 .&. 0x1f) `shiftL`  6
                    .|. (n1 .&. 0x3f)
        in c : go (off + 2)

      | otherwise =
        let c =  chr $  (n0 .&. 0x7f)
        in c : go (off + 1)
      where !n0 = index off

-- | Is the given byte sequence valid under the given encoding?
isValid :: UTF8Encoding -> [Word8] -> Bool
isValid encoding = go
  where
    go [] = True
    go (b0:bs)
      | inRange 0x00 0x7f b0 = go bs
    go (b0:b1:bs)
      | inRange 0xc2 0xdf b0
      , inRange 0x80 0xbf b1 = go bs
    go (0xe0:b1:b2:bs)
      | inRange 0xa0 0xbf b1
      , inRange 0x80 0xbf b2 = go bs
    go (0xed:b1:_)
      -- surrogate range
      | encoding == ConformantUTF8
      , inRange 0xa0 0xbf b1
      = False
    go (b0:b1:b2:bs)
      | inRange 0xe1 0xef b0
      , inRange 0x80 0xbf b1
      , inRange 0x80 0xbf b2 = go bs
    go (0xf0:b1:b2:b3:bs)
      | inRange 0x90 0xbf b1
      , inRange 0x80 0xbf b2
      , inRange 0x80 0xbf b3 = go bs
    go (b0:b1:b2:b3:bs)
      | inRange 0xf1 0xf3 b0
      , inRange 0x80 0xbf b1
      , inRange 0x80 0xbf b2
      , inRange 0x80 0xbf b3 = go bs
    go (0xf4:b1:b2:b3:bs)
      | inRange 0x80 0x8f b1
      , inRange 0x80 0xbf b2
      , inRange 0x80 0xbf b3 = go bs
    go _ = False

inRange :: Ord a => a -> a -> a -> Bool
inRange lower upper x = lower <= x && x <= upper
