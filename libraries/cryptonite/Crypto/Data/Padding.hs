-- |
-- Module      : Crypto.Data.Padding
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Various cryptographic padding commonly used for block ciphers
-- or asymmetric systems.
--
module Crypto.Data.Padding
    ( Format(..)
    , pad
    , unpad
    ) where

import           Data.ByteArray (ByteArray, Bytes)
import qualified Data.ByteArray as B

-- | Format of padding
data Format =
      PKCS5     -- ^ PKCS5: PKCS7 with hardcoded size of 8
    | PKCS7 Int -- ^ PKCS7 with padding size between 1 and 255
    | ZERO Int  -- ^ zero padding with block size
    deriving (Show, Eq)

-- | Apply some pad to a bytearray
pad :: ByteArray byteArray => Format -> byteArray -> byteArray
pad  PKCS5     bin = pad (PKCS7 8) bin
pad (PKCS7 sz) bin = bin `B.append` paddingString
  where
    paddingString = B.replicate paddingByte (fromIntegral paddingByte)
    paddingByte   = sz - (B.length bin `mod` sz)
pad (ZERO sz)  bin = bin `B.append` paddingString
  where
    paddingString = B.replicate paddingSz 0
    paddingSz
      | len == 0   =  sz
      | m == 0     =  0
      | otherwise  =  sz - m
    m = len `mod` sz
    len = B.length bin

-- | Try to remove some padding from a bytearray.
unpad :: ByteArray byteArray => Format -> byteArray -> Maybe byteArray
unpad  PKCS5     bin = unpad (PKCS7 8) bin
unpad (PKCS7 sz) bin
    | len == 0                           = Nothing
    | (len `mod` sz) /= 0                = Nothing
    | paddingSz < 1 || paddingSz > len   = Nothing
    | paddingWitness `B.constEq` padding = Just content
    | otherwise                          = Nothing
  where
    len         = B.length bin
    paddingByte = B.index bin (len - 1)
    paddingSz   = fromIntegral paddingByte
    (content, padding) = B.splitAt (len - paddingSz) bin
    paddingWitness     = B.replicate paddingSz paddingByte :: Bytes
unpad (ZERO sz)  bin
    | len == 0                           = Nothing
    | (len `mod` sz) /= 0                = Nothing
    | B.index bin (len - 1) /= 0         = Just bin
    | otherwise                          = Nothing
  where
    len         = B.length bin
