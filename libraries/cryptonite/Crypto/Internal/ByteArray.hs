-- |
-- Module      : Crypto.Internal.ByteArray
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
-- Simple and efficient byte array types
--
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Crypto.Internal.ByteArray
    ( module Data.ByteArray
    , module Data.ByteArray.Mapping
    , module Data.ByteArray.Encoding
    , constAllZero
    ) where

import Data.ByteArray
import Data.ByteArray.Mapping
import Data.ByteArray.Encoding

import Data.Bits ((.|.))
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)

import Crypto.Internal.Compat (unsafeDoIO)

constAllZero :: ByteArrayAccess ba => ba -> Bool
constAllZero b = unsafeDoIO $ withByteArray b $ \p -> loop p 0 0
  where
    loop :: Ptr b -> Int -> Word8 -> IO Bool
    loop p i !acc
        | i == len  = return $! acc == 0
        | otherwise = do
            e <- peekByteOff p i
            loop p (i+1) (acc .|. e)
    len = Data.ByteArray.length b
