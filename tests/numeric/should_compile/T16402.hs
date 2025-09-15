{-# OPTIONS_GHC -ddump-simpl -dhex-word-literals -dsuppress-all -dsuppress-uniques -O2 #-}
{-# LANGUAGE TypeApplications #-}
module T16402 where

import Data.Word
import Data.Int
import Data.Bits

smallWord_foo :: Word64 -> Word64
smallWord_foo x = fromIntegral @Word16 $ fromIntegral (x .&. 0xFFFF)

smallWord_bar :: Word64 -> Word64
smallWord_bar x = fromIntegral (fromIntegral x :: Word16)

smallInt_foo :: Int64 -> Int64
smallInt_foo x = fromIntegral @Int16 $ fromIntegral (x .&. 0x12FFFF)

smallInt_bar :: Int64 -> Int64
smallInt_bar x = fromIntegral (fromIntegral x :: Int16)
