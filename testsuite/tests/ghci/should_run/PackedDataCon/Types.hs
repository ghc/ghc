{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fobject-code #-}

module Types where

import GHC.Exts
import GHC.Int
import GHC.Word

data D1 = D1w8 Word8#
        | D1i8 Int8#
        | D1w16 Word16#
        | D1i16 Int16#
        | D1w32 Word32#
        | D1i32 Int32#

data D2 = D2a Word8# Word8#
        | D2b Word8# Word16#
        | D2c Word8# Word16# Word8#
        | D2d Word# Word8# Word#
        | D2e Word# Int8# Double# Float# Word8#
