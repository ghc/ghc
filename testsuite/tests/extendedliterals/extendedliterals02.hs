{-# LANGUAGE MagicHash, ExtendedLiterals #-}
{-# OPTIONS_GHC -fno-warn-overflowed-literals #-}

module Ex where

--import GHC.Exts
import GHC.Int

-- Overflowed 'Int8#' literals
exI8b1, exI8b2, exI8b3, exI8b4, exI8b5 :: Int8
exI8b1 = I8#  0x80#Int8
exI8b2 = I8# -0x81#Int8
exI8b3 = I8#  0xFF#Int8
exI8b4 = I8# -0xFF#Int8
exI8b5 = I8#  0xFFFFFFFFFFFFFFFF#Int8
