{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -O #-}

module Main where

import GHC.Exts

main = pure ()

test :: Word8# -> Word8#
test x = x `plusWord8#` narrowWord8# 1##
