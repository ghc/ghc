{-# OPTIONS -fglasgow-exts #-}

module ShouldFail where

import PrelGHC

die :: Int -> ByteArray#
die _ = undefined
