{-# OPTIONS -fglasgow-exts #-}

-- !!! Test top-level unboxed types

module ShouldFail where

import GHC.Base

x = 1#
