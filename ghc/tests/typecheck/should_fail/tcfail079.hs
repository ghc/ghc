{-# OPTIONS -fglasgow-exts #-}

module ShouldFail where

-- !!! unboxed field in newtype declaration

import GlaExts ( Int# )

newtype Unboxed = Unboxed Int#

f = [ Unboxed 1#, Unboxed 2# ] -- shouldn't be allowed!
