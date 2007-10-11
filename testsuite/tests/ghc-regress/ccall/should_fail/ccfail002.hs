{-# OPTIONS -fglasgow-exts -fffi #-}

-- Test for Trac #1680

module ShouldFail where

import GHC.Exts

foreign import ccall unsafe "foo" 
	foo :: Int# -> Int# -> Int# -> (# Int# , Int#, Int# #)
