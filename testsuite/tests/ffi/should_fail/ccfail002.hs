
{-# LANGUAGE ForeignFunctionInterface, UnboxedTuples, MagicHash, UnliftedFFITypes #-}

-- Test for #1680

module ShouldFail where

import GHC.Exts

foreign import ccall unsafe "foo"
        foo :: Int# -> Int# -> Int# -> (# Int# , Int#, Int# #)
