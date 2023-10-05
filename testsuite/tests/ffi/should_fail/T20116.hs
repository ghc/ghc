{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foo where

import GHC.Exts

foreign import prim "test_lt" lt_s :: Int64# -> Int64# -> Int#
