{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Bug ( box, wrap, proxy ) where

import GHC.Prim

box :: (# Proxy# a, b #) -> b
box (# x, y #) = y

wrap :: b -> Proxy# a -> (# Proxy# a, b #)
wrap x = \i# -> (# i#, x #)

proxy :: () -> Proxy# a
proxy () = proxy#
