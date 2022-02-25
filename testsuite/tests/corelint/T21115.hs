{-# LANGUAGE MagicHash #-}

module T21115 where

import GHC.Exts (Double#, Int#)

foo :: Double# -> Int#
foo x =
  case x of
    0.0##  -> 2#
    2.0##  -> 3#
    -0.0## -> 4#
    _      -> 5#
