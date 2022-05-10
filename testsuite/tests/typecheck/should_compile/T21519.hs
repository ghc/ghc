{-# LANGUAGE MagicHash, UnboxedTuples #-}

module T21519 where

import GHC.Exts

(# #) = (# #)

g1 :: Bool -> (# Int #)
g1 = g1

f1 x =  let (# a #) = g1 True in a

g2 :: Bool -> (#  #)
g2 = g2

f2 x =  let (#  #) = g2 True in True

