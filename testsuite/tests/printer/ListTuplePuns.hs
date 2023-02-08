{-# language MagicHash, UnboxedTuples #-}

import GHC.Tuple
import GHC.Types

u0e :: Int
u0e =
  case False :: Unit# of
    (# #) -> 5

u1e :: Int
u1e =
  case False :: Solo# Int of
    (# _ #) -> 5

u2e :: Int
u2e =
  case False :: Tuple2# Int Double of
    (# _, _ #) -> 5

b0e :: Int
b0e =
  case False :: Unit of
    () -> 5

b1e :: Int
b1e =
  case False :: Solo Int of
    _ -> 5

b2e :: Int
b2e =
  case False :: Tuple2 Int Double of
    _ -> 5

s2e :: Int
s2e =
  case False :: Sum2# Int Double of
    (# _ | #) -> 5
