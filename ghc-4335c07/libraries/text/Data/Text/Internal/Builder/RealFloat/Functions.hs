{-# LANGUAGE CPP #-}

-- |
-- Module:    Data.Text.Internal.Builder.RealFloat.Functions
-- Copyright: (c) The University of Glasgow 1994-2002
-- License:   see libraries/base/LICENSE
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!

module Data.Text.Internal.Builder.RealFloat.Functions
    (
      roundTo
    ) where

roundTo :: Int -> [Int] -> (Int,[Int])

#if MIN_VERSION_base(4,6,0)

roundTo d is =
  case f d True is of
    x@(0,_) -> x
    (1,xs)  -> (1, 1:xs)
    _       -> error "roundTo: bad Value"
 where
  b2 = base `quot` 2

  f n _ []     = (0, replicate n 0)
  f 0 e (x:xs) | x == b2 && e && all (== 0) xs = (0, [])   -- Round to even when at exactly half the base
               | otherwise = (if x >= b2 then 1 else 0, [])
  f n _ (i:xs)
     | i' == base = (1,0:ds)
     | otherwise  = (0,i':ds)
      where
       (c,ds) = f (n-1) (even i) xs
       i'     = c + i
  base = 10

#else

roundTo d is =
  case f d is of
    x@(0,_) -> x
    (1,xs)  -> (1, 1:xs)
    _       -> error "roundTo: bad Value"
 where
  f n []     = (0, replicate n 0)
  f 0 (x:_)  = (if x >= 5 then 1 else 0, [])
  f n (i:xs)
     | i' == 10  = (1,0:ds)
     | otherwise = (0,i':ds)
      where
       (c,ds) = f (n-1) xs
       i'     = c + i

#endif
