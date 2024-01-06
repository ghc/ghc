-- a bit simpler than T24296
module T24296b (r) where

f :: Int -> Int -> Int
f x = error "blah"

g :: (Int -> Int -> Int) -> (Int -> Int -> Int)
g f = f
{-# OPAQUE g #-}

r x y = g f y `seq` Just (g f x y)
{-# OPAQUE r #-}
