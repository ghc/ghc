module Foo where

f x = (g, g)
  where
    g :: Int -> IO Int
    g y = do { if y>2 then print x else return ()
             ; foogle y }

foogle :: Int -> IO Int
{-# NOINLINE foogle #-}
foogle n = return n
