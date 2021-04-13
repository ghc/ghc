module Main (main) where

import Data.Foldable (foldlM)

f :: Int -> IO Int
f n = foldlM (\a b -> a `seq` pure (a + b)) 0 (filter even [1..n])
{-# NOINLINE f #-}

main :: IO ()
main = f 1000000 >> pure ()
