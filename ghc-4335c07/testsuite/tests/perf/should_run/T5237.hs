{-# LANGUAGE BangPatterns #-}
module Main (main) where

-- Test that the rewrite rules for small exponents fire (#5237).
-- If they don't fire, this will allocate much.

fun :: Double -> Double
fun x = go 0 1.0
  where
    go !acc z
      | x < z   = acc
      | otherwise = go (acc + 1/z^4) (z+1.0)

main :: IO ()
main = print (fun 1e7)
