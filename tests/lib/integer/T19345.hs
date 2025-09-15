{-# OPTIONS_GHC -O #-}
module Main where

import Numeric.Natural
  ( Natural )

a, q :: Natural
a = fromIntegral ( 18446744073709551616 :: Integer )
q = 18446744073709551616

main :: IO ()
main = print ( fromIntegral ( a `div` q ) :: Word )
