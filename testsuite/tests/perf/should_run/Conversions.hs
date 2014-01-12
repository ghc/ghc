{-# LANGUAGE BangPatterns #-}

-- | Tests that conversions between various primitive types (e.g.
-- Word, Double, etc) doesn't allocate.
module Main (main) where

import Data.Word

-- Repeatedly convert Words to Doubles
loop :: Floating a => Word -> a
loop n = go 0 0.0
  where
    go i !acc | i < n = go (i+1) (acc + fromIntegral i)
              | otherwise = acc
{-# SPECIALISE loop :: Word -> Float #-}
{-# SPECIALISE loop :: Word -> Double #-}

main :: IO ()
main = do
    print (loop 1000000 :: Float)
    print (loop 1000000 :: Double)
