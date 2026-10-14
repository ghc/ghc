{-# LANGUAGE BangPatterns #-}
-- Two word-to-float conversions in one Cmm block, in a loop where the register
-- assignment differs at the back edge. The x86-64 NCG used to emit an
-- out-of-sync CFG and panic with "Can't find weight for edge that should have
-- one" (#27915).
module Main where

go :: Word -> Word -> Word -> Double -> Double
go 0 _ _ !acc = acc
go n a b !acc = go (n-1) b a (acc + fromIntegral a * fromIntegral b)
{-# NOINLINE go #-}

goF :: Word -> Word -> Word -> Float -> Float
goF 0 _ _ !acc = acc
goF n a b !acc = goF (n-1) b a (acc + fromIntegral a * fromIntegral b)
{-# NOINLINE goF #-}

main :: IO ()
main = do
  print (go 10 3 maxBound 0)
  print (goF 10 3 maxBound 0)
