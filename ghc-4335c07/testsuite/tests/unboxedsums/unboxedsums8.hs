{-# LANGUAGE UnboxedSums, UnboxedTuples, MagicHash #-}

module Main where

import GHC.Prim
import GHC.Types

type Sum1 = (# (# Int#, Int #) | (# Int#, Int# #) | (# Int, Int# #) #)

{-# NOINLINE showSum1 #-}
showSum1 :: Sum1 -> String
showSum1 (# p1 | | #) = showP1 p1
showSum1 (# | p2 | #) = showP2 p2
showSum1 (# | | p3 #) = showP3 p3

{-# NOINLINE showP1 #-}
showP1 :: (# Int#, Int #) -> String
showP1 (# i1, i2 #) = show (I# i1) ++ show i2

{-# NOINLINE showP2 #-}
showP2 :: (# Int#, Int# #) -> String
showP2 (# i1, i2 #) = show (I# i1) ++ show (I# i2)

{-# NOINLINE showP3 #-}
showP3 :: (# Int, Int# #) -> String
showP3 (# i1, i2 #) = show i1 ++ show (I# i2)

main :: IO ()
main = do
    putStrLn (showSum1 s1)
    putStrLn (showSum1 s2)
    putStrLn (showSum1 s3)
  where
    s1, s2, s3 :: Sum1
    s1 = (# (# 123#, 456 #) | | #)
    s2 = (# | (# 876#, 543# #) | #)
    s3 = (# | | (# 123, 456# #) #)
