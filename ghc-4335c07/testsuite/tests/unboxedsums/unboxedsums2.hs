{-# LANGUAGE UnboxedSums, MagicHash, BangPatterns #-}

module Main where

import GHC.Prim
import GHC.Types

-- Code generator used to fail with illegal instruction errors when Float# is
-- involved.

toInt :: (# Int# | Float# #) -> Int#
toInt (# i | #) = i
toInt (# | f #) = let !(I# i) = ceiling (F# f) in i

toFloat :: (# Int# | Float# #) -> Float#
toFloat (# i | #) = let !(F# f) = fromIntegral (I# i) in f
toFloat (# | f #) = f

data D = D { f1 :: (# Int# | Float# #) }

instance Show D where
  show (D (# i | #)) = "D " ++ show (I# i)
  show (D (# | f #)) = "D " ++ show (F# f)

main :: IO ()
main = do
    !(F# f) <- readLn
    print (I# (toInt (# | f #)))

    !(I# i) <- readLn
    print (F# (toFloat (# i | #)))

    print (D (# | f #))
    print (D (# i | #))
