{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -O0 #-}

import GHC.Exts
import GHC.Num.BigNat  (bigNatCompareWord#, bigNatFromWord#)
import GHC.Num.Integer (integerGcd)

main :: IO ()
main = do
   let
      x = noinline (14205695611797621937 :: Integer)
      y = noinline (2 :: Word)
   print (integerGcd x (toInteger y))
   print (toInteger (gcd (fromInteger x) y :: Word))

   let
      x@(W# x#) = 1 :: Word
      !x'       = bigNatFromWord# x#
   print (bigNatCompareWord# x' x#)
   print (compare x x)
