{-# LANGUAGE BangPatterns #-}
module Test10358 where

mtGamma x v d =
  let !x_2 = x*x; !x_4 = x_2*x_2
      v3 = v*v*v
      dv = d * v3
  in 5
