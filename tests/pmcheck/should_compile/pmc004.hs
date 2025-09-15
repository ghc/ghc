{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}
{-# LANGUAGE GADTs #-}

module PMC004 where

data F a where
  F1 :: F Int
  F2 :: F Bool

data G a where
  G1 :: G Int
  G2 :: G Char

h :: F a -> G a -> ()
h F1 G1 = ()
h _  G1 = ()
