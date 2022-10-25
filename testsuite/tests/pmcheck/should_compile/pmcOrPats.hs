{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE OrPatterns #-}

module PmcOrPats where

data T = A | B
data U = V | W

g :: T -> U -> Int
g (A;B) V = 0
g B (V;W) = 1

h A (_;W) B = 0
h B (V;_) B = 1
h (A;B) _ B = 2

z (1;2;1) = 0
z (3;2) = 1
z 1 = 2

careful (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) False = 1
careful (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) (LT;GT) True  = 2
careful EQ      EQ      EQ      EQ      EQ      EQ      EQ      EQ      EQ      EQ      EQ      EQ      EQ      _     = 3
