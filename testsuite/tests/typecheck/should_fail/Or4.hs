{-# LANGUAGE OrPatterns, GADTs #-}

module Main where

data G a where
  G1 :: Num a => G a
  G2 :: Num a => G a
  G3 :: Num a => G a

bar :: G a -> a
bar (G2; G1) = 3

data GADT a where
    IsInt1 :: GADT Int
    IsInt2 :: GADT Int

foo :: a -> GADT a -> a
foo x (IsInt1 {}; IsInt2 {}) = x + 1