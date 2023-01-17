{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE GADTs #-}

module Main where

data G a where
  G1 :: Num a => G a
  G2 :: Num a => G a
  G3 :: Num a => G a

bar :: G a -> a
bar (one of G2, G1) = 3

data GADT a where
    IsInt1 :: GADT Int
    IsInt2 :: GADT Int

foo :: a -> GADT a -> a
foo x (one of IsInt1 {}, IsInt2 {}) = x + 1

f x = case x of
  (one of Left a, Right a) -> a

g x = case x of
  (one of _, (one of _, x)) -> x

main = print $ foo 3 IsInt1