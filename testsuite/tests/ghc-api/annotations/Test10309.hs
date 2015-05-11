{-# LANGUAGE GADTs #-}
module Test10309 where

data H1 a b where
  C3 :: (Num a) => { field :: a -- ^ hello docs
                   } -> H1 Int Int
