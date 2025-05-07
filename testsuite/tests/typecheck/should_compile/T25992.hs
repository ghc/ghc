{-# OPTIONS_GHC -Wredundant-constraints #-}

module T25992 where

data P a = P

instance Eq a => Semigroup (P a) where
  P <> P = P
