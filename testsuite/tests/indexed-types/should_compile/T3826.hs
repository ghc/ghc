{-# LANGUAGE TypeFamilies #-}

module T3826 where

class C a where
  type E a
  c :: E a -> a -> a

data T a = T a

instance C (T a) where
  type E (T a) = a
  c x (T _) = T x

f t@(T x) = c x t
