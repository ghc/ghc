{-# LANGUAGE UndecidableInstances #-}
module T7279 where

data T a = MkT

instance (Eq a, Show b) => Eq (T a)
