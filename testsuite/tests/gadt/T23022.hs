{-# LANGUAGE TypeData #-}
module B where

type data T a b where
  MkT :: T a a

f :: T a b -> T a b
f x = x
