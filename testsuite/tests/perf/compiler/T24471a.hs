{-# LANGUAGE TemplateHaskell #-}
module T24471a where

data List_ a f = Nil_ | Cons_ a f deriving Functor

between alg a b
  | a == b = [|| $$alg Nil_ ||]
  | otherwise = [|| $$alg (Cons_ a $$(between alg (a + 1) b)) ||]
