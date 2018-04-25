{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Main ( main ) where

class Foldable f => C f a where
  m :: f a -> a

instance C [] Int where
  m = foldr (+) 0

{-# NOINLINE go #-}
go :: C [] b => b -> Int -> Int
go _ i = foldr (+) 0 [1..i]

main :: IO ()
main = print $ go (0 :: Int) 20000
