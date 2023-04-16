{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Foo where

class C a b c | b -> c where
  op :: a -> b -> c

bar1 :: C a Int Char => a -> Char
bar1 x = op x (3 :: Int) :: Char

bar2 x = op x (3 :: Int) :: Char
