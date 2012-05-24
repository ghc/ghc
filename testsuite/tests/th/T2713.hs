{-# LANGUAGE TemplateHaskell #-}
module Fixity where

class MyClass a where
    (.*.) :: a -> a -> a

f x = x

$( [d| x = undefined |] )

infixr 3 .*.
f :: Int -> Int
