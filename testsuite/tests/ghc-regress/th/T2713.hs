{-# LANGUAGE TemplateHaskell #-}
module Fixity where

class MyClass a where
    (.*.) :: a -> a -> a

$( [d| x = undefined |] )

infixr 3 .*.
