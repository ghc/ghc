{-# OPTIONS_GHC -XScopedTypeVariables #-}

module Foo where

foo = let c = \ x :: a -> (x :: a) in co
