{-# OPTIONS_GHC -XScopedTypeVariables -XPatternSignatures #-}

module Foo where

foo = let c = \ x :: a -> (x :: a) in co
