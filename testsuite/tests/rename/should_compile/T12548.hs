{-# OPTIONS_GHC -Wunused-binds #-}
{-# LANGUAGE PatternSynonyms #-}

module Foo (pattern P) where

-- x is used!!
x :: Int
x = 0

pattern P :: Int
pattern P <- _ where
        P = x
