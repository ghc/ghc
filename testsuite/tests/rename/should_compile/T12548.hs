{-# OPTIONS_GHC -Wunused-binds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Foo (data P) where

-- x is used!!
x :: Int
x = 0

pattern P :: Int
pattern P <- _ where
        P = x
