{-# LANGUAGE PatternSynonyms #-}

module T11955 ( A(..), pattern P ) where

data A a = A a

pattern P :: a -> A a
pattern P a <- A a
