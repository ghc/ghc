{-# LANGUAGE ScopedTypeVariables #-}

module T17687 where

x :: forall a -> a -> a
x = x
