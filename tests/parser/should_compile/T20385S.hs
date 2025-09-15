{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

main = pure ()

foo :: forall a. a -> a
foo x = mdo (x :: a)
