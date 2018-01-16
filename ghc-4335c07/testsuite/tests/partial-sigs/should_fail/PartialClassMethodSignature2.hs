{-# LANGUAGE PartialTypeSignatures #-}
module PartialClassMethodSignature2 where

class Foo a where
  foo :: (Eq a, _) => a -> a
