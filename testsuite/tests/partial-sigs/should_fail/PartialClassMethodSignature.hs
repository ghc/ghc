{-# LANGUAGE PartialTypeSignatures #-}
module PartialClassMethodSignature where


class Foo a where
  foo :: (Eq a, _) => a -> _
