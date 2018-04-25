{-# LANGUAGE PartialTypeSignatures #-}
module PartialClassMethodSignature where


class Foo a where
  foo :: a -> _
