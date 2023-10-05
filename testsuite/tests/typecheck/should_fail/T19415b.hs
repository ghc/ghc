{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Loopy where

data T a = MkT -- this needs to be poly-kinded

class Ping s t b | s -> t b where
  foo :: b -> s -> t

instance Ping (T a) (T b) Int => Ping (T a) (T b) Int where
  foo _ _ = MkT

loop = foo 'c' MkT
