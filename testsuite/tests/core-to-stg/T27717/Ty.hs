{-# LANGUAGE UndecidableSuperClasses #-}
module Ty where

data T a = MkT a

class CB a => CA a where
  opA :: a -> Int

class CA a => CB a where
  opB :: a -> Int
