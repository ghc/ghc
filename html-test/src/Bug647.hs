{-# LANGUAGE Haskell2010 #-}
module Bug647 where

class Bug647 a where
  f :: a -- ^ doc for arg1
    -> a -- ^ doc for arg2
    -> a -- ^ doc for arg3