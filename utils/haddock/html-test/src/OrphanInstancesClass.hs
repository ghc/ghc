{-# LANGUAGE Haskell2010 #-}
module OrphanInstancesClass (AClass(..)) where

class AClass a where
  aClass :: a -> Int
