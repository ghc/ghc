{-# LANGUAGE NoIncomplete #-}

module A where

class C a where
  m1 :: a
  m2 :: a

instance C Bool where
  m1 = False
