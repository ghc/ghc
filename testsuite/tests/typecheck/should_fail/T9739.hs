module T9739 where

class Class2 a => Class1 a where
  class3 :: (Class2 a) => b

class (Class1 a) => Class2 a where
