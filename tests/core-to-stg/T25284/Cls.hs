{-# LANGUAGE UndecidableInstances #-}

module Cls where

class HasConst a where constVal :: a

instance Cls.HasConst Word where constVal = 123

instance Cls.HasConst Int where constVal = 456

-- this class has a big dictionary
class HasConst10 a where
  constA :: a
  constInt1 :: a -> Int
  constInt1 _ = 1
  constInt2 :: a -> Int
  constInt2 _ = 2
  constInt3 :: a -> Int
  constInt3 _ = 3
  constInt4 :: a -> Int
  constInt4 _ = 4
  constInt5 :: a -> Int
  constInt5 _ = 5
  constInt6 :: a -> Int
  constInt6 _ = 6
  constInt7 :: a -> Int
  constInt7 _ = 7
  constInt8 :: a -> Int
  constInt8 _ = 8
  constInt9 :: a -> Int
  constInt9 _ = 9

instance HasConst a => HasConst10 a where
    constA = constVal

-- this doesn't use the big dictionary most of the time
printConst :: forall a. (Show a, HasConst10 a)
           => a -> Int -> IO ()
printConst x 5000  = print @a constA >> print (constInt8 x)
printConst _  _    = pure ()
