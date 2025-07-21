{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module T26116 where

data T a = MkT a

instance Eq (T a) where
    x == y = True

class (forall b. Eq (T b)) => D a where { dop :: a -> a }

class C f a where { op1,op2 :: f a -> Int }

instance (Eq (f a), D a) => C f a where
    op1 x | x==x      = 3
          | otherwise = 4
    {-# SPECIALISE instance D a => C T a #-}
