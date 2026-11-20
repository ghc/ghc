{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module Vector (Nat(..), Vec(..)) where

data Nat = Z | S Nat
data Vec (n :: Nat) a where
    Nil  :: Vec 'Z a
    Cons :: a -> Vec n a -> Vec ('S n) a

instance Foldable (Vec n) where
    foldr _ z Nil = z
    foldr f z (Cons x xs) = f x (foldr f z xs)
