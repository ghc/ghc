{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoExperimentalDependentTypes, RankNTypes #-}
module Vector (Nat(..), Vec(..), consumeVector) where

data Nat = Z | S Nat
data Vec (n :: Nat) a where
    Nil  :: Vec 'Z a
    Cons :: a -> Vec n a -> Vec ('S n) a

instance Foldable (Vec n) where
    foldr _ z Nil = z
    foldr f z (Cons x xs) = f x (foldr f z xs)

-- | Call a reader that works at every length.
consumeVector :: (forall j. Vec j a -> r) -> Vec k a -> r
consumeVector readAny xs = readAny xs
