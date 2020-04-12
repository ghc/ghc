module T17722A (Validation(..)) where

data Validation e a
  = Failure e
  | Success a

instance Functor (Validation e) where
   fmap _ (Failure e) = Failure e
   fmap f (Success a) = Success (f a)

(<.>) :: Semigroup e => Validation e (t -> a) -> Validation e t -> Validation e a
Failure e1 <.> b = Failure $ case b of
  Failure e2 -> e1 <> e2
  Success _  -> e1
Success _  <.> Failure e  = Failure  e
Success f  <.> Success x  = Success (f x)

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  (<*>) = (<.>)
