-- To test with GHC before liftA2 was added to the Applicative
-- class, remove the definition of liftA2 here, and import
-- liftA2 separately from Control.Applicative.
{-# LANGUAGE DeriveTraversable, GADTs, DataKinds,
    DeriveFunctor, StandaloneDeriving #-}

module Main where
import Control.Applicative (Applicative (..))
import Data.Monoid (Sum (..))
import qualified Data.Array as A

data Tree a = Leaf a a | Node (Tree a) (Tree a)
  deriving (Functor, Foldable, Traversable)

buildTree :: Int -> a -> Tree a
buildTree 0 a = Leaf a a
buildTree n a =
  let subtree = buildTree (n - 1) a
  in Node subtree subtree

data Nat = Z | S Nat

data Vec n a where
  Nil :: Vec 'Z a
  Cons :: a -> !(Vec n a) -> Vec ('S n) a

deriving instance Functor (Vec n)
deriving instance Foldable (Vec n)
deriving instance Show a => Show (Vec n a)

class Pure n where
  pure' :: a -> Vec n a
instance Pure 'Z where
  pure' _ = Nil
instance Pure n => Pure ('S n) where
  pure' a = Cons a (pure' a)

instance Pure n => Applicative (Vec n) where
  pure = pure'
  (<*>) = apVec
  liftA2 = liftA2Vec

apVec :: Vec n (a -> b) -> Vec n a -> Vec n b
apVec Nil Nil = Nil
apVec (Cons f fs) (Cons x xs) = f x `Cons` apVec fs xs

liftA2Vec :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
liftA2Vec _ Nil Nil = Nil
liftA2Vec f (Cons x xs) (Cons y ys) = f x y `Cons` liftA2Vec f xs ys

data SomeVec a where
  SomeVec :: Pure n => Vec n a -> SomeVec a

replicateVec :: Int -> a -> SomeVec a
replicateVec 0 _ = SomeVec Nil
replicateVec n a =
  case replicateVec (n - 1) a of
    SomeVec v -> SomeVec (a `Cons` v)

ones :: SomeVec Int
ones = replicateVec 6000 (1 :: Int)

theTree :: Tree ()
theTree = buildTree 7 ()

blah :: SomeVec (Tree Int)
blah = case ones of
         SomeVec v -> SomeVec $ traverse (const v) theTree

main = case blah of
         SomeVec v -> print $ getSum $ foldMap (foldMap Sum) v
