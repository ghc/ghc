{-# LANGUAGE DeriveFoldable #-}

module Main where
import Data.Semigroup

-- Just a list without any special fusion rules.
data List a = Nil | Cons a (List a) deriving Foldable

instance Semigroup (List a) where
  Nil <> ys = ys
  Cons x xs <> ys = Cons x (xs <> ys)

replicateList :: Int -> a -> List a
replicateList 0 x = Nil
replicateList n x = Cons x (replicateList (n - 1) x)

newtype ListList a = ListList (List (List a)) deriving Foldable

long :: Int -> Bool
long n = null $ ListList $ replicateList n Nil <> Cons (Cons () Nil) Nil

main :: IO ()
main = print $ long (10^(6 :: Int))
