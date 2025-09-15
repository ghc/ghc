{-# LANGUAGE DeriveFunctor #-}
-- A variation on T7436 that tests a derived Functor instance.
module Main where

data List a = Nil | Cons a (List a)
    deriving Functor

mkList :: Int -> List Int
mkList 0 = Nil
mkList n = Cons n (mkList (n-1))

sumList :: List Int -> Int
sumList = go 0
  where
    go a Nil = a
    go a (Cons n ns) = a `seq` go (a+n) ns

main :: IO ()
main = print $ sumList . fmap id $ mkList n
  where n = 40000
