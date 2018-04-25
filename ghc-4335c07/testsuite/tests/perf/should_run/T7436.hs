{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Main where

import Prelude hiding (foldr)
import Data.Foldable

data List a = Nil | Cons a (List a)
    deriving (Functor, Foldable)

mkList :: Int -> List Int
mkList 0 = Nil
mkList n = Cons n (mkList (n-1))

main :: IO ()
main = print $ foldr (\x y -> y) "end" (mkList n)
  where n = 40000
  -- Increase this to increase the difference between good and bad
  -- Eg 6000 takes a lot longer
  -- The biggest difference is not allocation or bytes used,
  -- but execution time!


