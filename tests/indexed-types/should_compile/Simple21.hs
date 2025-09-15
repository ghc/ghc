{-# LANGUAGE TypeFamilies #-}

module ShouldCompile where

import Prelude hiding (foldr, foldr1)

import Data.Maybe

type family Elem x

class Foldable a where
  foldr :: (Elem a -> b -> b) -> b -> a -> b

  foldr1 :: (Elem a -> Elem a -> Elem a) -> a -> Elem a
  foldr1 f xs = fromMaybe (error "foldr1: empty structure")
                  (foldr mf Nothing xs)
     where mf x Nothing  = Just x
           mf x (Just y) = Just (f x y)
