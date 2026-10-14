{-# LANGUAGE GADTs, ViewPatterns, RankNTypes #-}
{-# OPTIONS_GHC -fno-opt-coercion #-}
module T27936 where

data G a where
  MkG :: G Int

f :: G a -> a -> Int -> Int
f MkG v (((\x -> (v + 1, x)) :: forall b. b -> (Int, b)) -> (y, _)) = y
