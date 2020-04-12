{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Lib where

data S a where
  S1 :: S Int
  S2 :: S Bool

data T where
  K :: S a -> a -> T

f1 :: S Int -> ()
f1 s = case K @Int s 3 of K S1 _ -> ()

f2 :: S Int -> ()
f2 s = case K @Int s 3 of K s' _ -> case s' of S1 -> ()

data T2 where
  K2 :: (a -> S a) -> a -> T2

g1 :: (Int -> S Int) -> ()
g1 h = case K2 @Int h 3 of K2 h' (h' -> S1) -> ()

g2 :: (Int -> S Int) -> ()
g2 h = case K2 @Int h 3 of K2 h' i' -> case h' i' of S1 -> ()
