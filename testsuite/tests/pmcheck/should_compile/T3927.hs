{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}

module T3927 where

data T a where
  T1 :: T Int
  T2 :: T Bool

-- f1 is exhaustive
f1 :: T a -> T a -> Bool
f1 T1 T1 = True
f1 T2 T2 = False
