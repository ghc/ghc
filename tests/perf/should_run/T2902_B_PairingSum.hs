
{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances #-}

module T2902_B_PairingSum (Sum(..), PSum) where

import T2902_Sum

data PSum a b = Empty | Tree a b [PSum a b]

instance (Ord a, Eq b, Num b) ⇒ Sum PSum a b where

  insert v r = union $ Tree v r []

  union x Empty = x
  union Empty x = x
  union x@(Tree v r xs) y@(Tree w s ys) =
    case compare v w of
      LT → Tree v r (y:xs)
      GT → Tree w s (x:ys)
      EQ → case r + s of
        0 → z
        t → insert v t z
    where z = union (unions xs) (unions ys)

  unions [] = Empty
  unions [x] = x
  unions (x : y : zs) = union (union x y) (unions zs)

  extractMin Empty = undefined
  extractMin (Tree v r xs) = ((v,r), unions xs)

  fromList [] = Empty
  fromList ((v,r):xs) = insert v r $ fromList xs

  toList Empty = []
  toList x = let (y, z) = extractMin x in y : toList z

