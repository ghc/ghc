
{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances #-}

module T2902_A_PairingSum (Sum(..), PSum) where

import T2902_Sum

data PSum a b = Empty | Tree a b [(PSum a b)]

instance (Ord a, Eq b, Num b) ⇒ Sum PSum a b where
  insert     = insertX
  union      = unionX
  unions     = unionsX
  extractMin = extractMinX
  fromList   = fromListX
  toList     = toListX

insertX ∷ (Ord a, Eq b, Num b) ⇒ a → b → PSum a b → PSum a b
insertX v r = unionX $ Tree v r []

unionX ∷ (Ord a, Eq b, Num b) ⇒ PSum a b → PSum a b → PSum a b
unionX x Empty = x
unionX Empty x = x
unionX x@(Tree v r xs) y@(Tree w s ys) =
  case compare v w of
    LT → Tree v r (y:xs)
    GT → Tree w s (x:ys)
    EQ → case r + s of
      0 → z
      t → insertX v t z
  where z = unionX (unionsX xs) (unionsX ys)

unionsX ∷ (Ord a, Eq b, Num b) ⇒ [PSum a b] → PSum a b
unionsX [] = Empty
unionsX [x] = x
unionsX (x : y : zs) = unionX (unionX x y) (unionsX zs)

extractMinX ∷ (Ord a, Eq b, Num b) ⇒ PSum a b → ((a,b), PSum a b)
extractMinX Empty = undefined
extractMinX (Tree v r xs) = ((v,r), unionsX xs)

fromListX ∷ (Ord a, Eq b, Num b) ⇒ [(a,b)] → PSum a b
fromListX [] = Empty
fromListX ((v,r):xs) = insertX v r $ fromListX xs

toListX ∷ (Ord a, Eq b, Num b) ⇒ PSum a b → [(a,b)]
toListX Empty = []
toListX x = let (y, z) = extractMinX x in y : toListX z

