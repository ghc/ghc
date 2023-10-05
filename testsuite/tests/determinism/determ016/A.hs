
{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -w #-}

module A where

data PSum a b = Empty | Tree a b [(PSum a b)]

extractMinX ∷ (Ord a, Eq b, Num b) ⇒ PSum a b → ((a,b), PSum a b)
extractMinX Empty = undefined
extractMinX (Tree v r xs) = ((v,r), Empty)

toListX ∷ (Ord a, Eq b, Num b) ⇒ PSum a b → [(a,b)]
toListX Empty = []
toListX x = let (y, z) = extractMinX x in y : toListX z

main ∷ IO ()
main = print $ take 20 $ toListX $ (Empty :: PSum Int Int)
