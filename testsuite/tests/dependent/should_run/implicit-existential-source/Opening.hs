{-# LANGUAGE DataKinds, GADTs #-}
module Opening (audit) where

import Filter
import Vector

anyCells :: Vec n a -> Vec m a -> [a]
anyCells Nil Nil = []
anyCells Nil (Cons a _) = [a]
anyCells (Cons a _) _ = [a]

audit :: Hidden Int -> Hidden Int -> [Int]
audit p q = anyCells p q
