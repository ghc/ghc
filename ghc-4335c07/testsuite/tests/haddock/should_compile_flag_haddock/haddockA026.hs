{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module ShouldCompile where

test :: (Eq a) => [a] -- ^ doc1
               -> forall b . [b] {-^ doc2 -}
               -> [a] -- ^ doc3
test xs ys = xs
