module ShouldCompile where

test2 :: a -- ^ doc1 
        -> b {-^ doc2 -} -> a -- ^ doc 3 
test2 x y = x
