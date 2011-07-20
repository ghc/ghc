module ShouldCompile where

test :: (Eq a) => [a] -- ^ doc1 
               -> [a] {-^ doc2 -} 
               -> [a] -- ^ doc3
test xs ys = xs
