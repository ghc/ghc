module ShouldCompile where

data a <--> b = Mk a b

test :: [a] -- ^ doc1 
        -> a <--> b   
        -> [a] -- ^ blabla
test xs ys = xs
