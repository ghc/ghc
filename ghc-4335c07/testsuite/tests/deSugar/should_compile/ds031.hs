module ShouldCompile where

foldPair :: (a->a->a,b->b->b) -> (a,b) -> [(a,b)] -> (a,b)
foldPair fg       ab [] = ab
foldPair fg@(f,g) ab ((a,b):abs) = (f a u,g b v)
                       where (u,v) = foldPair fg ab abs

