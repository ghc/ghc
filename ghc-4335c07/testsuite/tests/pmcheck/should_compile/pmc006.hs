{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}

module PMC006 where

len :: [a] -> Int
len xs = case xs of
           []     -> 0
           (_:ys) -> case () of
                       () | (_:_) <- xs -> 1 + len ys

-- -- we would like these to work too but they don't yet
--
-- len :: [a] -> Int
-- len [] = 0
-- len xs = case xs of
--            (_:ys) -> 1 + len ys
--
-- len :: [a] -> Int
-- len xs = case xs of
--            [] -> 0
--            ys -> case ys of
--                    (_:zs) -> 1 + len zs
