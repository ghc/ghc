-- !!! ds005 -- mappairs from SLPJ Ch 5'
--
-- this simply tests a "typical" example

module ShouldCompile where

-- from SLPJ, p 78
mappairs f []     ys     = []
mappairs f (x:xs) []     = []
mappairs f (x:xs) (y:ys) = f x y : mappairs f xs ys

-- from p 80
mappairs' f []     ys     = []
mappairs' f x      []     = []
mappairs' f (x:xs) (y:ys) = f x y : mappairs' f xs ys
