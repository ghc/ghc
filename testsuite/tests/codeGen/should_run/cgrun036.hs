-- !! Won't compile unless the compile succeeds on
-- !! the "single occurrence of big thing in a duplicated small thing"
-- !! inlining old-chestnut.  WDP 95/03
--
module Main ( main, g ) where

main = putStr (shows (g 42 45 45) "\n")

g :: Int -> Int -> Int -> [Int]

g x y z
  = let
	f a b = a + b * a * b - a + a + b + b * a * b - a + a + b + b * a * b - a + a + b + b * a * b - a + a + b + b * a * b - a + a + b + b * a * b - a + a + b + b * a * b - a + a + b + b * a * b - a + a + b + b * a * b - a + a + b + b * a * b - a + a + b + b * a * b - a + a + b + b * a * b - a + a + b + b * a * b - a + a + b + b * a * b - a + a + b
	g c = f c c
    in
    [g x, g y, g z, g x, g y, g z, g x, g y, g z, g x, g y, g z, g x, g y, g z, g x, g y, g z, g x, g y, g z, g x, g y, g z, g x, g y, g z, g x, g y, g z, g x, g y]
