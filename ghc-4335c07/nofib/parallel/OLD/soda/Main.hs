{-
Return-Path: colin@uk.ac.york.minster
Return-Path: <colin@uk.ac.york.minster>
Received: from minster.york.ac.uk by goggins.dcs.gla.ac.uk with SMTP (PP) 
          id <02303-0@goggins.dcs.gla.ac.uk>; Thu, 24 Jun 1993 15:10:40 +0100
From: colin@minster.york.ac.uk
Date: Thu, 24 Jun 93 14:00:31
To: mattson@dcs.gla.ac.uk
Message-ID: <swordfish.740931041@minster.york.ac.uk>
-}

------------------------------------------------------------------
-- Searching in a grid of words for hidden words oriented in any of
-- the 8 possible directions.
-- Colin Runciman, May 1984 (Haskell version June 1993)
-- Version 5 -- search parallel lines in parallel
------------------------------------------------------------------

import Control.Parallel
import Data.List(transpose)--1.3

main = par (unigrid d) (par (unigrid dr) (par (unigrid ur) (
       putStr (concat (parmap find hidden))
       )))
    where
    find word = seq (unilist2 dirs) (word ++ " " ++ concat dirs ++ "\n")
        where
        dirs = par forw (seq back (map snd (forw ++ back)))
        forw = filter (parany (contains word) . fst)
               [(r,"right "), (d,"down "), (dr,"downright "), (ur,"upright ")]
        back = filter (parany (contains drow) . fst) 
               [(r,"left "), (d,"up "), (dr,"upleft "), (ur,"downleft ")]
        drow = reverse word
    r  = grid
    d  = transpose grid
    dr = diagonals grid
    ur = diagonals (reverse grid)

parmap :: (String -> String) -> [String] -> [String]
parmap f [] = []
parmap f (x:xs) = par fx (seq fxs (fx : fxs))
    where
    fx  = f x
    fxs = parmap f xs

parmap2 :: (String -> Bool) -> [String] -> [Bool]
parmap2 f [] = []
parmap2 f (x:xs) = par fx (seq fxs (fx : fxs))
    where
    fx  = f x
    fxs = parmap2 f xs


unimap :: ([Char] -> ()) -> [[Char]] -> ()
unimap f [] = ()
unimap f (x:xs) = seq (f x) (unimap f xs)  

unimap2 :: (Char -> Char) -> [Char] -> ()
unimap2 f [] = ()
unimap2 f (x:xs) = seq (f x) (unimap2 f xs)  

unigrid :: [[Char]] -> ()
unigrid = unimap unilist

unilist :: [Char] -> ()
unilist = unimap2 id

unilist2 :: [String] -> ()
unilist2 = unimap3 id

unimap3 :: (String -> String) -> [String] -> ()
unimap3 f [] = ()
unimap3 f (x:xs) = seq (f x) (unimap3 f xs)  

parany = (\x -> or . x) . parmap2

diagonals [r] = map (:[]) (reverse r)
diagonals (r:rs) = zipinit (reverse r) ([]:diagonals rs)

zipinit [] ys = ys
zipinit (x:xs) (y:ys) = (x : y) : zipinit xs ys

contains xs ys = any (prefix xs) (suffixes ys)

suffixes [] = []
suffixes xs = xs : suffixes (tail xs) 

prefix [] ys = True
prefix xs [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

grid =
   [['Y', 'I', 'O', 'M', 'R', 'E', 'S', 'K', 'S', 'T'],
    ['A', 'E', 'H', 'Y', 'G', 'E', 'H', 'E', 'D', 'W'],
    ['Z', 'F', 'I', 'A', 'C', 'N', 'I', 'T', 'I', 'A'],
    ['N', 'T', 'O', 'C', 'O', 'M', 'V', 'O', 'O', 'R'],
    ['E', 'R', 'D', 'L', 'O', 'C', 'E', 'N', 'S', 'M'],
    ['Z', 'O', 'U', 'R', 'P', 'S', 'R', 'N', 'D', 'A'],
    ['O', 'Y', 'A', 'S', 'M', 'O', 'Y', 'E', 'D', 'L'],
    ['R', 'N', 'D', 'E', 'N', 'L', 'O', 'A', 'I', 'T'],
    ['F', 'I', 'W', 'I', 'N', 'T', 'E', 'R', 'R', 'C'],
    ['F', 'E', 'Z', 'E', 'E', 'R', 'F', 'T', 'F', 'I'],
    ['I', 'I', 'D', 'T', 'P', 'H', 'U', 'B', 'R', 'L'],
    ['C', 'N', 'O', 'H', 'S', 'G', 'E', 'I', 'O', 'N'],
    ['E', 'G', 'M', 'O', 'P', 'S', 'T', 'A', 'S', 'O'],
    ['T', 'G', 'F', 'F', 'C', 'I', 'S', 'H', 'T', 'H'],
    ['O', 'T', 'B', 'C', 'S', 'S', 'N', 'O', 'W', 'I']]

hidden =
  ["COSY", "SOFT", "WINTER", "SHIVER", "FROZEN", "SNOW",
   "WARM", "HEAT", "COLD",   "FREEZE", "FROST",  "ICE" ]

