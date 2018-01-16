{-
From: Andrew J Bromage <ajb@spamcop.net>
Date: Fri, 22 Nov 2002 13:49:13 +1100
To: haskell@haskell.org
Subject: Re: diff in Haskell: clarification

Just for jollies, here's a Haskell version of Hirschberg's LCSS
algorithm.  It's O(N^2) time but O(N) space at any given point in
time, assuming eager evaluation.  You should be able to make diff out
of this.  You should also be able to find many opportunities for
optimisation here.
-}

module Main (main) where

import System.Environment

algb :: (Eq a) => [a] -> [a] -> [Int]
algb xs ys
  = 0 : algb1 xs [ (y,0) | y <- ys ]
  where
    algb1 [] ys' = map snd ys'
    algb1 (x:xs) ys'
      = algb1 xs (algb2 0 0 ys')
      where
	algb2 _ _ [] = []
	algb2 k0j1 k1j1 ((y,k0j):ys)
	  = let kjcurr = if x == y then k0j1+1 else max k1j1 k0j
	    in (y,kjcurr) : algb2 k0j kjcurr ys

algc :: (Eq a) => Int -> Int -> [a] -> [a] -> [a] -> [a]
algc m n xs []  = id
algc m n [x] ys = if x `elem` ys then (x:) else id
algc m n xs ys
  = algc m2 k xs1 (take k ys) . algc (m-m2) (n-k) xs2 (drop k ys)
  where
    m2 = m `div` 2

    xs1 = take m2 xs
    xs2 = drop m2 xs

    l1 = algb xs1 ys
    l2 = reverse (algb (reverse xs2) (reverse ys))

    k = findk 0 0 (-1) (zip l1 l2)

    findk k km m [] = km
    findk k km m ((x,y):xys)
      | x+y >= m  = findk (k+1) k  (x+y) xys
      | otherwise = findk (k+1) km m     xys

lcss :: (Eq a) => [a] -> [a] -> [a]
lcss xs ys = algc (length xs) (length ys) xs ys []

main = do 
 [a,b,c,d,e,f] <- getArgs
 let a', b', c', d', e', f' :: Int
     a' = read a; b' = read b; c' = read c; 
     d' = read d; e' = read e; f' = read f
 print (lcss [a',b'..c'] [d',e'..f'])
