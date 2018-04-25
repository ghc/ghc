-- Glasgow Haskell 0.403 : FINITE ELEMENT PROGRAM
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : basics.hs 		DATE : 4-3-1991                 *
-- *                                                                    *
-- * CONTENTS : Basics functions for output.                            *
-- *                                                                    *
-- **********************************************************************


module Basics(showrj,showlj, azip, module Data.Array) where

import Data.Array


showlj, showrj :: (Show a) => Int -> a -> [Char]

rep :: Int -> a -> [a]

rep 0 x = []
rep n x = x : (rep (n-1) x)

showrj l x
      = (rep bs ' ') ++ ns
        where
        ns = dropWhile ((==) ' ') (show x)
        bs | l <= length ns   = 1
           | otherwise        = l - length ns

showlj l x
      = ns ++ (rep bs ' ')
        where
        ns = dropWhile ((==) ' ') (show x)
        bs | l <= length ns   = 1
           | otherwise        = l - length ns

azip :: [a] -> [b] -> [(a,b)]

azip [] [] = []
azip ( x : ls ) ( x' : ls' ) = (x,x') : (azip ls ls')


