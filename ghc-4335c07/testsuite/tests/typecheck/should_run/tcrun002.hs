-- !!! space leak from overloading !!!
module Main where

-- This program develops a space leak if sfoldl isn't compiled with some
-- care.  See comment about polymorphic recursion in TcMonoBinds.lhs

import System.Environment (getArgs)
import GHC.IO

sfoldl :: (a -> Int -> a) -> a -> [Int] -> a
sfoldl f z [] = z
sfoldl f z (x:xs) = {-# SCC "sfoldl1" #-} (sfoldl f fzx (fzx `seq` xs))
                  where fzx = {-# SCC "fzx" #-} (f z x)


main = IO (\s -> case print (sfoldl (+) (0::Int) [1..200000]) of { IO a -> a s })
