module Main where

--!!! space leak from overloading !!!

-- This program develops a space leak if sfoldl isn't compiled with some
-- care.  See comment about polymorphic recursion in TcMonoBinds.lhs

import System(getArgs)
import IOBase
import STBase

sfoldl :: Eval a => (a -> Int -> a) -> a -> [Int] -> a
sfoldl f z [] = z
sfoldl f z (x:xs) = _scc_ "sfoldl1" (sfoldl f fzx (fzx `seq` xs))
                  where fzx = _scc_ "fzx" (f z x)


main = IO (\s -> case print (sfoldl (+) (0::Int) [1..200000]) of { IO a -> a s })
