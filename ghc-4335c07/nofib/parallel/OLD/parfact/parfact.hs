-- -*- haskell -*-
-- Time-stamp: <2005-11-09 15:46:13 simonmar>
--
-- Doing a factorial-like computation, actually just sum, using a std
-- parallel divide-and-conquer stucture.
-- Haskell98 version.
-----------------------------------------------------------------------------

module Main(main) where

import System.Environment (getArgs)
import Control.Parallel

main = do args <- getArgs
          let 
            n = read (args!!0)  -- size of the interval
            t = read (args!!1)  -- threshold
            res = pfact n t
          putStrLn ("pfact " ++ (show n) ++ " " ++ (show t) ++ " = " ++ (show res)) {- ++ 
                    "\nResult is " ++ 
                    (if (sum [1..n] == res) then "ok"  else "NOT OK"))-}

-- ASSERT: pfact n _ == sum [1..n]
pfact :: Integer -> Integer -> Integer
pfact n t = pfact' 1 n t

-- thresholding version
pfact' :: Integer -> Integer -> Integer -> Integer
pfact' m n t | (n-m) <= t = sum [m..n]             -- seq version below t
             | otherwise  = left `par` right `seq` -- par d&c version
                            (left + right)
                            where mid = (m + n) `div` 2
                                  left = pfact' m mid t 
                                  right = pfact' (mid+1) n t
