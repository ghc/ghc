-----------------------------------------------------------------------------
-- -*- haskell -*-
-- Ever popular nfib, now in parallel.
-----------------------------------------------------------------------------

module Main(main) where

import System.Environment (getArgs)
import Control.Parallel

main = do [arg1,arg2] <- getArgs
          let
            n = read arg1 :: Int  -- input for nfib
            t = read arg2 :: Int  -- threshold
            res = parfib n t
          putStrLn ("parfib " ++ show n ++ " = " ++ show res)

-- parallel version of the code with thresholding
parfib :: Int -> Int -> Int
parfib n t | n <= t = nfib n
           | otherwise = n1 `par` (n2 `pseq` n1 + n2 + 1)
	                 where n1 = parfib (n-1) t
			       n2 = parfib (n-2) t

-- sequential version of the code
nfib :: Int -> Int
nfib 0 = 1
nfib 1 = 1
nfib x = nfib (x-2) + nfib (x-1) + 1
