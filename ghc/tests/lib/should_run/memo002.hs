module Main
where

import Memo
import System(getArgs)

main = do (arg:_) <- getArgs
	  mapM_ printTriple [ (i,fib i,mfib i) | i <- [10..read arg] ]
  where printTriple (i,fi,mfi) = do print i
				    print fi
				    print mfi
				    putStrLn ""

mfib :: Integer -> Integer
mfib = memo ufib

ufib :: Integer -> Integer
ufib 0 = 1
ufib 1 = 1
ufib n = mfib (n-1) + mfib (n-2)

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
