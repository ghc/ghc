module Main where

import Memo2              ( memo )
import Data.List          ( genericLength, genericReplicate )
import System.Environment ( getArgs )

main :: IO ()
main = do (arg:_) <- getArgs
	  mapM_ printTriple [ (i,fib i,mfib i) | i <- [10..read arg] ]
  where printTriple (i,fi,mfi) = do print i
				    print fi
				    print mfi
				    putStrLn ""

-- There is not much point in memoising Integers, so we use unary "numbers" instead
mfib :: Integer -> Integer
mfib = genericLength . mfib' . flip genericReplicate ()

mfib' :: [()] -> [()]
mfib' = memo ufib

ufib :: [()] -> [()]
ufib []              = [()]
ufib [()]            = [()]
ufib (():n1@(():n2)) = mfib' n1 ++ mfib' n2

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
