import OneShot1
import System.Environment
import Debug.Trace

p n = trace "p evaluated" (n > 0)
{-# NOINLINE p #-}

summap :: (Int -> Int) -> (Int -> Int)
summap f n = sum $ map f [1..10]
{-# NOINLINE summap #-}

foo' n = if p n then foo n else foo (n+1)
{-# NOINLINE foo' #-}

bar' n = if p n then bar n else bar (n+1)
{-# NOINLINE bar' #-}

baz' n = if p n then baz n else baz (n+1)
{-# NOINLINE baz' #-}

main = do
  n <- length `fmap` getArgs
  print $ summap (foo' n) n + summap (bar' n) n + summap (baz' n) n

