{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}

-- The idea:
--
--  1. Register a CAFfy foreign export
--  2. Force a CAF reachable from the export
--  3. Do a GC
--  4. Then call the foreign export.

import System.Mem
import Foreign.C.Types

x :: Integer
x = fib 80
{-# NOINLINE x #-}

test_export :: IO CInt
test_export = return $ fromIntegral x

fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = go 1 1 n
  where
    go :: Integer -> Integer -> Int -> Integer
    go !n0 !n1 0 = n1
    go  n0  n1 i = let n0' = n1
                       n1' = n0 + n1
                   in go n0' n1' (i-1)
{-# NOINLINE fib #-}

foreign export ccall test_export :: IO CInt
foreign import ccall test :: IO CInt

main :: IO ()
main = do
  print (fromIntegral x :: CInt)
  _ <- return $! fib 100000
  performMajorGC
  test >>= print
