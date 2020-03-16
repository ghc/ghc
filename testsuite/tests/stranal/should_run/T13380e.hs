import Control.Exception

-- This is just like T13380d, but doesn't look through the IO abstraction.
-- With Nested CPR, it will result in very similar code, however!

-- | An IO action that throws a precise excpetion that isn't inlined.
throws :: IO ()
throws = throwIO (userError "What")
{-# NOINLINE throws #-}

{-# NOINLINE f #-}
f :: Int -> Int -> IO Int
-- à la #13380
f x y | x>0       = throws >> return 0
      | y>0       = return 1
      | otherwise = return 2

main = f 2 undefined >>= print
