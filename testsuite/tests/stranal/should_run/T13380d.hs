{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

import Control.Exception
import GHC.Exts
import GHC.IO

-- | Just an arbitrary IO action without throwIO that isn't inlined.
throws :: State# RealWorld -> State# RealWorld
throws s = case raiseIO# (toException (userError "What")) s of (# s', _ #) -> s'
{-# NOINLINE throws #-}

{-# NOINLINE f #-}
f :: Int -> Int -> IO Int
-- à la #13380
f x y | x>0       = IO $ \s -> case throws s of s' -> unIO (return 0) s'
      | y>0       = return 1
      | otherwise = return 2

main = f 2 undefined >>= print
