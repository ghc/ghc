module T9509a (foo) where

import Data.IORef

foo :: Ord a => a -> IO a
{-# INLINABLE foo #-}
foo x = newIORef x >>= readIORef >>= \y ->
        case compare x y of
           LT ->  return x
           _  ->  foo x   -- Recursive so it won't inline
