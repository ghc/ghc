-- {-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
-- {-# OPTIONS_GHC -O2 -fforce-recomp #-}
-- {-# LANGUAGE PatternSynonyms #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
module T21119 ( get, getIO, indexError, throwIndexError ) where

import Control.Exception (Exception(..))
import GHC.IO hiding (throwIO)
import GHC.Exts

throwIO :: Exception e => e -> IO a
throwIO e = IO (raiseIO# (toException e))

myconcat :: [[a]] -> [a]
myconcat = concat
{-# NOINLINE myconcat #-}

class MyShow a where
  myshow :: a -> String

instance MyShow Int where
  myshow !_ = "0"

instance MyShow (a, b) where
  myshow !_ = "()"

indexError :: MyShow a => (a, a) -> a -> String -> b
indexError rng i s = errorWithoutStackTrace (myconcat [myshow rng, myshow i, s])

get :: (Int, Int) -> Int -> [a] -> a
get p@(l,u) i xs
  | l <= i, i < u = xs !! (i-u)
  | otherwise     = indexError p i "get"

-- Now the same with precise exceptions:

throwIndexError :: MyShow a => (a, a) -> a -> String -> IO b
throwIndexError rng i s = throwIO (userError (myconcat [myshow rng, myshow i, s]))

-- It's important that we don't unbox 'u' here.
-- We may or may not unbox 'p' and 'l'.
-- Last time I checked, we didn't unbox 'p' and 'l', because 'throwIndexError'
-- isn't strict in them. That's fine.
getIO :: (Int, Int) -> Int -> [a] -> IO a
getIO p@(l,u) i xs
  | l <= i, i < u = return $! xs !! (i-u)
  | otherwise     = throwIndexError p i "get"
