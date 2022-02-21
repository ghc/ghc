-- {-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
-- {-# OPTIONS_GHC -O2 -fforce-recomp #-}
-- {-# LANGUAGE PatternSynonyms #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE MagicHash, UnboxedTuples #-}
module T21119 where

import Control.Exception

indexError :: Show a => (a, a) -> a -> String -> b
indexError rng i s = error (show rng ++ show i ++ show s)

get :: (Int, Int) -> Int -> [a] -> a
get p@(l,u) i xs
  | l <= i, i < u = xs !! (i-u)
  | otherwise     = indexError p i "get"

-- Now the same with precise exceptions:

throwIndexError :: Show a => (a, a) -> a -> String -> IO b
throwIndexError rng i s = throwIO (userError (show rng ++ show i ++ show s))

-- It's important that we don't unbox 'u' here.
-- We may or may not unbox 'p' and 'l'.
-- Last time I checked, we didn't unbox 'p' and 'l', because 'throwIndexError'
-- isn't strict in them. That's fine.
getIO :: (Int, Int) -> Int -> [a] -> IO a
getIO p@(l,u) i xs
  | l <= i, i < u = return $! xs !! (i-u)
  | otherwise     = throwIndexError p i "get"
