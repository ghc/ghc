-- At some point when developing the new capability for
-- #24359 (SEPECIALISE pragmas on values), this program
-- went into a in infinite loop.  So just keeping it here
-- as a rgression test

module Main where

data UA i = UA !i

class IArray a where
  bounds :: a i -> i

showsIArray :: (IArray a, Show i) => a i -> String
showsIArray a = show (bounds a)

{-# SPECIALISE
    showsIArray :: (Show i) => UA i -> String
  #-}

instance IArray UA where
    bounds (UA u) = u

main :: IO ()
main = putStrLn $ showsIArray (UA 1 :: UA Int)
