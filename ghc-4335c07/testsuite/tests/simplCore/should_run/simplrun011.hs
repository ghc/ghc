module Main where

import GHC.Exts

-- This checks that rules look through unfoldings when matching
-- lambdas, but only in the right phase

foo :: (Int -> IO ()) -> IO ()
foo f = putStr "not fired: " >> f 0
{-# NOINLINE foo #-}

f1 :: Int -> IO ()
f1 _ = putStrLn "f1"
{-# NOINLINE[0] f1 #-}

f2 :: Int -> IO ()
f2 _ = putStrLn "f2"
{-# NOINLINE f2 #-}

newtype Age = MkAge Int

-- It also checks that this can look through casted lambdas

f3 :: Age -> IO ()
f3 _ = putStrLn "f3"
{-# NOINLINE[0] f3 #-}


{-# RULES "foo" [0] forall g . foo (\x -> g) = putStr "fired: " >> g #-}

main = do
    foo f1
    foo f1
    foo f2
    foo f2
    foo (coerce f3)
    foo (coerce f3)
