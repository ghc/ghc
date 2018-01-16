{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- Test allocation of statically sized byte arrays. There's an
-- optimization that targets these and we want to make sure that the
-- code generated in the optimized case is correct.
--
-- The tests proceeds by allocating a bunch of byte arrays of
-- different sizes, to try to provoke GC crashes, which would be a
-- symptom of the optimization not generating correct code.
module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
    loop 1000
    putStrLn "success"
  where
    loop :: Int -> IO ()
    loop 0 = return ()
    loop i = do
        -- Sizes have been picked to match the triggering of the
        -- optimization and to match boundary conditions. Sizes are
        -- given explicitly as to not rely on other optimizations to
        -- make the static size known to the compiler.
        newByteArray 0
        newByteArray 1
        newByteArray 2
        newByteArray 3
        newByteArray 4
        newByteArray 5
        newByteArray 6
        newByteArray 7
        newByteArray 8
        newByteArray 9
        newByteArray 10
        newByteArray 11
        newByteArray 12
        newByteArray 13
        newByteArray 14
        newByteArray 15
        newByteArray 16
        newByteArray 64
        newByteArray 128
        newByteArray 129
        loop (i-1)

newByteArray :: Int -> IO ()
newByteArray (I# sz#) = IO $ \s -> case newByteArray# sz# s of
    (# s', _ #) -> (# s', () #)
{-# INLINE newByteArray #-}  -- to make sure optimization triggers
