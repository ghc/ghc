{-# LANGUAGE InterruptibleFFI #-}

module T13380f where

-- This one concerns FFI calls, and tests precise exception detection
-- like T13380{d,e}. Unsafe FFI calls should be considered not to throw a
-- precise exception, all other FFI calls may well throw precise exceptions.
-- We test that by looking at the strictness signatures of f, g and h, so that
-- we don't actually have to provide C bindings.

foreign import ccall unsafe "__unsafe"
   unsafeCall :: IO ()

foreign import ccall safe "__safe"
   safeCall :: IO ()

foreign import ccall interruptible "__interruptible"
   interruptibleCall :: IO ()

{-# NOINLINE f #-}
f :: Int -> Int -> IO Int
-- Strict in y!
f x y | x>0       = unsafeCall >> (y `seq` return 0)
      | y>0       = return 1
      | otherwise = return 2

{-# NOINLINE g #-}
g :: Int -> Int -> IO Int
-- à la #13380. Lazy in y!
g x y | x>0       = safeCall >> (y `seq` return 0)
      | y>0       = return 1
      | otherwise = return 2

{-# NOINLINE h #-}
h :: Int -> Int -> IO Int
-- à la #13380. Lazy in y!
h x y | x>0       = interruptibleCall >> (y `seq` return 0)
      | y>0       = return 1
      | otherwise = return 2
