{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
import GHC.Exts
import System.IO.Unsafe ( unsafePerformIO )

z# :: Int -> Int#
z# _ = unsafePerformIO (putStrLn "Can you read this?")  `seq` 0#
{-# NOINLINE z# #-}

f :: Int# -> Int
f _ = unsafePerformIO (putStrLn "Rewrite rule did not match? Bad!")  `seq` 0
{-# NOINLINE f #-}

g :: Int -> Int
g _ = 1
{-# NOINLINE g #-}

h :: Int# -> Int
h _ = 2
{-# NOINLINE h #-}

main = print (f (z# 3))

{-# RULES "f to h.g" forall x. f x = g (h x) #-}
