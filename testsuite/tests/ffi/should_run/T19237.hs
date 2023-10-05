{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -dno-typeable-binds -O #-}

module Main where

import Control.Monad
import GHC.Exts
import Control.Concurrent

foreign import ccall expect_999 :: Int# -> IO ()

main :: IO ()
main = do
  _ <- forkIO $ forever $ putStr ""
  replicateM_ 100000 (baz (# #))

{-# NOINLINE baz #-}
baz :: (# #) -> IO ()
baz c = expect_999 (bar c)

{-# NOINLINE bar #-}
bar :: (# #) -> Int#
bar (# #) = 999#
