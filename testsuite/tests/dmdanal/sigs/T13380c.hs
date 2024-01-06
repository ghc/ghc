module Lib (f) where

import Control.Exception

-- | Just an arbitrary IO action without throwIO that isn't inlined.
doesn'tThrow :: IO ()
doesn'tThrow = return ()
{-# NOINLINE doesn'tThrow #-}

{-# NOINLINE f #-}
f :: Int -> Int -> IO Int
f x y | x>0       = doesn'tThrow >> (y `seq` return 0)
      | y>0       = return 1
      | otherwise = return 2
