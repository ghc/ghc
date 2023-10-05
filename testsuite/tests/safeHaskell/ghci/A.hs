{-# LANGUAGE Trustworthy #-}
module A (a) where

import System.IO.Unsafe

a :: Int
a = 1

unsafe = unsafePerformIO

