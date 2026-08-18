module Caller where

import Callee

{-# NOINLINE a #-}
a :: UC (UC (TC t)) => t -> Int
a x = b x + 1
