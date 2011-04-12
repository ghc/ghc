module Supercompile.StaticFlags where

import System.Environment
import System.IO.Unsafe


{-# NOINLINE aRGS #-}
aRGS :: [String]
aRGS = unsafePerformIO getArgs

cALL_BY_NAME :: Bool
cALL_BY_NAME = "--call-by-name" `elem` aRGS

dEEDS :: Bool
dEEDS = "--deeds" `elem` aRGS
