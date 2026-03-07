{-# LANGUAGE TypeApplications #-}

module T16122 (f, g) where

import Data.Int (Int64)

f :: Double -> Int64
f = round

g :: Double -> Int64
g = fromIntegral @Int @Int64 . round
