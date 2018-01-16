{-# LANGUAGE QuasiQuotes #-}
module Test where

import QQ

f' = f . (+ 1)

[pq| foo |]         -- Expands to f :: Int -> Int
f x = x + 1
