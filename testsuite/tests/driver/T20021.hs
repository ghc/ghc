{-# OPTIONS_GHC -ddump-rule-firings -fno-enable-rewrite-rules #-}
module T20021 where

foo :: Int -> Int
foo n = length ([0..(10^n)] :: [Int])
