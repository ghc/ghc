{-# OPTIONS -fwarn-incomplete-patterns #-}
module ShouldCompile where

-- should *not* produce a warning about non-exhaustive patterns
lazyZip:: [a] -> [b] -> [(a, b)]
lazyZip []     _       = []
lazyZip (x:xs) ~(y:ys) = (x, y):lazyZip xs ys
