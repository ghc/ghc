module Rejected (recursive, partial, wrongDefault, predicate) where

recursive, partial, wrongDefault :: Char -> Char
recursive 'x' = recursive 'x'
recursive c = c
partial 'x' = 'X'
wrongDefault 'x' = 'X'
wrongDefault _ = '?'

predicate :: Char -> Bool
predicate 'x' = True
predicate _ = False
