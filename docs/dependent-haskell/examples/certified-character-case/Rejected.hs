module Rejected (recursive, partial, wrongDefault) where

recursive, partial, wrongDefault :: Char -> Char
recursive 'x' = recursive 'x'
recursive c = c
partial 'x' = 'X'
wrongDefault 'x' = 'X'
wrongDefault _ = '?'
