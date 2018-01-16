module ParserNoMultiWayIf where

x = 123
y = if | x < 0     -> -1
       | x == 0    -> 0
       | otherwise -> 1

