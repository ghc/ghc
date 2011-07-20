module F10 where

f10f = \h -> (h 1 2, h 3)
f10g = \x -> \y -> x+y
f10h = f10f f10g
f10x1 = fst f10h
f10x2 = snd f10h
f10 = f10x2 f10x1
