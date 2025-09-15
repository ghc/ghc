module HolesInfix where

f, g, q :: Int -> Int -> Int
f x y = _ x y
g x y = x `_` y
q x y = x `_a` y

h x y = x ` _  `   y
r x y = x  `  _a ` y
