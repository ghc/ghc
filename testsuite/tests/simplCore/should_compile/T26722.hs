module T26722 where

data T = MkT ![Int]

g s True  t = f s t t
g s False t = g s True t

f True  (MkT xs) t = f False (MkT xs) t
f False (MkT xs) _ = xs
