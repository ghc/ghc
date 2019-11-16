module T16615
where

f :: Int -> Bool
f i = if i == 0 then True else g (pred i)

g :: Int -> Bool
g i = if i == 0 then False else f (pred i)
