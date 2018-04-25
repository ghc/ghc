-- Tests pattern bindings that are generalised

module T5032 where

id1 :: a -> a
(id1) = id

foo = (id1 True, id1 'x')

g :: a -> a
h :: a -> a
(g, h) = (\x -> x, \y -> y)

too = (g (h True), g (h 'x'))

-- No type signature necessary; 
-- the MR only strikes when overloading is involved
[id3] = [id]

noo = (id3 True, id3 'x')
