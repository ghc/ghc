module Test where

-- two sets of mutually-recursive blobs:
--  f, g, h are mut rec
--  i, j, k are mut rec

(f1@(f2@(f3@f)), 1) = g 1 1
(i1@(i2@(i3@i)), 1) = j 1 1

(Foo g 1 2) = (h, 1, 1, 2)
(Foo j 1 2) = (k, 1, 1, 2)

(~ ~ ~ ~h, 1, 2, 3) = f 3
(~ ~ ~ ~k, 1, 2, 3) = i 3
