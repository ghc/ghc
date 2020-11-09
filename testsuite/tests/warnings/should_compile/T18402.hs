module T18402 where

a = ['b'      .. 'a'] -- empty
b = ['b', 'a' .. 'c'] -- empty
c = ['b', 'c' .. 'a'] -- empty
d = ['a'      .. 'c'] -- not empty
e = ['a', 'c' .. 'b'] -- not empty

