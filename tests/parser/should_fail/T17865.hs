module T17865 where

data T = 'MkT

data T' = ' MkT'

data I a b = a ':> b

data I' a b = a ' :>$ b
