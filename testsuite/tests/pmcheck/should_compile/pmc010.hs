module Lib where

data D = A | B | C | D

f :: D -> D -> D
f A A = A
f B B = B
f C C = C
f D D = D
