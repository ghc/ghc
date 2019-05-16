module Lib where

data D = A | B

f :: D -> D -> D
f A A = A
f B B = B
