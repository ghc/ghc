module X (x, D1(..), D2(..))
where

data D1 = D { f :: D2 } -- deriving Show
data D2 = A | B -- deriving Show

x :: D1
x = D { f = A }


