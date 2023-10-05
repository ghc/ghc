module T20185a where

data X = X { foo :: Y   }
data Y = Y { bar :: Int }

y :: Y
y = Y 1

x :: X
x = X y
