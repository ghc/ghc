module A where

import B

baz = foo + foobar

foobar = 18

data A = A deriving (Show)

instance Test A where
  test = show
