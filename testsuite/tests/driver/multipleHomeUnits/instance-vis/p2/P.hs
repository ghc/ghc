-- The same as the module in p1, but doesn't contain an instance
module P where

class Test x where
  test :: x -> x

data P = P

instance Test P where
  test = id

