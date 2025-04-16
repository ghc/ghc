module B where

foo = 50

class Test a where
  test :: a -> String

instance Test Int where
  test = show
