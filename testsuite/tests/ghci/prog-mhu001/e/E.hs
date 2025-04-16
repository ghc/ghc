module E where

foo = 4

main = putStrLn "Hello, World"

class Test a where
  test :: a -> String

instance Test Int where
  test = show
