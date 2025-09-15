module E (main, foo, Test(..)) where

foo = 5

bar = 10

main = do
  print bar
  print foo

class Test a where
  test :: a -> String

instance Test Int where
  test = show

data E = E

eId E = E

instance Test E where
  test E = "E"

myCustomMain = do
  putStrLn "Entry point via -main-is"
