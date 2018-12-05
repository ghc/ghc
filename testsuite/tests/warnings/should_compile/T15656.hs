module T15656 where

myHead :: [a] -> a
myHead = \(a:_) -> a

data Foo
  = Bar { barInt :: Int, barString :: String }
  | Baz

mySetter :: Int -> Foo -> Foo
mySetter int foo = foo { barInt = int }
