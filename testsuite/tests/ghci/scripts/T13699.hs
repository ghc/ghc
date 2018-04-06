module T13699 where

data Foo = Foo
  { foo1 :: Int
  , foo2 :: !Int
  , foo3 :: Maybe Int
  , foo4 :: !(Maybe Int)
  }

data Bar = Bar Int !Int (Maybe Int) !(Maybe Int)
