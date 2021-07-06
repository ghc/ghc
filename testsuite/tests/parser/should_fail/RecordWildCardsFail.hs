
module RecordWildCardsFail where

data Foo = Foo { a :: Int }

foo :: Foo -> Int
foo Foo{..} = a
