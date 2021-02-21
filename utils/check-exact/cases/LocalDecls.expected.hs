module LocalDecls where

foo a = bar a
  where
    nn :: Int
    nn = 2

    bar :: Int -> Int
    bar x = x + 2

    baz = 4
