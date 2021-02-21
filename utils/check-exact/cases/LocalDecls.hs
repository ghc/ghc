module LocalDecls where

foo a = bar a
  where
    bar :: Int -> Int
    bar x = x + 2

    baz = 4
