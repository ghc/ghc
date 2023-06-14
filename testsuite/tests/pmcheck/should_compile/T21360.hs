module T21360 where

data Foo = A {a :: Int} | B deriving Show

foo = A 4

-- wibble is safe - no warning
wibble = do
  case foo of
    bar@A{} -> Just bar{a = 9}
    _ -> fail ":("

-- using guards doesn't throw a warning
twomble | bar@A{} <- foo = Just bar{a = 9}
        | otherwise  = fail ":("

-- sworble has the same semantics as wibble and twomble - but we get a warning!
sworble = do
  bar@A{} <- Just foo
  Just bar{a = 9}
