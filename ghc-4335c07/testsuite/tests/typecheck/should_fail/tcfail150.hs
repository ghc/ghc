module ShouldFail where

-- !!! constraining the type variable in a class head is illegal
-- Simpler version of tcfail149
class Foo a where
  op :: Eq a => a -> a
