module ShouldFail where

-- !!! ambiguous constraint in the context of an instance declaration
class Bar a
instance Bar a => Bar Bool

-- !!! constraining the type variable in a class head is illegal
class Foo a where
  op :: Eq a => a -> a
