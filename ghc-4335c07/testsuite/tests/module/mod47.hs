-- !!! Instance context can't satisfy class-hierarchy constraint
module M where
class Foo a
class Foo a => Bar a
instance Num a => Foo [a]
instance (Eq a, Enum a) => Bar [a]

