-- !!! This is the example given in TcDeriv
--
module ShouldSucceed where

data T a b
  = C1 (Foo a) (Bar b) 
  | C2 Int (T b a) 
  | C3 (T a a)
  deriving Eq

data Foo a = MkFoo Double a deriving ()
instance (Eq a) => Eq (Foo a)

data Bar a = MkBar Int Int deriving ()
instance (Ping b) => Eq (Bar b)

class Ping a
