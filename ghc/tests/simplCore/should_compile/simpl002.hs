-- !!! class/instance mumble that failed Lint at one time
--
module ShouldCompile where
class Foo a where
   op :: Int -> a -> Bool

data Wibble a b c = MkWibble a b c

instance (Foo a, Foo b, Foo c) => Foo (Wibble a b c) where
    op x y = error "xxx"
