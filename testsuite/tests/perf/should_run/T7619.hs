{-# LANGUAGE TypeFamilies #-}
module Main where
data family Foo a

data instance Foo Int = FooInt Int Int

foo :: Foo Int -> Int
foo (FooInt a 0) = 0
foo (FooInt a b) = foo (FooInt a (b-1))

main :: IO ()
main = foo (FooInt 0 10000) `seq` return ()
