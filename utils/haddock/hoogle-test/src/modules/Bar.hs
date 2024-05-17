{-# LANGUAGE Haskell2010 #-}
module Bar where


import Foo


bar :: Int -> Int
bar x = foo' x x


bar' :: Int -> Int -> Int
bar' x y = foo' (bar (foo x)) (bar (foo y))
