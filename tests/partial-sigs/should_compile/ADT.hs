{-# LANGUAGE PartialTypeSignatures #-}
module ADT where

data Foo x y z = Foo x y z

bar :: Int -> _ Int
bar x = Foo True () x
