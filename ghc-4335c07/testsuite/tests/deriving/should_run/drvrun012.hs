-- Tests readings of record syntax

module Main where

data Foo = Foo { x :: Baz, y :: Maybe Int } deriving (Read,Show)

infix 0 :%%
data Baz = Int :%% Int deriving( Read,Show)


main = print (read "Foo { x = 1 :%% 2, y = Just 4 }" :: Foo)
