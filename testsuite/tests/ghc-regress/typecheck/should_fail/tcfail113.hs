-- Tests kind error messages

module ShouldFail where

data T k = T (k Int)

f :: [Maybe]
f x = x

g :: T Int
g x = x

h :: Int Int
h x = x
