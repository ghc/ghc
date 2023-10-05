-- Kind error message

module ShouldFail where

data T k = T (k Int)

g :: T Int
g x = x
