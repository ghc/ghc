-- Tests kind error messages

-- GHC 6.6 stops on the first kind error it encounters,
-- so we only get one error report here.  I'll leave 
-- the other tests in here, though, in case matters improve
-- again

module ShouldFail where

data T k = T (k Int)

f :: [Maybe]
f x = x

g :: T Int
g x = x

h :: Int Int
h x = x
