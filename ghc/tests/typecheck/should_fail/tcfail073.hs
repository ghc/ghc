-- what error do you get if you redefined Prelude instances?

module ShouldFail where

f x@(a,b) y@(c,d) = x == y

instance Eq (a,b) where
    (m,n) == (o,p) = m == o
