-- what error do you get if you redefined Prelude instances?

module ShouldFail where

f :: (Eq a) => (a,a) -> (a,a) -> Bool
f x y = x == y

instance Eq a => Eq (a,b) where
    (m,n) == (o,p) = m == o

