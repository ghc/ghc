-- !!! Legitimate re-use of prelude class-method name (==)
-- Used not to be legal, but a late H98 change made it legal
--
module ShouldFail where

data NUM = ONE | TWO
class EQ a where
	(==) :: a -> a -> Bool

instance EQ NUM where
	a == b = True
