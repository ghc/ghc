--!!! bogus re-use of prelude class-method name (==)
--
module ShouldFail where

data NUM = ONE | TWO
class EQ a where
	(==) :: a -> a -> Bool

instance EQ NUM
--	a /= b = False
--	a == b = True
--	a /= b = False
