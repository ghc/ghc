-- !!! instances of functions
--
module ShouldFail where

data NUM = ONE | TWO

class EQ a where
	(===) :: a -> a -> Bool

class ORD a where
	(<<) :: a -> a -> Bool
	a << b = True

instance EQ (a -> b) where
	f === g = True

instance ORD (a -> b)

f = (<<) === (<<)
--f :: (EQ a,Num a) => a -> a -> Bool


{-
instance EQ NUM where
--	a /= b = False
	a === b = True
--	a /= b = False

-}
