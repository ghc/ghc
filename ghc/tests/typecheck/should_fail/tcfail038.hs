-- !!! duplicate class-method declarations

module ShouldFail where

data NUM = ONE | TWO
instance Eq NUM where
	a == b = True
	a /= b = False
	a == b = False
	a /= b = True

