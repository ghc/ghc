-- !!! tc082: an instance for functions
--
module ShouldSucceed where

class Normal a
	where
	normal :: a -> Bool

instance Normal ( a -> b ) where
	normal _ = True

f x = normal id
