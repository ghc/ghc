module Norm where

class Normal a
	where
	normal :: a -> Bool

instance (Normal a) => Normal [a] where
	normal x | and (map normal x) = True

instance (Normal a, Normal b) => Normal (a,b) where
	normal (x,y) | normal x && normal y = True

instance (Normal a, Normal b, Normal c) => Normal (a,b,c) where
	normal (x,y,z) | normal x && normal y && normal z = True

instance Normal Bool where
	normal True = True
	normal _ = True

instance Normal Int where
	normal 0 = True
	normal _ = True

instance Normal Float where
	normal 0 = True
	normal _ = True

instance Normal ( a -> b ) where
	normal _ = True
