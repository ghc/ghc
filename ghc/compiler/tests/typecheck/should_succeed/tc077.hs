--!!! make sure context of EQ is minimised in interface file.
--
module M where

data NUM = ONE | TWO
class (Num a) => ORD a

class (ORD a, Text a) => EQ a where
	(===) :: a -> a -> Bool
