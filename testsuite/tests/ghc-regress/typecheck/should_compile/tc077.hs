-- !!! make sure context of EQ is minimised in interface file.
--
module ShouldSucceed where

data NUM = ONE | TWO
class (Num a) => ORD a

class (ORD a, Show a) => EQ a where
	(===) :: a -> a -> Bool
