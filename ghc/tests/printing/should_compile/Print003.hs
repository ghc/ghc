module Word where

infixl 8 `bitLsh`, `bitRsh`

class Bits a where
	bitRsh, bitLsh :: a -> Int -> a
