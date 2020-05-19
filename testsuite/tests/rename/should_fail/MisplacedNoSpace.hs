module MisplacedNoSpace where

instance Eq (T a)
{-#SPECIALISE instance Eq (T Int) #-}
 -- A mis-placed signature without spaces before the
 -- pragma string

data T a = T
