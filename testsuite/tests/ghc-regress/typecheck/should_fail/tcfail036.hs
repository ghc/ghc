-- !!! prelude class name in an instance-tycon position
--
module ShouldFail where

data NUM = ONE | TWO
instance Num NUM
  where ONE + ONE = TWO
instance Num NUM
instance Eq Num
--instance Text Num
