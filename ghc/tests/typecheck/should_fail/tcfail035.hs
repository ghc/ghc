-- !!! instances with empty where parts: duplicate
--
module ShouldFail where

data NUM = ONE | TWO
instance Num NUM
instance Num NUM
instance Eq NUM
instance Show NUM
