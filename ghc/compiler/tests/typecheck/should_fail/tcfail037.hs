--!!! PreludeCore entities cannot be redefined at the top-level
--
module M where

data NUM = ONE | TWO

f a b = a + b
f :: NUM -> NUM -> NUM

ONE + ONE = TWO

