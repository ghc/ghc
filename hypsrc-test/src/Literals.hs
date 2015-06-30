module Literals where


str :: String
str = "str literal"

num :: Num a => a
num = 0 + 1 + 1010011 * 41231 + 12131

frac :: Fractional a => a
frac = 42.0000001

list :: [[[[a]]]]
list = [[], [[]], [[[]]]]

pair :: ((), ((), (), ()), ())
pair = ((), ((), (), ()), ())
