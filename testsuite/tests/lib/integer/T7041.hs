
module T7041 where

gcdInt :: Int -> Int -> Int
gcdInt a b = fromInteger (gcd (toInteger a) (toInteger b))

