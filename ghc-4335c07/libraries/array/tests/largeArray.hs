import Data.Array

main :: IO ()
main = print (((!1).inc.inc.inc.inc.inc.inc.inc.inc.inc.inc) a)

size :: Int
size = 60

a :: Array Int Integer
a = listArray (1,size) [1..]

inc :: Array Int Integer -> Array Int Integer
inc a = accum (+) a [(i,1) | i <- [1..size]]
