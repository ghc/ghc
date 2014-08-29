module T6056 where
import T6056a

foo1 :: Int -> (Maybe Int, [Int])
foo1 x = smallerAndRest x [x]

foo2 :: Integer -> (Maybe Integer, [Integer])
foo2 x = smallerAndRest x [x]
