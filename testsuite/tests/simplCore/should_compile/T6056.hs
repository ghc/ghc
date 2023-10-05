module T6056 where
import T6056a

foo :: Int -> (Maybe Int, [Int])
foo x = smallerAndRest x [x]

