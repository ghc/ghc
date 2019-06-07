add3 :: Int -> Int -> Int -> Int
add3 arg_x arg_y arg_z = arg_x + arg_y + arg_z

zipWitPartialInt :: (Int -> Int -> Int -> Int) -> [Int] -> [Int] -> [Int -> Int]
zipWitPartialInt f (x:xs) (y:ys) = f x y : zipWitPartialInt f xs ys
zipWitPartialInt _ _ _ = []

main :: IO ()
main =
  let list = [1..10] in
    print (map (\f -> f 1) (zipWitPartialInt add3 list list))
