add3 :: Int -> Int -> Int -> Int
add3 arg_x arg_y arg_z = arg_x + arg_y + arg_z

zipWit3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWit3 f (x:xs) (y:ys) (z:zs) = f x y z : zipWit3 f xs ys zs
zipWit3 _ _ _ _ = []

main :: IO ()
main =
  let list = [1..10] in
    print (zipWit3 add3 list list list)
