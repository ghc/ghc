add2 :: Int -> Int -> Int
add2 arg_x arg_y = arg_x + arg_y

zipWit2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWit2 f (x:xs) (y:ys) = f x y : zipWit2 f xs ys
zipWit2 _ _ _ = []

main :: IO ()
main =
  let list = [1..10] in
    print (zipWit2 add2 list list)
