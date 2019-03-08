add3 :: Int -> Int -> Int -> Int
add3 arg_x arg_y arg_z = arg_x + arg_y + arg_z

zipWitPartial :: (a -> b -> c -> d) -> [a] -> [b] -> [c -> d]
zipWitPartial f (x:xs) (y:ys) = f x y : zipWitPartial f xs ys
zipWitPartial _ _ _ = []

main :: IO ()
main =
  let list = [1..10] in
    print (map (\f -> f 1) (zipWitPartial add3 list list))
