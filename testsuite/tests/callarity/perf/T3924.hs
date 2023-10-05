f2 :: Int -> Int -> Int
f2 x1 = if x1 == 0 then (\x0 -> x0) else let
     y = x1 - 1
     in f3 y y
f3 :: Int -> Int -> Int -> Int
f3 x2 = if x2 == 0 then f2 else let
     y = x2 - 1
     in f4 y y
f4 :: Int -> Int -> Int -> Int -> Int
f4 x3 = if x3 == 0 then f3 else let
     y = x3 - 1
     in \x2 x1 x0 -> f4 y x2 x1 (y + x0)
main = print (f2 100 0)
