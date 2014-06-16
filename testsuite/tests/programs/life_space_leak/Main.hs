--------------------------------
--	The Game of Life      --
--------------------------------

generations x = 30

data L a = N | C1 a (L a) | C2 a a (L a)

data Tuple2 a b = T2 a b

data Tuple3 a b c = T3 a b c


main = putStr (listChar_string
                    (append1 (C1 '\FF' N)
                             (life1 (generations ()) (start ()))))

listChar_string :: L Char -> String
listChar_string N = []
listChar_string (C1 x xs) = x : listChar_string xs

start :: a -> L (L Int)
start x = (C1 N
          (C1 N
          (C1 N
          (C1 N
          (C1 N
          (C1 N
          (C1 N
          (C1 N
          (C1 N
          (C1 N
          (C1 N
          (C1 N
          (C1 N
          (C1 N
          (C1
           (C1 0
           (C1 0
           (C1 0
           (C1 1
           (C1 1
           (C1 1
           (C1 1
           (C1 1
           (C1 0
           (C1 1
           (C1 1
           (C1 1
           (C1 1
           (C1 1
           (C1 0
           (C1 1
           (C1 1
           (C1 1
           (C1 1
           (C1 1
           (C1 0
           (C1 1
           (C1 1
           (C1 1
           (C1 1
           (C1 1
           (C1 0 N))))))))))))))))))))))))))) N)))))))))))))))

-- Calculating the next generation

gen1 :: Int -> L (L Int) -> L (L Int)
gen1 n board = map1 row1 (shift1 (copy1 n 0) board)

row1 :: Tuple3 (L Int) (L Int) (L Int) -> L Int
row1 (T3 last this next)
  = zipWith31 elt1 (shift2 0 last) 
                   (shift2 0 this) 
                   (shift2 0 next)


elt1 :: Tuple3 Int Int Int 
        -> (Tuple3 Int Int Int) 
        -> (Tuple3 Int Int Int) -> Int
elt1 (T3 a b c) (T3 d e f) (T3 g h i) 
 = if (not (eq tot 2))
          && (not (eq tot 3))
      then 0
      else if (eq tot 3) then 1 else e
   where tot = a `plus` b `plus` c `plus` d 
               `plus` f `plus` g `plus` h `plus` i

eq :: Int -> Int -> Bool
eq x y = x == y

plus :: Int -> Int -> Int
plus x y = x + y

shiftr1 :: L Int -> L (L Int) -> L (L Int)
shiftr1 x xs = append2 (C1 x N)  (init1 xs)

shiftl1 :: L Int -> L (L Int) -> L (L Int)
shiftl1 x xs = append2 (tail1 xs) (C1 x N)

shift1 :: L Int -> L (L Int) 
            -> L (Tuple3 (L Int) (L Int) (L Int))
shift1 x xs = zip31 (shiftr1 x xs) xs (shiftl1 x xs)

shiftr2 :: Int -> L Int -> L Int
shiftr2 x xs = append3 (C1 x N) (init2 xs)

shiftl2 :: Int -> L Int -> L Int
shiftl2 x xs = append3 (tail2 xs) (C1 x N)

shift2 :: Int -> L Int -> L (Tuple3 Int Int Int)
shift2 x xs = zip32 (shiftr2 x xs) xs (shiftl2 x xs)

-- copy

copy1 :: Int -> Int -> L Int
copy1 0 x = N
copy1 n x = C1 x (copy1 (n-1) x)

copy2 :: Int -> L Int -> L (L Int)
copy2 0 x = N
copy2 n x = C1 x (copy2 (n-1) x)

copy3 :: Int -> Char -> L Char
copy3 0 x = N
copy3 n x = C1 x (copy3 (n-1) x)

-- Displaying one generation

disp1 :: (Tuple2 (L Char) (L (L Int))) -> L Char
disp1 (T2 gen xss) 
 = append1 gen 
    (append1 (C1 '\n' (C1 '\n' N)) 
             (foldr_1 (glue1 (C1 '\n' N)) N
                       (map4 (compose2 concat1 (map2 star1)) xss)))

star1 :: Int -> L Char
star1 i = case i of
           0 -> C1 ' ' (C1 ' ' N)
           1 -> C1 ' ' (C1 'o' N)

glue1 :: L Char -> L Char -> L Char -> L Char 
glue1 s xs ys = append1 xs (append1 s ys)

-- Generating and displaying a sequence of generations

life1 :: Int -> L (L Int) -> L Char
life1 n xss 
  = foldr_1 (glue1 (copy3 (n+2) '\VT')) N
            (map5 disp1
              (zip1_ (map6 (string_ListChar.show) (ints 0))
                    gens))
    where
    gens = take3 (100 {-740-}::Int) (iterate1 (gen1 n) (initial1 n xss))

ints :: Int -> L Int
ints x = C1 x (ints (x+1))

string_ListChar :: String -> L Char
string_ListChar [] = N
string_ListChar (x:xs) = C1 x (string_ListChar xs)

initial1 :: Int -> L (L Int) -> L (L Int)
initial1 n xss = take1 n (append2 (map3 (compose1 (take2 n)
                           (`append3` (copy1 n 0))) xss)
                                (copy2 n (copy1 n 0)))

iterate1 :: (L (L Int) -> L (L Int)) 
               -> L (L Int) -> L (L (L Int))
iterate1 f x = C1 x (iterate1 f (f x))

-- versions of built in functions

-- take
take1 :: Int -> L (L Int) -> L (L Int)
take1 0 _ = N
take1 _ N = N
--should be:take1 (n+1) (C1 x xs) = C1 x (take1 n xs)
take1 n (C1 x xs) | n < 0     = error "Main.take1"
		  | otherwise = C1 x (take1 (n-1) xs)

take2 :: Int -> L Int -> L Int
take2 0 _ = N
take2 _ N = N
--should be:take2 (n+1) (C1 x xs) = C1 x (take2 n xs)
take2 n (C1 x xs) | n < 0     = error "Main.take2"
		  | otherwise = C1 x (take2 (n-1) xs)

take3 :: Int -> L (L (L Int))
             -> L (L (L Int))
take3 0 _ = N
take3 _ N = N
take3 n (C1 x xs) = C1 x (take3 (n-1) xs)

-- init

init1 :: L (L Int) -> L (L Int)
init1 (C1 x N) = N
init1 (C1 x xs) = C1 x (init1 xs)
init1 N = error "init1 got a bad list"

init2 :: L Int -> L Int
init2 (C1 x N) = N
init2 (C1 x xs) = C1 x (init2 xs)
init2 N = error "init1 got a bad list"

-- tail

tail1 :: L (L Int) -> L (L Int)
tail1 (C1 _ xs) = xs
tail1 N = error "tail1 got a bad list"

tail2 :: L Int -> L Int
tail2 (C1 _ xs) = xs
tail2 N = error "tail2 got a bad list"

-- maps

map1 :: (Tuple3 (L Int) (L Int) (L Int) -> L Int) -> 
                L (Tuple3 (L Int) (L Int) (L Int))
             -> L (L Int)
map1 f N = N
map1 f (C1 x xs) = C1 (f x) (map1 f xs)

map2 :: (Int -> L Char) -> L Int -> L (L Char)
map2 f N = N
map2 f (C1 x xs) = C1 (f x) (map2 f xs)

map3 :: (L Int -> L Int) -> L (L Int) -> L (L Int)
map3 f N = N
map3 f (C1 x xs) = C1 (f x) (map3 f xs)

map4 :: (L Int -> L Char)
         -> L (L Int) -> L (L Char)
map4 f N = N
map4 f (C1 x xs) = C1 (f x) (map4 f xs)

map5 :: (Tuple2 (L Char) (L (L Int)) -> L Char) 
          -> L (Tuple2 (L Char) (L (L Int)))
          -> L (L Char)
map5 f N = N
map5 f (C1 x xs) = C1 (f x) (map5 f xs)

map6 :: (Int -> L Char) -> L Int -> L (L Char)
map6 f N = N
map6 f (C1 x xs) = C1 (f x) (map6 f xs)

-- compose

compose2 :: (L (L Char) -> L Char) 
            -> (L Int -> L (L Char)) 
            -> L Int -> L Char
compose2 f g xs = f (g xs)

compose1 :: (L Int -> L Int) 
             -> (L Int -> L Int) -> L Int -> L Int
compose1 f g xs = f (g xs)

-- concat

concat1 :: L (L Char) -> L Char
concat1 = foldr_1 append1 N

-- foldr

foldr_1 :: (L Char -> L Char -> L Char) 
            -> L Char -> L (L Char) -> L Char
foldr_1 f a N = a
foldr_1 f a (C1 x xs) = f x (foldr_1 f a xs)

-- appends

append1 :: L Char -> L Char -> L Char
append1 N ys = ys
append1 (C1 x xs) ys = C1 x (append1 xs ys)

append2 :: L (L Int) -> L (L Int) -> L (L Int)
append2 N ys = ys
append2 (C1 x xs) ys = C1 x (append2 xs ys)

append3 :: L Int -> L Int -> L Int
append3 N ys = ys
append3 (C1 x xs) ys = C1 x (append3 xs ys)

-- zips

pzip f (C1 x1 xs) (C1 y1 ys)
 = C1 (f x1 y1) (pzip f xs ys)
pzip f _ _ = N


zip1_ :: L (L Char)
         -> L (L (L Int))
         -> L (Tuple2 (L Char) (L (L Int)))
zip1_ = pzip T2

zip2_ :: L (L Int)
         -> L (L Int)
         -> L (Tuple2 (L Int) (L Int))
zip2_ = pzip T2 

zip3d :: L Int -> (Tuple2 (L Int) (L Int)) 
            -> (Tuple3 (L Int) (L Int) (L Int))
zip3d x (T2 y z) = T3 x y z

zip3_ :: L (L Int) 
         -> L (Tuple2 (L Int) (L Int))
         -> L (Tuple3 (L Int) (L Int) (L Int))
zip3_ = pzip zip3d

zip4_ :: L Int
         -> L Int 
         -> L (Tuple2 Int Int)
zip4_ = pzip T2

zip5d :: Int -> (Tuple2 Int Int) -> (Tuple3 Int Int Int)
zip5d x (T2 y z) = T3 x y z

zip5_ :: L Int 
         -> L (Tuple2 Int Int)
         -> L (Tuple3 Int Int Int)
zip5_ = pzip zip5d

zip6_ :: L (Tuple3 Int Int Int)
         -> L (Tuple3 Int Int Int)
         -> L (Tuple2 (Tuple3 Int Int Int)
                      (Tuple3 Int Int Int))
zip6_ = pzip T2

zip31 :: L (L Int) -> L (L Int) 
         -> L (L Int)  
         -> L (Tuple3 (L Int) (L Int) (L Int))
zip31 as bs cs
  = zip3_ as (zip2_ bs cs)

zip32 :: L Int -> L Int -> L Int 
          -> L (Tuple3 Int Int Int)
zip32 as bs cs
  = zip5_ as (zip4_ bs cs)

-- zipWith

zipWith21 :: ((Tuple3 Int Int Int) 
              -> (Tuple2 (Tuple3 Int Int Int) 
                         (Tuple3 Int Int Int)) -> Int)
              -> L (Tuple3 Int Int Int) 
              -> L (Tuple2 (Tuple3 Int Int Int) 
                           (Tuple3 Int Int Int))
              -> L Int
zipWith21 = pzip 

zipWith31 :: ((Tuple3 Int Int Int) 
              -> (Tuple3 Int Int Int) 
              -> (Tuple3 Int Int Int) -> Int)
               -> L (Tuple3 Int Int Int)
               -> L (Tuple3 Int Int Int)
               -> L (Tuple3 Int Int Int) -> L Int
zipWith31 z as bs cs
 = zipWith21 z' as (zip6_ bs cs)
   where z' a (T2 b c) = z a b c
