module Miranda(cjustify, lay, layn, limit, ljustify, merge, rep, rjustify, spaces,
	       {-force,seq,-}sort) where
--import UnsafeDirty
import QSort

cjustify :: Int -> String -> String
cjustify n s = spaces l ++ s ++ spaces r
               where
               m = n - length s
               l = m `div` 2
               r = m - l

{-
index :: [a] -> [Int]
index xs = f xs 0
		where f []     n = []
		      f (_:xs) n = n : f xs (n+1)
-}

lay :: [String] -> String
lay = concat . map (++"\n")

layn :: [String] -> String
layn =  concat . zipWith f [1..]
           where
	   f :: Int -> String -> String
           f n x = rjustify 4 (show n) ++ ") " ++ x ++ "\n"

limit :: (Eq a) => [a] -> a
limit (x:y:ys) | x == y    = x
               | otherwise = limit (y:ys)
limit _                    = error "Miranda.limit: bad use"

ljustify :: Int -> String -> String
ljustify n s = s ++ spaces (n - length s)

merge :: (Ord a) => [a] -> [a] -> [a]
merge []         ys                     = ys
merge xs         []                     = xs
merge xxs@(x:xs) yys@(y:ys) | x <= y    = x : merge xs  yys
		            | otherwise = y : merge xxs ys

rep :: Int -> b -> [b]
rep n x = take n (repeat x)

rjustify :: Int -> String -> String
rjustify n s = spaces (n - length s) ++ s

spaces :: Int -> String
spaces 0 = ""
spaces n = ' ' : spaces (n-1)

-------------

arctan x = atan x
code c = ord c
converse f a b = flip f a b
decode n = chr n
digit c = isDigit c
e :: (Floating a) => a
e = exp 1
entier x = floor x
filemode f = error "Miranda.filemode"
--getenv
hd xs = head xs
hugenum :: (Floating a) => a
hugenum = error "hugenum" --!!!
integer x = x == truncate x
letter c = isAlpha c
map2 f xs ys = zipWith f xs ys
--max
max2 x y = max x y
member xs x = x `elem` xs
--min
min2 x y = min x y
mkset xs = nub xs
neg x = negate x
numval :: (Num a) => String -> a
numval cs = read cs
postfix xs x = xs ++ [x]
--read
scan f z l = scanl f z l
--shownum !!!
--showfloat !!!
--showscaled !!!
tinynum :: (Floating a) => a
tinynum = error "tinynum"
undef = error "undefined"
zip2 xs ys = zip xs ys
--zip
