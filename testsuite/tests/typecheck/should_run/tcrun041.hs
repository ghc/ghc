{-# LANGUAGE TupleSections, UnboxedTuples #-}
module Main where

a :: Int -> (Int, Bool)
a = ( , True)

b :: Bool -> (Int, Bool)
b = (1, )

c :: a -> (a, Bool)
c = (, True || False)

d = (,1,)


e = (# , True #)

f = (# 1, #)

g = (# , True || False #)

h = (# ,1, #)


unchanged :: a -> (# Int #)
unchanged _binding = (# 1 #)

main = do
    print (a 1, b False, c "Hello", c 1337, d "Yeah" "Baby")
    case e 1 of { (# x1, x2 #) ->
    case f False of { (# x3, x4 #) ->
    case g "Hello" of { (# x5, x6 #) ->
    case g 1337 of { (# x7, x8 #) ->
    case h "Yeah" "Baby" of { (# x9, x10, x11 #) ->
    print (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) }}}}}

