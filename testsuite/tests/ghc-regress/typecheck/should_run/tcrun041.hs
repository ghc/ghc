{-# LANGUAGE TupleSections #-}
module Main where

a :: Int -> (Int, Bool)
a = ( , True)

b :: Bool -> (Int, Bool)
b = (1, )

c :: a -> (a, Bool)
c = (, True || False)

d = (,1,)

main = print (a 1, b False, c "Hello", c 1337, d "Yeah" "Baby")