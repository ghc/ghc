{-# LANGUAGE TupleSections #-}
module Main where

a :: Bool -> (Int, Bool)
a = ( , True)

b :: Int -> Bool -> (Int, Bool)
b = (1, )

c :: a -> (a, Bool)
c = (True || False, )

main = return ()