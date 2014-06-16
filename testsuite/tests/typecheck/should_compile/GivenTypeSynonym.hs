{-# LANGUAGE TypeFamilies #-}
module Main where 

data A a

type T a = A a


f :: (A a ~ T Int) => a -> Int 
f x = x 


main :: IO ()
main = return ()