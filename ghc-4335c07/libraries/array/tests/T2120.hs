
module Main (main) where

import Control.Exception
import Data.Array.IArray

a :: Array Int Int
a = listArray (1,4) [1..4]

b :: Array (Int,Int) Int
b = listArray ((0,0), (3,3)) (repeat 0)

main :: IO ()
main = do print (a ! 5) `catch` \e -> print (e :: SomeException)
          print (b ! (0,5)) `catch` \e -> print (e :: SomeException)

