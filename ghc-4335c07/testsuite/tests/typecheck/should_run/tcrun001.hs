-- !! Test for (->) instances

module Main where

class Flob k where
  twice :: k a a  -> k a a

instance Flob (->) where
  twice f = f . f

inc :: Int -> Int
inc x = x+1

main = print (twice inc 2)


