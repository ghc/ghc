
module Main (main) where

main :: IO ()
main = do print $ f True
          print $ g 3
          print $ h 'a'

f :: a -> a
f x = x

g :: Int -> Int
g x = x

h :: Char -> Char
Just h = Just id
