module Main where

main :: IO ()
main = print $ sum $ map bitcount [0, 4 .. 2 ^ (24 :: Int) - 1]

bitcount :: Int -> Int
bitcount x =
  if x > 0
     then let (d, m) = divMod x 2 in bitcount d + m
     else 0
