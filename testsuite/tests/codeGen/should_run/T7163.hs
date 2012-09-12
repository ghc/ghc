
module Main where

main :: IO ()
main = putStrLn $ printFloat 100

printFloat :: Float -> String
printFloat x = f (show (round (x * 10)))
    where f "0" = "0"
          f _   = show (round x)
