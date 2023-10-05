module Main(main) where

scoreWeek = scanr (:) [] $ repeat True

main = seq scoreWeek (putStrLn "good")

