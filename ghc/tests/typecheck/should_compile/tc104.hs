-- !!! Checking that Main.main's type can now be of the form (IO a)
module Main(main) where

main = putStrLn "Hello" >> return (id)
