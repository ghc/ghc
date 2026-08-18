module Main where
import Mid
import Callee ()

main :: IO ()
main = print (f (1 :: Int))
