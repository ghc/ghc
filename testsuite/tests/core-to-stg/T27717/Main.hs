module Main where
import Mid
import Callee ()

main :: IO ()
main = print (g (1 :: Int))
