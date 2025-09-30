module Main where
import GHC.InfoProv

main :: IO ()
main = print =<< whereFrom main
