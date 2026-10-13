module Spec (main) where

import Lib (greeting)
import Lib.Internal (name)

main :: IO ()
main
  | greeting == "Hello, " ++ name = putStrLn "OK"
  | otherwise = error "unexpected greeting"
