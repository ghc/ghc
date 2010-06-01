
module Main where

import Text.PrettyPrint.HughesPJ

xs :: [Doc]
xs = [text "hello",
      nest 10 (text "world")]

d1 :: Doc
d1 = vcat xs

d2 :: Doc
d2 = foldr ($$) empty xs

d3 :: Doc
d3 = foldr ($+$) empty xs

main :: IO ()
main = do print d1
          print d2
          print d3

