module UnitT3911 where

import Text.PrettyPrint.HughesPJ

import TestUtils

xs :: [Doc]
xs = [text "hello",
      nest 10 (text "world")]

d1, d2, d3 :: Doc
d1 = vcat xs
d2 = foldr ($$) empty xs
d3 = foldr ($+$) empty xs

testT3911 :: IO ()
testT3911 = simpleMatch "T3911" expected out
  where out = show d1 ++ "\n" ++ show d2 ++ "\n" ++ show d3

expected :: String
expected =
  "hello     world\n\
hello     world\n\
hello\n\
          world"
