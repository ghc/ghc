module Main (main) where

import T14965_Sep

main :: IO ()
main = print $ cc bb

bb :: Sep
bb = catSep b1 b2

b1 :: Sep
b1 = Sep [] ["foo"] []

b2 :: Sep
b2 = Sep [] ["bar"] []
