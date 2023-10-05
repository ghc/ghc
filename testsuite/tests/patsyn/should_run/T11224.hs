{-# LANGUAGE  PatternSynonyms, ViewPatterns #-}

module Main where

import Text.Read

pattern PRead :: Read a => () => a -> String
pattern PRead a <- (readMaybe -> Just a)

foo :: String -> Int
foo (PRead x)  = (x::Int)
foo (PRead xs) = sum (xs::[Int])
foo _ = 666

bar :: String -> Int
bar (readMaybe -> Just x)  = (x::Int)
bar (readMaybe -> Just xs) = sum (xs::[Int])
bar _ = 666

main :: IO ()
main = do
  print $ foo "1"       -- 1
  print $ foo "[1,2,3]" -- 666 -- ???
  print $ foo "xxx"     -- 666

  print $ bar "1"       -- 1
  print $ bar "[1,2,3]" -- 6
  print $ bar "xxx"     -- 666

