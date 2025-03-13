{-# LANGUAGE NamedDefaults #-}
module Main where

import A

-- This function contains an ambiguous type
-- Without a default, GHC will use its standard defaulting rules
-- With the named default (Bool), this will produce "Bool"
ambiguousValue :: Stringify a => a
ambiguousValue = undefined

ambiguousValue2 :: Bingify a => a
ambiguousValue2 = undefined

main :: IO ()
main = do
  -- Try to print the result of a function that would use the default
  putStrLn $ "Result(Stringify): " ++ stringify ambiguousValue
  putStrLn $ "Result(Bingify): " ++ bingify ambiguousValue2
