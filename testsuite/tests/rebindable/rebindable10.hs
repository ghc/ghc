{-# LANGUAGE RebindableSyntax #-}
module Main where
import Prelude

ifThenElse :: Int -> String -> String -> String
ifThenElse a b c = case a > 0 of
    True -> b
    False -> c

main :: IO ()
main = do
     print $ if -5 then "this fails" else "this works"
     print $ if 5 then "this works" else "this fails"