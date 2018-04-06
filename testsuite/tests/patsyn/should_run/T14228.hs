{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

type Maybe' t = (# t | () #)

pattern Just' :: a -> Maybe' a
pattern Just' x = (# x | #)

pattern Nothing' :: Maybe' a
pattern Nothing' = (# | () #)

foo x = case x of
  Nothing' -> putStrLn "nothing"
  Just' _ -> putStrLn "just"

main = do
  putStrLn "Nothing'"
  foo Nothing'

  putStrLn "Just'"
  foo (Just' "hello")
