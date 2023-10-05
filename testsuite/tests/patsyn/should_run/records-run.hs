{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

pattern Bi{a, b} = (a, b)

foo = ("a","b")

pattern ReadP :: Read a => a -> String
pattern ReadP {readp} <- (read -> readp)

main = do
  print foo
  print (a foo)
  print (b foo)
  print (foo {a = "c"})
  print (foo {a = "fst", b = "snd"})

  print (readp @Int "5")
