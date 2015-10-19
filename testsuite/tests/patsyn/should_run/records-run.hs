{-# LANGUAGE PatternSynonyms #-}

module Main where

pattern Bi{a, b} = (a, b)

foo = ("a","b")

main = do
  print foo
  print (a foo)
  print (b foo)
  print (foo {a = "c"})
  print (foo {a = "fst", b = "snd"})
