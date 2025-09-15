{-# LANGUAGE PatternSynonyms #-}
module Main where

pattern First x <- x:_ where
  First x = [x]

main = mapM_ print $ First ()
