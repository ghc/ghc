{-# LANGUAGE PatternSynonyms #-}
module Main where

pattern First x <- x:_ where
  First x = foo [x, x, x]

foo :: [a] -> [a]
foo xs@(First x) = replicate (length xs + 1) x

main = mapM_ print $ First ()
