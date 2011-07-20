-- Tests grouping WITH a by clause but WITHOUT a using clause

{-# OPTIONS_GHC -XMonadComprehensions -XTransformListComp #-}

module Main where

import GHC.Exts(the)

main = putStrLn (show output)
  where
    output = [ (the dept, sum salary, name) 
             | (dept, salary, name) <- [("A", 1, "Bob"), ("B", 2, "Fred"), ("A", 5, "Jim"), ("A", 9, "Jim")]
             , then group by dept ]
