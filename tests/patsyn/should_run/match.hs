-- Pattern synonyms

{-# LANGUAGE PatternSynonyms #-}
module Main where

pattern Single x y = [(x,y)]

foo []                   = 0
foo [(True, True)]       = 1
foo (Single True True)   = 2
foo (Single False False) = 3
foo _                    = 4

main = mapM_ (print . foo) tests
  where
    tests = [ [(True, True)]
            , []
            , [(True, False)]
            , [(False, False)]
            , repeat (True, True)
            ]
