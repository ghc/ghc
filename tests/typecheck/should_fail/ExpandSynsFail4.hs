-- Synonyms shouldn't be expanded since type error is visible without
-- expansions. Error message should not have `Type synonyms expanded: ...` part.

module Main where

type T a = [a]

f :: T Int -> String
f = undefined

main = putStrLn $ f (undefined :: T Bool)
