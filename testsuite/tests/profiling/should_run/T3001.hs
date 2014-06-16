
-- This caused 6.10.1 to segfault when run with +RTS -hb
-- trac #3001

module Main (main) where

main :: IO ()
main = print $ replicate 40000 'x'
