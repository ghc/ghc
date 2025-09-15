{-# LANGUAGE QuasiQuotes #-}
module Main where

main :: IO ()
main = print $ \parse -> [parse||]
