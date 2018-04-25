{-# LANGUAGE QuasiQuotes #-}
module Main where

parse = undefined

main :: IO ()
main = case () of
         [parse||] -> return ()
         _ -> return ()
