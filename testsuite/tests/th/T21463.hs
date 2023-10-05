{-# LANGUAGE TemplateHaskell #-}
module Main where

main :: IO ()
main = print
  $([| let f :: Int -> Int
           f x = x + 1
           {-# OPAQUE f #-}
       in f 41
     |])
