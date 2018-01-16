{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

class R where
  f :: Int -> Int
  g :: a -> a

instance R where
  f = (+1)
  g = id

main = print (g (f 0))
