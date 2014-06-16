{-# LANGUAGE TemplateHaskell #-}

module T5404 where

foobar :: Int
foobar = $([|
      let
        bar :: Int
        bar = 5
      in bar
   |])

