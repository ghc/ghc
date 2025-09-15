{-# OPTIONS_GHC -fwarn-unused-pattern-binds #-}

-- #17

module Temp (foo, bar, quux) where

top :: Int
top = 1

foo :: ()
foo = let True = True in ()

bar :: Int -> Int
bar match = 1

quux :: Int
quux = let local = True
       in 2
