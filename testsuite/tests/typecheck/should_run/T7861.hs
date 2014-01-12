{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module Main where

type A a = forall b. a

doA :: A a -> [a]
doA = undefined

f :: A a -> a
f = doA

main = do { print "Hello"; f `seq` print "Bad" }
