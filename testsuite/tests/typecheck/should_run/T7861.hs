{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module Main where

type A a = forall b. a

doA :: A a -> [a]
doA = undefined

f :: A a -> a
f = doA

main = do { print "Hello 1"

          ; f `seq` print "Hello 2"
              -- The casts are pushed inside the lambda
              -- for f, so this seq succeds fine

          ; f (error "urk") `seq` print "Bad"
              -- But when we *call* f we get a type error
        }
