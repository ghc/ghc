{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module Main where

doA :: (forall b. a) -> [a]
doA = undefined

f :: (forall b. a) -> a
f = doA

main = do { print "Hello 1"

          ; f `seq` print "Hello 2"
              -- The casts are pushed inside the lambda
              -- for f, so this seq succeeds fine
              -- It does require ImpredicativeTypes, because we instantiate
              -- seq's type (c->d->d) with f's type (c:= (forall b. a) -> a),
              -- which is polymorphic (it has foralls).

          ; f (error "urk") `seq` print "Bad"
              -- But when we *call* f we get a type error
        }
