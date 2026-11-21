{-# LANGUAGE Haskell2010, NoExperimentalDependentTypes #-}
module Main where

import RetainedConstructor (emitted, lazy, withNatural, withRefinement)
import RetainedTuple (observe)
import TupleProvider (Tuple(Tup2))

main :: IO ()
main = print (snd (emitted ()), snd (withNatural ()),
              snd (observe (Tup2 False True)),
              lazy (error "constructor field was demanded"),
              snd (withRefinement ()))
