{-# LANGUAGE Haskell2010 #-}
module FancyContextsWithoutExtension1 where

type Showish = Show

f :: (Showish a) => a -> a
f = undefined

