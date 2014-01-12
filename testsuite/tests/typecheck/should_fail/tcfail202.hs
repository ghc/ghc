-- trac #2307
-- This was accepted due to a bug in GHC

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             OverlappingInstances, UndecidableInstances, IncoherentInstances,
             FlexibleInstances #-}

module Foo where

class C a b c | b -> c
instance C Bool Int Float
instance C Char Int Double

