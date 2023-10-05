{-# LANGUAGE MultiParamTypeClasses #-}

module Foo where

class Wob a b where
   to :: a -> b
   from :: b -> a

foo x = [x, to (from x)]
-- Ambiguous type:  Wob a0 b => b -> [b]
-- Should be rejected
