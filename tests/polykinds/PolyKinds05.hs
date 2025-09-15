{-# LANGUAGE PolyKinds                  #-}

module PolyKinds05 where

data A f
data B = B1 (A Maybe)

-- Should work. We have -XPolyKinds on, so `A` gets the polymorphic kind 
--   forall k. k -> *
