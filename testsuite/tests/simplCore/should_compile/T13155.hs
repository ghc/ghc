{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -funfolding-use-threshold=10 #-}

module T13155 where

import GHC.Ptr
import GHC.Prim
import GHC.Exts

foo :: Ptr Float -> State# RealWorld -> (# State# RealWorld, Float #)
foo p s = case q :: Ptr Float of { Ptr a1 ->
          case readFloatOffAddr# a1 0# s of { (# s1, f1 #) ->
          case q :: Ptr Float of { Ptr a2 ->
          case readFloatOffAddr# a2 1# s of { (# s2, f2 #) ->
          (# s2, F# (plusFloat# f1 f2) #) }}}}
  where
    q :: Ptr a  -- Polymorphic
    q = p `plusPtr` 4
