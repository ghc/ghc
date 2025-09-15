{-# LANGUAGE LinearTypes, MultiWayIf #-}
module T20023 where

f :: Bool -> a %1-> a
f b x = if | b -> x
