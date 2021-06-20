{-# LANGUAGE LinearTypes, MultiWayIf #-}
module LinearMultiWay where

f :: Bool -> a %1-> a
f b x = if | b -> x
