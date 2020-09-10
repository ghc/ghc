{-# LANGUAGE LinearTypes #-}
module LinearGuards where

f :: Bool -> a %1 -> a
f b a | b = a
      | True = a
