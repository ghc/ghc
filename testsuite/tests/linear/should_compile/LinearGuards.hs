{-# LANGUAGE LinearTypes #-}
module LinearGuards where

f :: Bool -> a #-> a
f b a | b = a
      | True = a
