{-# LANGUAGE LinearTypes #-}
module LinearLet8 where

-- Unbound multiplicity annotation
f :: a %1 -> a
f x = let %p y = x in y
