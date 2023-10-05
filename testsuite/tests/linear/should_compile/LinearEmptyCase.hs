{-# LANGUAGE EmptyCase, LinearTypes #-}

module LinearEmptyCase where

data Void

f :: a %1 -> Void -> b
f x y = case y of {}
