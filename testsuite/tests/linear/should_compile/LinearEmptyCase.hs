{-# LANGUAGE EmptyCase, LinearTypes #-}

module LinearEmptyCase where

data Void

f :: a #-> Void -> b
f x y = case y of {}
