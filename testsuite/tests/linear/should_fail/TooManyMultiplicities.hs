{-# LANGUAGE LinearTypes #-}

module TooManyMultiplicities where

f :: a %1 %1 -> b
f = undefined
