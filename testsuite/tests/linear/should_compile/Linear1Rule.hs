{-# LANGUAGE LinearTypes #-}
module Linear1Rule where

-- Test the 1 <= p rule
f :: a %1 -> b
f = f

g :: a %p -> b
g x = f x
