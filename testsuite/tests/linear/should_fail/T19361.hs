{-# LANGUAGE LinearTypes #-}

module T19361 where

f :: a %m -> a
f x = g x

g :: a -> a
g x = x
