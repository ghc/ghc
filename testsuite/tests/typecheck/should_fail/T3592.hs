{-# LANGUAGE RankNTypes #-}

module T3592 where

type T a = Show a => a

f :: T a -> String
f = show

g :: T a -> String
g x = show x

