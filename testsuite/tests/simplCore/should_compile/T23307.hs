module T23307 where

data Stream a = Nil | Cons a !(Stream a)
data Unconsed a = Unconsed a !(Stream a)
data MUnconsed a = No | Yes {-# UNPACK #-} !(Unconsed a)
