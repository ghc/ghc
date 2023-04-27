module T23307a where

data List a = Nil | Cons {-# UNPACK #-} !(Unconsed a)
                    -- This UNPACK should work

data Unconsed a = Unconsed a !(List a)
data MUnconsed a = No | Yes {-# UNPACK #-} !(Unconsed a)