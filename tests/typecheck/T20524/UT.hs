{-#LANGUAGE UnboxedTuples#-}

module UT where

class Tupleable a where
  tup :: a -> (# a #)

newtype Boring a = Boring a
instance Tupleable (Boring a) where
  tup a = (# a #)

instance Tupleable () where
  tup _ = (# () #)


