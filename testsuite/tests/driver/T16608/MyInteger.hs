module MyInteger
  ( MyInteger (MyInteger)
  , ToMyInteger (toMyInteger)
  ) where

newtype MyInteger = MyInteger Integer

class ToMyInteger a where
  toMyInteger :: a -> MyInteger

instance ToMyInteger Integer where
  toMyInteger = MyInteger {- . succ -}
