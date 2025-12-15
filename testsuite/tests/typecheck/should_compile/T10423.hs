{-# LANGUAGE TypeFamilies #-}

module T10423 where

class Monad m => Testable m a

newtype Prop m = MkProp (m Int)

instance (Monad m, m ~ n) => Testable n (Prop m)
