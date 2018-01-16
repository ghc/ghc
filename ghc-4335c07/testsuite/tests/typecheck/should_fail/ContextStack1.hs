{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}

module ContextStack1 where

class Cls a where meth :: a

instance Cls [a] => Cls a

t :: ()
t = meth

