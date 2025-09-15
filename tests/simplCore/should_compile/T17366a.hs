module T17366a where
import Data.Functor.Identity

class C f where
  c :: f a -> a

instance C Identity where
  c (Identity a) = a

newtype Tagged tag a = Tagged a

instance C (Tagged tag) where
  c (Tagged a) = a

f :: C f => f a -> a
f a = c a
{-# INLINABLE[0] f #-}
