module DList where

newtype DList a = DList ([a] -> [a])

snoc :: DList a -> a -> DList a
DList f `snoc` x = DList (f . (x:))

toList :: DList a -> [a]
toList (DList f) = f []

instance Monoid (DList a) where
  mempty = DList id
  DList a `mappend` DList b = DList (a . b)
