-- By default -fspecialise-incoherents is in effect, so the call to m in f
-- should get specialised even though there is another potential instance.

{-# LANGUAGE UndecidableInstances #-}
module T25833 (y) where

class C a where
  m :: a -> a

instance {-# INCOHERENT #-} Num a => C a where
  m = (* 3)

instance C () where
  m = id

f :: Num a => a -> a
f = m

y :: Int
y = f 2
