-- Under -fno-specialise-incoherents, the call to m in f should not be
-- specialised, because there is another possible (though unused) instance.

{-# OPTIONS_GHC -fno-specialise-incoherents #-}
{-# LANGUAGE UndecidableInstances #-}
module T25833b (y) where

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
