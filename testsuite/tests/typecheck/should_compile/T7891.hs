{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module T7891 where

newtype T = T (forall t. t -> t)

tf :: T
tf = T id

-- Can't write this type signature:
-- f :: t -> t
T f = tf

-- But with an indirection we can:
g :: t -> t
g = f

-- We can still use f as it were fully polymorphic (which is good):
a :: ()
a = f ()
b :: Char
b = f 'b'

-------------

class C t where
  data F t :: *
  mkF :: t -> F t

instance C () where
  data F () = FUnit (forall t. t -> t)
  mkF () = FUnit id

-- Can't write a type for f here either:
k :: t -> t
FUnit k = mkF ()
