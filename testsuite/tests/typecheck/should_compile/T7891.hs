{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -O #-}    -- -O casused a Lint error in the simplifier, so I'm putting that in
                          -- all the time, so we don't miss it in a fast validate

module T7891 where

import Data.Kind (Type)

newtype T = T (forall t. t -> t)

tf :: T
tf = T id

-- Can't write this type signature:
f :: t -> t
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
  data F t :: Type
  mkF :: t -> F t

instance C () where
  data F () = FUnit (forall t. t -> t)
  mkF () = FUnit id

-- Can't write a type for f here either:
k :: t -> t
FUnit k = mkF ()
