{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}

module Main where

--------------------------------------------------------------------------------
-- Simple generic programming (instant-generics-like library)
--------------------------------------------------------------------------------
data U a = UNIT | SUM (U a) (U a) | PRODUCT (U a) (U a) | REC a

-- GADT interpretation
data I :: U * -> * where
  Unit          :: I UNIT
  Inl           :: I a -> I (SUM a b)
  Inr           :: I b -> I (SUM a b)
  Product       :: I a -> I b -> I (PRODUCT a b)
  Rec           :: a -> I (REC a)

-- Class embedding types and their generic representation
class Generic (a :: *) where
  type Rep a :: U *
  from :: a -> I (Rep a)
  to   :: I (Rep a) -> a

-- Generic size on representations
class GSize (a :: U *) where
  gsize :: I a -> Int

instance GSize UNIT where
  gsize Unit = 0

instance (GSize a, GSize b) => GSize (SUM a b) where
  gsize (Inl x) = gsize x
  gsize (Inr x) = gsize x

instance (GSize a, GSize b) => GSize (PRODUCT a b) where
  gsize (Product a b) = gsize a + gsize b

instance (Size a) => GSize (REC a) where
  gsize (Rec x) = 1 + size x

-- Size on datatypes
class Size (a :: *) where
  size :: a -> Int
  default size :: (Generic a, GSize (Rep a)) => a -> Int
  size = gsize . from

instance (Size a) => Size [a] -- default
instance Size Char where size _ = 1 -- adhoc

-- Example encoding: lists
instance Generic [a] where
  type Rep [a] = SUM UNIT (PRODUCT (REC a) (REC [a]))
  from []    = Inl Unit
  from (h:t) = Inr (Product (Rec h) (Rec t))
  to (Inl Unit)                      = []
  to (Inr (Product (Rec h) (Rec t))) = h:t

-- Testing size
test1 = size "abc"

main = print test1
