{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

-- This one sent an earlier version of GHC into a
-- loop in the simplfier, because we allowed a RULE
-- to fire on a loop-breaker
--
-- Discovered by Roman L, Nov 09

module Roman where

data T a = T (T a)

type family F a
type instance F (T a) = Wrap (T a)

class B (F a) => C a where
  toF :: a -> F a

go :: C a => a -> Int
{-# INLINE go #-}
go x = gow (toF x)

instance C a => C (T a) where
  {-# INLINE toF #-}
  toF x = Wrap x

newtype Wrap a = Wrap a

class B a where
  dummy :: a

  gow :: a -> Int

instance C a => B (Wrap a) where
  {-# INLINE gow #-}
  gow (Wrap x) = go x
  dummy = error "urk"

foo :: C a => T a -> Int
foo t = let t' = T t in go t'

