{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts, FlexibleInstances #-}

module T2850 where

class K a where
  bar :: a -> a

class K (B a) => M a where
  data B a :: *
  foo :: B a -> B a

instance M Bool where
  data B Bool = B1Bool Bool | B2Bool Bool
  foo = id

instance K (B Bool) where
  bar = id

-- The 'deriving K' gives the (K (B Int)) instance
-- needed for the superclasses of M
instance M Int where
  newtype B Int = BInt (B Bool) deriving K
  foo = id
