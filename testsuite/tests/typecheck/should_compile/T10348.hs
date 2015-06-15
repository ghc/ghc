{-# LANGUAGE AutoDeriveTypeable, GADTs, DataKinds, KindSignatures, StandaloneDeriving #-}

module T10348 where

import GHC.TypeLits
import Data.Typeable

data Foo (n :: Nat) where
  Hey :: KnownNat n => Foo n

deriving instance Show (Foo n)

data T t where
  T :: (Show t, Typeable t) => t -> T t

deriving instance Show (T n)

hey :: (Typeable n, KnownNat n) => T (Foo n)
-- SHOULD BE: hey :: KnownNat n => T (Foo n)
hey = T Hey
