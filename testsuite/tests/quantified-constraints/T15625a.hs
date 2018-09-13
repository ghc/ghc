{-# Language RankNTypes, ConstraintKinds, QuantifiedConstraints,
             PolyKinds, GADTs, MultiParamTypeClasses,
             DataKinds, FlexibleInstances #-}

module T15625a where

import Data.Kind

type Cat ob = ob -> ob -> Type

data KLEISLI (m :: Type -> Type) :: Cat (KL_kind m) where
  MkKLEISLI :: (a -> m b) -> KLEISLI(m) (KL a) (KL b)

data KL_kind (m :: Type -> Type) = KL Type

class    (a ~ KL xx) => AsKL a xx
instance (a ~ KL xx) => AsKL a xx

ekki__ :: Monad m => (forall xx. AsKL a xx) => KLEISLI m a a
ekki__ = MkKLEISLI undefined
