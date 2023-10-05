{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies, TypeOperators #-}

module T20974 where

import Data.Kind

class A (m :: Type -> Type)
class B (m :: Type -> Type)

type family F cs (m :: Type -> Type) :: Constraint where
  F '[]      m = ()
  F (c : cs) m = (c m, F cs m)

test :: F [Monad, A, B] m => m ()
test = pure ()
