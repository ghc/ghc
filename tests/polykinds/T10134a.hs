{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module T10134a where

import Data.Kind (Type)
import GHC.TypeLits

data Vec :: Nat -> Type -> Type where
  Nil  :: Vec 0 a
  (:>) :: a -> Vec n a -> Vec (n + 1) a
