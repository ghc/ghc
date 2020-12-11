{-# LANGUAGE LinearTypes, GADTs, DataKinds, KindSignatures #-}

-- this caused a TcLevel assertion failure

module T18998 where

import GHC.Types
import GHC.TypeLits

data Id :: Nat -> Type -> Type where
  MkId :: a %1-> Id 0 a

f :: a %1-> Id n a -> Id n a
f a (MkId _) = MkId a
