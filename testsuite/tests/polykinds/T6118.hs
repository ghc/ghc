{-# LANGUAGE PolyKinds, DataKinds, KindSignatures, RankNTypes,
             TypeFamilies, FlexibleInstances, UndecidableInstances #-}

module T6118 where

import GHC.Exts

data Nat = Zero | Succ Nat
data List a = Nil | Cons a (List a)

class SingE (a :: k) where
  type Demote a :: *

instance SingE (a :: Bool) where
  type Demote a = Bool
instance SingE (a :: Nat) where
  type Demote a = Nat

instance SingE (a :: Maybe k) where
  type Demote a = Maybe (Demote (Any :: k))

instance SingE (a :: List k) where
  type Demote (a :: List k) = List (Demote (Any :: k))