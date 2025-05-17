{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module T18747A where

import Data.Kind
import Data.Type.Equality

type family Sing :: k -> Type
data SomeSing :: Type -> Type where
  SomeSing :: Sing (a :: k) -> SomeSing k

data SList :: forall a. [a] -> Type where
  SNil  :: SList '[]
  SCons :: Sing x -> Sing xs -> SList (x:xs)
type instance Sing @[_] = SList

data Univ = U1 | K1 Type | Sum Univ Univ | Product Univ Univ

data SUniv :: Univ -> Type where
  SU1      ::                     SUniv U1
  SK1      :: Sing c           -> SUniv (K1 c)
  SSum     :: Sing a -> Sing b -> SUniv (Sum a b)
  SProduct :: Sing a -> Sing b -> SUniv (Product a b)
type instance Sing @Univ = SUniv

data In :: Univ -> Type where
  MkU1      ::                 In U1
  MkK1      :: c            -> In (K1 c)
  L1        :: In a         -> In (Sum a b)
  R1        ::         In b -> In (Sum a b)
  MkProduct :: In a -> In b -> In (Product a b)

data SIn :: forall u. In u -> Type where
  SMkU1      ::                     SIn MkU1
  SMkK1      :: Sing c           -> SIn (MkK1 c)
  SL1        :: Sing a           -> SIn (L1 a)
  SR1        ::           Sing b -> SIn (R1 b)
  SMkProduct :: Sing a -> Sing b -> SIn (MkProduct a b)
type instance Sing @(In _) = SIn

class Generic (a :: Type) where
  type Rep a :: Univ
  from :: a -> In (Rep a)
  to   :: In (Rep a) -> a

class PGeneric (a :: Type) where
  type PFrom (x :: a)          :: In (Rep a)
  type PTo   (x :: In (Rep a)) :: a

class SGeneric k where
  sFrom :: forall (a :: k).          Sing a -> Sing (PFrom a)
  sTo   :: forall (a :: In (Rep k)). Sing a -> Sing (PTo a :: k)
  sTof  :: forall (a :: k).          Sing a -> PTo (PFrom a) :~: a
  sFot  :: forall (a :: In (Rep k)). Sing a -> PFrom (PTo a :: k) :~: a

instance Generic [a] where
  type Rep [a] = Sum U1 (Product (K1 a) (K1 [a]))
  from []     = L1 MkU1
  from (x:xs) = R1 (MkProduct (MkK1 x) (MkK1 xs))
  to (L1 MkU1)                           = []
  to (R1 (MkProduct (MkK1 x) (MkK1 xs))) = x:xs

instance PGeneric [a] where
  type PFrom '[]    = L1 MkU1
  type PFrom (x:xs) = R1 (MkProduct (MkK1 x) (MkK1 xs))
  type PTo (L1 MkU1)                           = '[]
  type PTo (R1 (MkProduct (MkK1 x) (MkK1 xs))) = x:xs

instance SGeneric [a] where
  sFrom SNil         = SL1 SMkU1
  sFrom (SCons x xs) = SR1 (SMkProduct (SMkK1 x) (SMkK1 xs))
  sTo (SL1 SMkU1)                             = SNil
  sTo (SR1 (SMkProduct (SMkK1 x) (SMkK1 xs))) = SCons x xs
  sTof SNil    = Refl
  sTof SCons{} = Refl
  sFot (SL1 SMkU1)                        = Refl
  sFot (SR1 (SMkProduct SMkK1{} SMkK1{})) = Refl
