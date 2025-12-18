{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module T26682a
  ( TK(..), TKR, TKS, TKX
  , Dict0(..)
  , randomValue2
  , lemTKScalarAllNumAD2
  ) where

import Prelude


import GHC.TypeLits ( KnownNat(..), Nat, SNat )
import Data.Kind ( Type, Constraint )
import Data.Typeable ( Typeable )
import Data.Proxy ( Proxy )

import Type.Reflection
import Data.Type.Equality

ifDifferentiable2 :: forall r a. Typeable r
                 => (Num r => a) -> a -> a
{-# INLINE ifDifferentiable2 #-}
ifDifferentiable2 ra _
  | Just Refl <- testEquality (typeRep @r) (typeRep @Double) = ra
ifDifferentiable2 ra _
  | Just Refl <- testEquality (typeRep @r) (typeRep @Float) = ra
ifDifferentiable2 _ a = a

data Dict0 c where
  Dict0 :: c => Dict0 c

type ShS2 :: [Nat] -> Type
data ShS2 ns where
  Z :: ShS2 '[]
  S :: {-# UNPACK #-} !( SNat n ) -> !( ShS2 ns ) -> ShS2 (n ': ns)

type KnownShS2 :: [Nat] -> Constraint
class KnownShS2 ns where
  knownShS2 :: ShS2 ns

instance KnownShS2 '[] where
  knownShS2 = Z
instance ( KnownNat n, KnownShS2 ns ) => KnownShS2 ( n ': ns ) where
  knownShS2 =
    case natSing @n of
      !i ->
        case knownShS2 @ns of
          !j ->
            S i j

type RandomValue2 :: Type -> Constraint
class RandomValue2 vals where
  randomValue2 :: Int -> Int


type IsDouble :: Type -> Constraint
type family IsDouble a where
  IsDouble Double = ( () :: Constraint )

class ( Typeable r, IsDouble r ) => NumScalar2 r
instance ( Typeable r, IsDouble r ) => NumScalar2 r

instance forall sh r target. (KnownShS2 sh, NumScalar2 r)
         => RandomValue2 (target (TKS sh r)) where
  randomValue2 g =
    ifDifferentiable2 @r
      ( case knownShS2 @sh of
          !_ -> g )
      g

instance (RandomValue2 (target a), RandomValue2 (target b))
         => RandomValue2 (target (TKProduct a b)) where
  randomValue2 g =
    let g1 = randomValue2 @(target a) g
        g2 = randomValue2 @(target b) g1
    in g2

lemTKScalarAllNumAD2 :: Proxy r -> Dict0 ( IsDouble r )
lemTKScalarAllNumAD2 _ = undefined
{-# OPAQUE lemTKScalarAllNumAD2 #-}


type data TK =
    TKScalar Type
  | TKR2 Nat TK
  | TKS2 [Nat] TK
  | TKX2 [Maybe Nat] TK
  | TKProduct TK TK

type TKR n r = TKR2 n (TKScalar r)
type TKS sh r = TKS2 sh (TKScalar r)
type TKX sh r = TKX2 sh (TKScalar r)
