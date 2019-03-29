{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds, DataKinds, RankNTypes, TypeFamilies,
             TypeApplications, TypeOperators, GADTs #-}

module SAKS_030 where

import Data.Kind
import Data.Type.Equality

type T1 :: forall k (a :: k). Bool
type T2 :: k -> Bool

type family T1 where
  T1 @Bool @True  = False
  T1 @Bool @False = True

type family T2 a where
  T2 True  = False
  T2 False = True

type SBool :: Bool -> Type
data SBool b where
  STrue :: SBool True
  SFalse :: SBool False

proof_t1_eq_t2 :: SBool b -> T1 @Bool @b :~: T2 b
proof_t1_eq_t2 STrue  = Refl
proof_t1_eq_t2 SFalse = Refl
