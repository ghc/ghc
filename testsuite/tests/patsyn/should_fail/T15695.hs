{-# Language RankNTypes, PatternSynonyms, DataKinds, PolyKinds, GADTs,
             TypeOperators, MultiParamTypeClasses, TypeFamilies,
             TypeSynonymInstances, FlexibleInstances, InstanceSigs, FlexibleContexts #-}

{-# Options_GHC -fdefer-type-errors #-}

module T15695 where

import Data.Kind
import Data.Type.Equality

data TyVar :: Type -> Type -> Type where
 VO :: TyVar (a -> as) a
 VS :: TyVar as a -> TyVar (b -> as) a

data NP :: (k -> Type) -> ([k] -> Type) where
 Nil :: NP f '[]
 (:*) :: f a -> NP f as -> NP f (a:as)

data NS :: (k -> Type) -> ([k] -> Type) where
 Here  :: f a     -> NS f (a:as)
 There :: NS f as -> NS f (a:as)

infixr 6 :&:
data Ctx :: Type -> Type where
 E     :: Ctx(Type)
 (:&:) :: a -> Ctx(as) -> Ctx(a -> as)

data NA a

type SOP(kind::Type) code = NS (NP NA) code

type ApplyT :: forall (kind::Type) -> kind -> Ctx(kind) -> Type
data ApplyT k t ctx where
 AO :: a -> ApplyT(Type) a E
 AS :: ApplyT(ks)      (f a) ctx
    -> ApplyT(k -> ks) f     (a:&:ctx)

from' :: ApplyT(Type -> Type -> Type) Either ctx -> NS (NP NA) '[ '[VO] ]
from' (ASSO (Left  a)) = Here  (a :* Nil)
from' (ASSO (Right b)) = There (Here undefined)

pattern ASSO
  :: () =>
     forall (ks :: Type) k (f :: k -> ks) (a1 :: k) (ks1 :: Type) k1 (f1 :: k1 -> ks1) (a2 :: k1) a3.
     (kind ~ (k -> k1 -> Type), a ~~ f, b ~~ (a1 :&: a2 :&: E),
      f a1 ~~ f1, f1 a2 ~~ a3) =>
     a3 -> ApplyT kind a b
pattern ASSO a = AS (AS (AO a))
