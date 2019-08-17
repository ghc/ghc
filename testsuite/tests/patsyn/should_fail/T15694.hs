{-# Language RankNTypes, PatternSynonyms, TypeOperators, DataKinds, PolyKinds, KindSignatures, GADTs #-}

module T15694 where

import Data.Kind
import Data.Type.Equality

data Ctx :: Type -> Type where
 E     :: Ctx(Type)
 (:&:) :: a -> Ctx(as) -> Ctx(a -> as)

data ApplyT(kind::Type) :: kind ->  Ctx(kind) -> Type where
 AO :: a -> ApplyT(Type) a E
 AS :: ApplyT(ks)      (f a) ctx
    -> ApplyT(k -> ks) f     (a:&:ctx)


pattern ASSO :: () => forall ks k (f :: k -> ks) (a1 :: k) (ctx :: Ctx ks)
                             (ks1 :: Type) k1 (a2 :: k1) (ctx1 :: Ctx ks1) a3.
                     (kind ~ (k -> ks), a ~~ f, b ~~ (a1 :&: ctx),
                      ks ~ (k1 -> ks1), ctx ~~ (a2 :&: E),
                      ks1 ~ Type, f a1 a2 ~~ a3)
                   => a3 -> ApplyT kind a b

pattern ASSO a = AS (AS (AO a))
