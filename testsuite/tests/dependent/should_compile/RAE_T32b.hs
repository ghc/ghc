{-# LANGUAGE TemplateHaskell, TypeFamilies, GADTs, DataKinds, PolyKinds,
             RankNTypes, TypeOperators #-}

module RAE_T32b where

import Data.Kind

data family Sing (k :: Type) :: k -> Type

data TyArr (a :: Type) (b :: Type) :: Type
type family (a :: TyArr k1 k2 -> Type) @@ (b :: k1) :: k2
$(return [])

data Sigma (p :: Type) (r :: TyArr p Type -> Type) :: Type where
  Sigma :: forall (p :: Type) (r :: TyArr p Type -> Type)
                  (a :: p) (b :: r @@ a).
           Sing Type p -> Sing (TyArr p Type -> Type) r -> Sing p a ->
           Sing (r @@ a) b -> Sigma p r
$(return [])

data instance Sing (Sigma p r) (x :: Sigma p r) :: Type where
  SSigma :: forall (p :: Type) (r :: TyArr p Type -> Type)
                   (a :: p) (b :: r @@ a)
                   (sp :: Sing Type p) (sr :: Sing (TyArr p Type -> Type) r)
                   (sa :: Sing p a) (sb :: Sing (r @@ a) b).
            Sing (Sing (r @@ a) b) sb ->
            Sing (Sigma p r) ('Sigma sp sr sa sb)
