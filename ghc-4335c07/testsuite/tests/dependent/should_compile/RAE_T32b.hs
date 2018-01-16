{-# LANGUAGE TemplateHaskell, TypeFamilies, GADTs, DataKinds, PolyKinds,
             RankNTypes, TypeOperators, TypeInType #-}

module RAE_T32b where

import Data.Kind

data family Sing (k :: *) :: k -> *

data TyArr (a :: *) (b :: *) :: *
type family (a :: TyArr k1 k2 -> *) @@ (b :: k1) :: k2
$(return [])

data Sigma (p :: *) (r :: TyArr p * -> *) :: * where
  Sigma :: forall (p :: *) (r :: TyArr p * -> *) (a :: p) (b :: r @@ a).
           Sing * p -> Sing (TyArr p * -> *) r -> Sing p a -> Sing (r @@ a) b -> Sigma p r
$(return [])

data instance Sing (Sigma p r) (x :: Sigma p r) :: * where
  SSigma :: forall (p :: *) (r :: TyArr p * -> *) (a :: p) (b :: r @@ a)
            (sp :: Sing * p) (sr :: Sing (TyArr p * -> *) r) (sa :: Sing p a) (sb :: Sing (r @@ a) b).
            Sing (Sing (r @@ a) b) sb ->
            Sing (Sigma p r) ('Sigma sp sr sa sb)
