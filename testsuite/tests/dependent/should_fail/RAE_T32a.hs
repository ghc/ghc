{-# LANGUAGE TemplateHaskell, RankNTypes, TypeOperators, DataKinds,
             PolyKinds, TypeFamilies, GADTs #-}

module RAE_T32a where

import Data.Kind

data family Sing (k :: Type) :: k -> Type

data TyArr' (a :: Type) (b :: Type) :: Type
type TyArr (a :: Type) (b :: Type) = TyArr' a b -> Type
type family (a :: TyArr k1 k2) @@ (b :: k1) :: k2
data TyPi' (a :: Type) (b :: TyArr a Type) :: Type
type TyPi (a :: Type) (b :: TyArr a Type) = TyPi' a b -> Type
type family (a :: TyPi k1 k2) @@@ (b :: k1) :: k2 @@ b
$(return [])

data MkStar (p :: Type) (x :: TyArr' p Type)
type instance MkStar p @@ x = Type
$(return [])

data Sigma (p :: Type) (r :: TyPi p (MkStar p)) :: Type where
  Sigma ::
    forall (p :: Type) (r :: TyPi p (MkStar p)) (a :: p) (b :: r @@@ a).
    Sing Type p -> Sing (TyPi p (MkStar p)) r -> Sing p a ->
    Sing (r @@@ a) b -> Sigma p r
$(return [])

data instance Sing Sigma (Sigma p r) x where
  SSigma ::
    forall (p :: Type) (r :: TyPi p (MkStar p)) (a :: p) (b :: r @@@ a)
           (sp :: Sing Type p) (sr :: Sing (TyPi p (MkStar p)) r)
           (sa :: Sing p a) (sb :: Sing (r @@@ a) b).
    Sing (Sing (r @@@ a) b) sb ->
    Sing (Sigma p r) ('Sigma sp sr sa sb)

-- I (RAE) believe this last definition is ill-typed.
