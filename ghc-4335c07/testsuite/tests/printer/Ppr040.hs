{-# LANGUAGE TemplateHaskell, RankNTypes, TypeOperators, DataKinds,
             PolyKinds, TypeFamilies, GADTs, TypeInType #-}

module RAE_T32a where

import Data.Kind

data family Sing (k :: *) :: k -> *

data TyArr' (a :: *) (b :: *) :: *
type TyArr (a :: *) (b :: *) = TyArr' a b -> *
type family (a :: TyArr k1 k2) @@ (b :: k1) :: k2
data TyPi' (a :: *) (b :: TyArr a *) :: *
type TyPi (a :: *) (b :: TyArr a *) = TyPi' a b -> *
type family (a :: TyPi k1 k2) @@@ (b :: k1) :: k2 @@ b
$(return [])

data MkStar (p :: *) (x :: TyArr' p *)
type instance MkStar p @@ x = *
$(return [])

type instance (MkStar p) @@ x = *
$(return [])

foo :: forall p x . MkStar p @@ x
foo = undefined

data Sigma (p :: *) (r :: TyPi p (MkStar p)) :: * where
  Sigma ::
    forall (p :: *) (r :: TyPi p (MkStar p)) (a :: p) (b :: r @@@ a).
    Sing * p -> Sing (TyPi p (MkStar p)) r -> Sing p a -> Sing (r @@@ a) b
        -> Sigma p r
$(return [])

data instance Sing Sigma (Sigma p r) x where
  SSigma ::
    forall (p :: *) (r :: TyPi p (MkStar p)) (a :: p) (b :: r @@@ a)
    (sp :: Sing * p) (sr :: Sing (TyPi p (MkStar p)) r) (sa :: Sing p a)
      (sb :: Sing (r @@@ a) b).
    Sing (Sing (r @@@ a) b) sb ->
    Sing (Sigma p r) ('Sigma sp sr sa sb)

-- I (RAE) believe this last definition is ill-typed.
