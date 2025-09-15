{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module T22326_typeRep where

import Type.Reflection
import Data.Kind
import GHC.TypeLits

-- Definition:
typeRepVis :: forall a -> Typeable a => TypeRep a
typeRepVis (type t) = typeRep @t

-- Usage:
tMaybe       = typeRepVis (type Maybe)
tMaybeString = typeRepVis (type (Maybe String))
tFunctor     = typeRepVis (type Functor)
tHello       = typeRepVis (type "Hello")
tTupFun      = typeRepVis (type ((Int,Bool,Char) -> Double))

-- Definition (using a lambda)
typeRepVis_lam :: forall a -> Typeable a => TypeRep a
typeRepVis_lam = \(type t) -> typeRep @t

-- Definition (with explicit kind quantification)
typeRepVis_kforall :: forall k. forall (a :: k) -> Typeable a => TypeRep a
typeRepVis_kforall = typeRepVis

-- Definition (with required kind parameter):
typeRepKiVis :: forall k (a :: k) -> Typeable a => TypeRep a
typeRepKiVis (type k) (type a) = typeRep @(a :: k)

-- Usage (with required kind parameter):
tMaybe'       = typeRepKiVis (type (Type -> Type)) (type Maybe)
tMaybeString' = typeRepKiVis (type Type) (type (Maybe String))
tFunctor'     = typeRepKiVis (type ((Type -> Type) -> Constraint)) (type Functor)
tHello'       = typeRepKiVis (type Symbol) (type "Hello")
tTupFun'      = typeRepKiVis (type Type) (type ((Int,Bool,Char) -> Double))