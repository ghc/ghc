{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, GADTs, MultiParamTypeClasses, KindSignatures,
             FunctionalDependencies, FlexibleInstances, UndecidableInstances, ExistentialQuantification #-}

module T6068 where

import Prelude hiding (Maybe, Nothing)
import Data.Kind (Type)

data Maybe :: Type -> Type where
  Nothing :: Maybe a

data family Sing (a :: k)

data instance Sing (a :: Maybe k) where
  SNothing :: Sing Nothing

data KProxy (a :: Type) = KProxy
data Existential (p :: KProxy k) =
  forall (a :: k). Exists (Sing a)

class HasSingleton a (kp :: KProxy k) | a -> kp where
  exists :: a -> Existential kp

class Floop a b | a -> b

instance Floop a (mp :: KProxy (Maybe ak)) => HasSingleton (Maybe a) mp where
  exists Nothing = Exists SNothing

-- instance forall (a ::Type) (mp :: KProxy (Maybe ak)).
--          HasSingleton (Maybe ak) (Maybe a) mp where
--   exists Nothing = Exists SNothing
