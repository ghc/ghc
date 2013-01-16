{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, GADTs, MultiParamTypeClasses,
             FunctionalDependencies, FlexibleInstances, UndecidableInstances, ExistentialQuantification #-}

module T6068 where

import Prelude hiding (Maybe, Nothing)

data Maybe :: * -> * where
  Nothing :: Maybe a

data family Sing (a :: k)

data instance Sing (a :: Maybe k) where
  SNothing :: Sing Nothing

data KProxy (a :: *) = KProxy
data Existential (p :: KProxy k) =
  forall (a :: k). Exists (Sing a)

class HasSingleton a (kp :: KProxy k) | a -> kp where
  exists :: a -> Existential kp

class Floop a b | a -> b

instance forall a (mp :: KProxy (Maybe ak)). Floop a mp => HasSingleton (Maybe a) mp where
  exists Nothing = Exists SNothing

-- instance forall (a ::*) (mp :: KProxy (Maybe ak)). HasSingleton (Maybe ak) (Maybe a) mp where
--   exists Nothing = Exists SNothing
