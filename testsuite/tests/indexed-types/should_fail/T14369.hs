{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module T14369 where

data family Sing (a :: k)

data instance Sing (z :: Maybe a) where
  SNothing :: Sing Nothing
  SJust :: Sing x -> Sing (Just x)

class SingKind k where
  type Demote k = r | r -> k
  fromSing :: Sing (a :: k) -> Demote k

instance SingKind a => SingKind (Maybe a) where
  type Demote (Maybe a) = Maybe (Demote a)
  fromSing SNothing = Nothing
  fromSing (SJust x) = Just (fromSing x)

f :: forall (x :: forall a. Maybe a) a. SingKind a => Sing x -> Maybe (Demote a)
f = fromSing
