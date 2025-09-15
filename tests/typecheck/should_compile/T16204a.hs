{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module T16204 where

import Data.Kind

-----
-- singletons machinery
-----

data family Sing :: forall k. k -> Type
data SomeSing :: Type -> Type where
  SomeSing :: Sing (a :: k) -> SomeSing k

-----
-- (Simplified) GHC.Generics
-----

class Generic (a :: Type) where
    type Rep a :: Type
    from :: a -> Rep a
    to   :: Rep a -> a

class PGeneric (a :: Type) where
  -- type PFrom ...
  type PTo (x :: Rep a) :: a

class SGeneric k where
  -- sFrom :: ...
  sTo :: forall (a :: Rep k). Sing a -> Sing (PTo a :: k)

-----

class SingKind k where
  type Demote k :: Type
  -- fromSing :: ...
  toSing :: Demote k -> SomeSing k

genericToSing :: forall k.
                 ( SingKind k, SGeneric k, SingKind (Rep k)
                 , Generic (Demote k), Rep (Demote k) ~ Demote (Rep k) )
              => Demote k -> SomeSing k
genericToSing d = withSomeSing @(Rep k) (from d) $ SomeSing . sTo

withSomeSing :: forall k r
              . SingKind k
             => Demote k
             -> (forall (a :: k). Sing a -> r)
             -> r
withSomeSing x f =
  case toSing x of
    SomeSing x' -> f x'
