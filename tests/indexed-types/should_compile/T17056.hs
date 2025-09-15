-- This test tripped Core Lint by producing a
-- case expression whose 'type' field included an
-- out of scope variable because of a phantom type
-- synonym

{-# OPTIONS_GHC -O #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Bug where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(..))

type IsInt a = Int :~: a

data Sing :: forall (b :: Type). IsInt b -> Type where
  SRefl :: Sing Refl

data SomeSing :: Type -> Type where
  SomeSing :: Sing (x :: IsInt b) -> SomeSing b

type WhyCastWith    (e :: IsInt b) = b
-- Type /synonym/
--   WhyCastWith b (e :: IsInt b) = b

type family   Apply (e :: IsInt b) :: Type
type instance Apply (e :: IsInt b) = WhyCastWith e

-- axiom   Apply (b :: *) (e :: IsInt b) ~ WhyCastWith e

(~>:~:) :: forall (b :: Type) (r :: IsInt b).
           Sing r
        -> Apply r
(~>:~:) SRefl = let w = w in w

castWith :: forall b. IsInt b ->  b
castWith eq
  = case (case eq of Refl -> SomeSing SRefl) :: SomeSing b of
      SomeSing SRefl -> (~>:~:) @b SRefl
