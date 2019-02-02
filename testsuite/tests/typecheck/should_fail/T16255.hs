{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module T16255 where

import Data.Kind
import GHC.TypeLits

data SBool :: Bool -> Type
type family F1 :: forall k. k -> Type where
  F1 @Bool = SBool

data family D1 :: forall k. k -> Type
-- Note that this similar-looking data family instance is OK, since data family
-- instances permit oversaturation in their equations.
data instance D1 @Bool :: Bool -> Type

type family F2 (x :: j) :: forall k. Either j k where
  F2 5 @Symbol = Left 4
