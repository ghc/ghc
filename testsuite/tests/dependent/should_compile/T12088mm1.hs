{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module T12088mm1 where

import Data.Kind
import T12088mm1_helper

data family F (t :: Type) :: Closed t -> Type

data Q

type instance Open Q = Bool

data instance F Q r where
  F0 :: F Q 'True
