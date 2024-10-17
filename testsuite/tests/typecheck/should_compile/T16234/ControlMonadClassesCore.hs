{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module ControlMonadClassesCore where

import Data.Kind (Type)
import DataPeano

type family CanDo (m :: Type -> Type) (eff :: k) :: Bool

type family MapCanDo (eff :: k) (stack :: Type -> Type) :: [Bool] where
  MapCanDo eff (t m) = CanDo (t m) eff ': MapCanDo eff m
  MapCanDo eff m = '[ CanDo m eff ]

type family FindTrue
  (bs :: [Bool])
  :: Peano
  where
  FindTrue ('True ': t) = 'Zero
  FindTrue ('False ': t) = 'Succ (FindTrue t)

type Find eff (m :: Type -> Type) =
  FindTrue (MapCanDo eff m)
