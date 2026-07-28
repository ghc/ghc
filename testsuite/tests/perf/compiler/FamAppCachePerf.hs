{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module FamAppCachePerf where

import Data.Kind
import GHC.TypeNats

type Id :: Type -> Type
type family Id a where
  Id a = a

type F :: Type -> Type
type family F a where
  F Int = Int

type G :: Type
type G = F ( Id Int )

type K :: Type -> Type -> Type
type family K a b where
  K
    ( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
    , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
    , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
    , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
    )
    b = b

type Loop :: Nat -> Type
type family Loop n where
  Loop 0 = Int
  Loop n =
    K
      ( G, G, G, G, G, G, G, G, G, G, G, G, G, G, G, G
      , G, G, G, G, G, G, G, G, G, G, G, G, G, G, G, G
      , G, G, G, G, G, G, G, G, G, G, G, G, G, G, G, G
      , G, G, G, G, G, G, G, G, G, G, G, G, G, G, G, G
      )
      ( Loop ( n - 1 ) )

foo :: Loop 3000 -> Int
foo x = x
