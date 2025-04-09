{-# LANGUAGE GADTs, PolyKinds, DataKinds, TypeFamilies #-}

module Foo where

import GHC.Exts
import Data.Kind

type family F a :: RuntimeRep
type instance F Int = LiftedRep

data family T a :: TYPE (F a)

data instance T Int where
  MkT :: Int -> T Int

-- ASSERT error in HEAD
