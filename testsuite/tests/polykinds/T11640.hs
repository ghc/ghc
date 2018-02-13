{-# LANGUAGE GADTs, RankNTypes, TypeInType #-}

module T11640 where

import Data.Kind

data HEq :: forall k1. k1 -> forall k2. k2 -> Type where
