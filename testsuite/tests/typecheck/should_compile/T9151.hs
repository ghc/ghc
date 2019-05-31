{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, UndecidableInstances,
             TopLevelKindSignatures #-}

module T9151 where

import Data.Kind
import Data.Proxy

type PEnum :: KProxy a -> Constraint
class PEnum (kproxy :: KProxy a) where
  type ToEnum (x :: a) :: Bool
  type ToEnum x = TEHelper

type TEHelper = ToEnum Int
