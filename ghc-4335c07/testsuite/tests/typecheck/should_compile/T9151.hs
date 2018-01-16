{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, UndecidableInstances #-}

module T9151 where

import Data.Proxy

class PEnum (kproxy :: KProxy a) where
  type ToEnum (x :: a) :: Bool
  type ToEnum x = TEHelper

type TEHelper = ToEnum Int
