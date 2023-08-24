{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE TypeAbstractions #-}

module T9151 where

import Data.Proxy

class PEnum (kproxy :: KProxy a) where
  type ToEnum (x :: a) :: Bool
  type ToEnum x = TEHelper

type TEHelper = ToEnum Int
