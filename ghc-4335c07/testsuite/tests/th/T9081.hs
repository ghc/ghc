{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds, TypeFamilies #-}

module T9081 where

import Data.Proxy

$( [d|
  class kproxy ~ 'KProxy => C (kproxy :: KProxy a) where
    type TF (x :: a) :: Bool
  |])
