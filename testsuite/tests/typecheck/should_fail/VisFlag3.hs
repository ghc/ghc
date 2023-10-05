{-# LANGUAGE TypeFamilies #-}

module VisFlag3 where

class C (hk :: forall k. k -> k) where
  type F (hk :: forall k -> k -> k)
