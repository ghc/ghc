{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module T13398a where

data Nat
data Rate
data StaticTicks where
        (:/:) :: Nat -> Rate -> StaticTicks
type ticks :/ rate = ticks ':/: rate

class HasStaticDuration (s :: k) where
  type SetStaticDuration s (pt :: StaticTicks) :: k

instance HasStaticDuration (t :/ r) where
  type SetStaticDuration (t :/ r) (t' :/ r') = t' :/ r'
