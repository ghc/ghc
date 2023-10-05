{-# LANGUAGE StandaloneKindSignatures #-}

module VisFlag4 where

import Data.Kind

type C :: (forall k -> k -> k) -> Constraint
class C (hk :: forall k. k -> k) where
