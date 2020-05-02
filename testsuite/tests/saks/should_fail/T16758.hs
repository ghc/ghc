{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExplicitForAll #-}

module T16758 where

import Data.Kind

type C :: forall (a :: Type) -> a ~ Int => Constraint
class C a where
  f :: C a => a -> Int
