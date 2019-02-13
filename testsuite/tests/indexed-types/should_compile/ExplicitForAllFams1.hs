{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}

module ExplicitForAllFams where

import Data.Proxy
import Data.Kind

-- From Proposal 0007

data family F a
data instance forall (x :: Bool). F (Proxy x) = MkF

class C a where
  type G a b
instance forall a. C [a] where
  type forall b. G [a] b = Int

type family H a b where
  forall x y. H [x] (Proxy y) = Double
  forall z.   H z   z         = Bool

-- More tests

type family D a b where
  forall (a :: Type -> Type) (b :: a Int) k (c :: k). D (Proxy b) (Proxy c) = ()
  forall (a :: Bool) (b :: Proxy a). D (Proxy b) () = Int
  forall (a :: Type). D a a = Maybe a
