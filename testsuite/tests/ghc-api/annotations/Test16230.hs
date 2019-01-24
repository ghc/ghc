{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module MoreExplicitForalls where

import Data.Proxy

data family F1 a
data instance forall (x :: Bool). F1 (Proxy x) = MkF

class C a where
  type F2 a b

instance forall a. C [a] where
  type forall b. F2 [a] b = Int


type family G a b where
  forall x y. G [x] (Proxy y) = Double
  forall z.   G z   z         = Bool
