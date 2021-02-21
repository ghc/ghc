{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts, ExistentialQuantification #-}
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


data instance GMap (Either a b) v = GMapEither (GMap a v) (GMap b v)
data instance forall a (b :: Proxy a). F (Proxy b) = FProxy Bool
data instance forall k (a :: k). F a = FOtherwise  -- accepted

data family D a b
data instance (Show b) => D Int b
data instance forall b . (Show b) => D Int b
data instance forall b . D Int b
