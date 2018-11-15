{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module T13985 where

import Data.Kind
import Data.Proxy

data family Fam
data instance Fam = MkFam (forall (a :: k). Proxy a)

type family T
type instance T = Proxy (Nothing :: Maybe a)

class C k where
  data CD :: k
  type CT :: k

instance C Type where
  data CD = forall (a :: k). CD (Proxy a)
  type CT = Proxy (Nothing :: Maybe a)

class Z a where
  type ZT a
  type ZT a = Proxy (Nothing :: Maybe x)
