{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module T15012a where

import GHC.Generics

type FakeOut a = Int

data family   TyFamily y z
data instance TyFamily a b = TyFamily Int (FakeOut b)
  deriving Generic1
