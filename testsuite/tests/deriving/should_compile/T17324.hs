{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wunused-imports #-}
module T17324 where

import Data.Monoid (Sum(Sum), Product(Product), Dual(Dual))

class C1 a
deriving anyclass instance C1 (Sum a)

class C2 a
deriving anyclass instance C2 (Product a)

class C3 a
deriving via Dual a instance C3 (Dual a)
