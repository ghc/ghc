{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module T16341 where

import Data.Data (Data)

data Foo a where
  X :: Foo Int
  Y :: (Bool -> Bool) -> Foo Bool

-- These instances should work whether or not `Y` is a constructor in
-- `Foo`, because the `Foo Int` designation precludes `Y` from being
-- a reachable constructor
deriving instance Show (Foo Int)
deriving instance Read (Foo Int)
deriving instance Eq (Foo Int)
deriving instance Ord (Foo Int)
deriving instance Data (Foo Int)
