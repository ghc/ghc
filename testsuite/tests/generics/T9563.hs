{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

module T9563 where

import Data.Kind (Type)
import GHC.Generics

data family F typ :: Type -> Type
data A
data instance F A a = AData a
  deriving (Generic, Generic1)

data family G a b c d
data instance G Int b Float d = H deriving Generic

deriving instance Generic1 (G Int b Float)
