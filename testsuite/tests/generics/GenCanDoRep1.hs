{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module CanDoRep1 where

import GHC.Generics (Generic, Generic1)


-- We should be able to generate a generic representation for these types
data A a
  deriving Generic1

data B a = B0 | B1
  deriving Generic1

data C a = C0 | C1 { c11 :: a, c12 :: (C a) }
  deriving (Generic, Generic1)

data (:*:) a b = a :*: b
  deriving (Generic, Generic1)
