{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module CanDoRep1 where

import GHC.Generics (Generic, Generic1)


-- We should be able to generate a generic representation for these types
data A a                                deriving Generic1

data B a = B0 | B1                      deriving Generic1

data C a = C0 | C1 { c11 :: a, c12 :: (C a) }
  deriving (Generic, Generic1)

data D a = D (Either Int a)             deriving Generic1

data E a = E (Either Int (E a))         deriving Generic1

data F c b a = F (c, b, a)              deriving Generic1
data G c b a = G [(c, c, c, c, b, a)]   deriving (Generic1, Show)


data Odd a = Odd a (Even a)             deriving Generic1
data Even a = NilEven | Even a (Odd a)  deriving Generic1

data Odd' a = Odd' a (Even' a)          deriving Generic1
data Even' a = NilEven' | Even' a (Odd' a)

data H b a = H0 | H1 (H b b)            deriving Generic1

data (:*:) a b = a :*: b                deriving (Generic, Generic1)
