{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module GenCanDoRep1_0 where

import GHC.Generics (Generic1(..), Rep1)


-- We should be able to generate a generic representation for these types
data B a
  deriving Generic1

data C a = C0 | C1
  deriving Generic1

data D a = D0 | D1 { d11 :: a, d12 :: (D a) }
  deriving Generic1

data (:*:) a b = a :*: b
  deriving Generic1

data E b a = E0 | E1 (E b b) deriving Generic1

data F c b a = F (c, b, a) deriving Generic1
data G c b a = G [(c, c, c, c, b, a)] deriving (Generic1, Show)


data Odd a = Odd a (Even a) deriving Generic1
data Even a = NilEven | Even a (Odd a) deriving Generic1

data Odd' a = Odd' a (Even' a) deriving Generic1
data Even' a = NilEven' | Even' a (Odd' a)
