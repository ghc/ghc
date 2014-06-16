{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}

module CanDoRep0 where

import GHC.Generics (Generic)


-- We should be able to generate a generic representation for these types
data A                                          deriving Generic

data B a                                        deriving Generic

data C = C0 | C1                                deriving Generic

data D a = D0 | D1 { d11 :: a, d12 :: (D a) }   deriving Generic

data (:*:) a b = a :*: b                        deriving Generic

data family F a
data instance F Int = FInt                      deriving Generic
