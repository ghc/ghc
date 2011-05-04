{-# LANGUAGE Generics      #-}
{-# LANGUAGE TypeOperators #-}

module CanDoRep0 where

import GHC.Generics (Representable0)


-- We should be able to generate a generic representation for these types
data A  
  deriving Representable0

data B a
  deriving Representable0

data C = C0 | C1
  deriving Representable0

data D a = D0 | D1 { d11 :: a, d12 :: (D a) }
  deriving Representable0

data (:*:) a b = a :*: b
  deriving Representable0
