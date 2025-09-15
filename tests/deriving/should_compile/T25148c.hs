{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
module T25148c where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

data Dict :: Constraint -> Type where
  Dict :: c => Dict c

class Facts a where
  fact1 :: forall n. Proxy a -> Dict (0 <= n)

newtype T a = MkT a
  deriving newtype Facts
