{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE TypeOperators       #-}

module T15322a where

import Data.Proxy (Proxy (..))
import Type.Reflection
import GHC.TypeLits (KnownNat, type (+))

f :: forall n . (KnownNat n) => Proxy n -> TypeRep (n+1)
f _ = typeRep
