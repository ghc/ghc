{-# LANGUAGE GHC2021             #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE TypeOperators       #-}

module T15322 where

import Data.Proxy (Proxy (..))
import Type.Reflection
import GHC.TypeLits (KnownNat, type (+))

f :: forall n . (Typeable (n+1), KnownNat n) => Proxy n -> TypeRep (n+1)
f _ = typeRep
