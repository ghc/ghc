{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds, PolyKinds, ExplicitForAll #-}

module SAKS_019 where

import Data.Kind (Type)

data P (a :: k)

type T :: forall a. P a -> Type
data T (y :: P (b :: j))
