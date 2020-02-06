{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds, ExplicitForAll #-}

module SAKS_Fail018 where

import Data.Kind (Type)

data P w

-- j = k, x = a
type T :: forall k. forall (a :: k) -> Type
data T (x :: j) = MkT (P k) (P j) (P x)    -- 'k' is not brought into scope by ScopedTypeVariables
