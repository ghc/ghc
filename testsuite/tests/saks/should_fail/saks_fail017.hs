{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds, TypeApplications #-}

module SAKS_Fail017 where

import Data.Kind (Type)

data T (a :: k)

type S :: forall k. k -> Type
type S = T @k   -- 'k' is not brought into scope by ScopedTypeVariables
