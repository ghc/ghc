{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ExplicitForAll, PolyKinds #-}

module SAKS_009 where

import Data.Kind (Type)

type Q :: forall k -> k -> Type
data Q j (a :: j)
