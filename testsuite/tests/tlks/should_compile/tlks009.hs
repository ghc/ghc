{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE ExplicitForAll, PolyKinds #-}

module TLKS_009 where

import Data.Kind (Type)

type Q :: forall k -> k -> Type
data Q j (a :: j)
