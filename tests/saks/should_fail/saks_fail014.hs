{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies, PolyKinds, ExplicitForAll #-}

module SAKS_Fail014 where

import Data.Kind (Type)

type T :: forall k. k
type family T :: forall j. j where
  T = Maybe
  T = Integer
