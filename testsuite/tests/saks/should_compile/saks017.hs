{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, ExplicitForAll #-}

module SAKS_017 where

import Data.Kind (Type)

type family F a where
  F Bool = Type
  F (f (a :: Type)) = F a

type family G a where
  G Int = Type

data family T :: F (Maybe Bool) -> t
data instance T (a :: G Int) = MkT a
