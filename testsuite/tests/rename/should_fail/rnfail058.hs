{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Kind

data D :: forall. a -> Type
newtype D' :: forall. a -> Type

type family F :: forall. a -> Type
data family F' :: forall. a -> Type

type family G = (r :: forall. a -> Type)

data instance G :: forall. a -> Type
