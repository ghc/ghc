{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module T12088mm2_helper where

import Data.Kind

type family Open (t :: Type) :: Type

data family F (t :: Type) :: Open t -> Type

