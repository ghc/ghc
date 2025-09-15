{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module T12088mm3_helper where

import Data.Kind

type family Open1 (t :: Type) :: Type

type family Open2 (t :: Type) (q :: Open1 t) :: Type

