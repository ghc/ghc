{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module T12088mm1_helper where

import Data.Kind

type family Closed (t :: Type) :: Type where
  Closed t = Open t

type family Open (t :: Type) :: Type

