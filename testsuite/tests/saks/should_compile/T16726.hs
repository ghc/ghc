{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module T16726 where

import Data.Kind

type D :: forall k. k -> Type
data D :: forall j. j -> Type

type DF :: forall k. k -> Type
data family DF :: forall j. j -> Type

type T :: forall k. k -> Type
type family T :: forall j. j -> Type
