{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module T16204c where

import Data.Kind

data family Sing :: forall k. k -> Type
type family Rep  :: Type

sTo :: forall (a :: Rep). Sing a
sTo = sTo

x :: forall (a :: Type). Sing a
x = id sTo
