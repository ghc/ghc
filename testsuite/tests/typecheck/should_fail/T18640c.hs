{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module T18640c where

import Data.Kind

type F1 :: forall k -> Type
type family F1 k :: Type

type F2 :: forall x. forall k -> x
type F2 k = F1 k
