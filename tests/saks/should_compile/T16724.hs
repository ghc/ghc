{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module T16724 where

import Data.Kind

type T1 :: forall k (a :: k). Type
type family T1

-- type T2 :: forall {k} (a :: k). Type
type T2 :: forall a. Type
type family T2
