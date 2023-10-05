{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module T15549a where

import Data.Proxy
import Data.Void

data family Sing (a :: k)
data instance Sing (z :: Void)

type family Rep a
class SGeneric a where
  sTo :: forall (r :: Rep a). Sing r -> Proxy a

type instance Rep Void = Void
instance SGeneric Void where
  sTo x = case x of
