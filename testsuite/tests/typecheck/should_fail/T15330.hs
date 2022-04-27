{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module T15330 where

import Data.Kind
import Data.Proxy

data T :: forall a. a -> Type

f1 :: Proxy (T True)
f1 = "foo"

f2 :: forall (t :: forall a. a -> Type).
      Proxy (t True)
f2 = "foo"
