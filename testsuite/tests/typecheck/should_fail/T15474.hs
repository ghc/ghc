{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module T15474 where

import Data.Kind (Type)

data Proxy a

type Forall = forall t. Proxy t

f1 :: forall (t :: Type). Proxy t
f1 = f1

f2 :: Forall
f2 = f1
